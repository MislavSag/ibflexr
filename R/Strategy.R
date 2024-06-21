#' @title Strategy
#'
#' @description
#' Strategy class is used to extract and clean data from the flex reports.
#'
#' @details
#' The Strategy class is used to extract and clean data from the flex reports.
#'
#' @param flex_reports_xml list of XML flex reports.
#'
#' @return A Strategy object
Strategy = R6::R6Class(
  "Strategy",

  public = list(

    #' @field flex_reports_xml list of XML flex reports
    flex_reports_xml = NULL,

    #' @description
    #' Initialize the Strategy object
    #'
    #' @param flex_reports_xml list of XML flex reports
    #'
    #' @return The Strategy object
    initialize = function(flex_reports_xml) {
      # DEBUG
      # self = list()
      # self$flex_reports_xml = flex_reports_xml

      self$flex_reports_xml = flex_reports_xml
    },

    #' @description
    #' Extract node from the XML report. The function clean data so that it can
    #' be easly used for further analysis.
    #'
    #' @param xml_reports list of XML flex reports.
    #' @param node The node to be extracted.
    #'
    #' @return The extracted node in data.table format
    extract_node = function(node) {
      # node = "Trade"
      # Extract node
      xml_l = lapply(self$flex_reports_xml, function(x) {
        # x = self$flex_reports_xml[[1]]
        xml_ = x |>
          xml_find_all(paste0(".//", node)) |>
          xml_attrs()
        xml_ = lapply(xml_, function(x) {
          transpose(as.data.table(x = x, keep.rownames = TRUE),
                    make.names = TRUE)
        })
        rbindlist(xml_)
      })
      xml_extracted = rbindlist(xml_l, fill = TRUE)

      # Type conversion
      xml_extracted = xml_extracted[, lapply(.SD, type.convert, as.is = TRUE)]
      time_columns = colnames(xml_extracted)[grepl("time", colnames(xml_extracted), ignore.case = TRUE)]
      date_columns = colnames(xml_extracted)[grepl("date", colnames(xml_extracted), ignore.case = TRUE)]
      date_columns = setdiff(date_columns, time_columns)
      xml_extracted[, (date_columns) := lapply(.SD, as.Date, format = "%Y-%m-%d"), .SDcols = date_columns]
      xml_extracted[, (time_columns) := lapply(.SD, as.POSIXct, format = "%Y-%m-%d;%H:%M:%S"), .SDcols = time_columns]

      # Cleaning
      xml_extracted = xml_extracted[, .SD, .SDcols = !colSums(is.na(xml_extracted)) == nrow(xml_extracted)]
      xml_extracted = xml_extracted[, .SD, .SDcols = colSums(xml_extracted != 0) > 0]

      # Keep only unique rows
      xml_extracted = unique(xml_extracted)

      return(xml_extracted)
    },

    #' @description
    #' Summarize CFD trades
    #'
    #' @return The summarized CFD trades
    summary_cfd_trades = function() {
      trades = self$extract_node("Trade")
      trades = trades[, .(
        q = sum(quantity),
        p = weighted.mean(tradePrice, quantity)), by = dateTime]
      setorder(trades, dateTime)
      return(trades)
    },

    #' @description
    #' Calculate NAV units
    #'
    #' @param benchmark_symbol The benchmark symbol
    #'
    #' @return The NAV units
    calculate_nav_units = function(benchmark_symbol = NULL) {
      # Get transfers
      transfers = self$extract_node("Transfer")
      transfers = transfers[, .(date, cashTransfer)]
      setnames(transfers, c("timestamp", "NAV"))

      # Get NAV values
      equity_curve = strategy$extract_node("EquitySummaryByReportDateInBase")
      equity_curve = equity_curve[, .(timestamp = reportDate, NAV = total)]
      equity_curve = equity_curve[NAV > 0]
      setorder(equity_curve, "timestamp")

      # see this issue: https://github.com/enricoschumann/PMwR/issues/1#issuecomment-1533207687
      if (!is.null(transfers) && nrow(transfers) > 2) {
        transfers[3:nrow(transfers), timestamp := timestamp - 1]
        equity_curve[timestamp %in% transfers[3:nrow(transfers), timestamp],
                     NAV := NAV + transfers[3:nrow(transfers), NAV]]
      }

      # calculate NAV units using PMwr package
      nav_units = unit_prices(
        as.data.frame(equity_curve),
        cashflow = as.data.frame(transfers),
        initial.price = 100
      )
      setDT(nav_units)

      # Add benchmark
      if (!is.null(benchmark_symbol)) {
        # benchmark_symbol = "SPY"
        benchmark_yahoo = Ticker$new(benchmark_symbol)
        benchmark = benchmark_yahoo$get_history(start = nav_units[, min(timestamp)-5],
                                                end = Sys.Date(),
                                                interval = '1d')
        setDT(benchmark)
        benchmark[, date := as.Date(date)]
        benchmark[, adj_close := as.numeric(adj_close)]
        benchmark[, close_unit := adj_close / shift(adj_close) - 1]
        nav_units = benchmark[nav_units, on = c("date" = "timestamp")]
      }

      # Set names as Strategy and Benchmark
      setnames(nav_units,
               c("close_unit", "price"),
               c("Benchmark", "Strategy"))

      return(nav_units[, .(date, Strategy, Benchmark)])
    }
  )
)

# DEBUG
# SNP_TOKEN = '114675794008076852662086' # '112391365954681463992598'
# CGS_TOKEN = '357033856026608409139901'
# CGS_ED_TOKEN = '66472513822268724154000'
# CGS_TOKEN_PRA = '22092566548262639113984' # "201490993548010999912988"
# CGS_TOKEN_EXUBER_INVERSE = "661783759275938846524009"
#
# flex = Flex$new(token ='22092566548262639113984', query = '803831')
# p = flex$send()
# g = flex$get(newtoken = p)
# report = flex$get_flex_report()
#
# FLEX_PRA = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/pra_2023.xml"
# )
# flex_report_2023 = read_xml(FLEX_PRA)
# flex = Flex$new(token ='22092566548262639113984', query = '803831')
# report = flex$get_flex_report()
# flex_reports_xml = list(flex_report_2023, report)
#
# strategy = Strategy$new(flex_reports_xml)
# self = strategy$clone()
# trades = strategy$extract_node("Trade")
# trades = strategy$summary_cfd_trades()
# equity_summary_in_base = strategy$extract_node("EquitySummaryByReportDateInBase")
# equity_summary_in_base[, .(timestamp = reportDate,
#                            NAV = as.numeric(total))]
# transfers = strategy$extract_node("Transfer")
#
# nav_units = strategy$calculate_nav_units(benchmark_symbol = "SPY")
#
