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
#' @export
Strategy = R6::R6Class(
  "Strategy",

  public = list(

    #' @field flex_reports_xml list of XML flex reports
    flex_reports_xml = NULL,

    #' @field start_date The start date of the strategy
    start_date = NULL,

    #' @field end_date The end date of the strategy
    end_date = NULL,

    #' @description
    #' Initialize the Strategy object
    #'
    #' @param flex_reports_xml list of XML flex reports
    #' @param start_date The start date of the strategy
    #' @param end_date The end date of the strategy
    #'
    #' @return The Strategy object
    initialize = function(flex_reports_xml, start_date = NULL, end_date = NULL) {
      # DEBUG
      # self = list()
      # self$flex_reports_xml = flex_reports_xml

      # Set calendar
      qlcal::setCalendar("UnitedStates/NYSE")

      # Test if there is APIKEY environ
      test = Sys.getenv("APIKEY")
      if (test == "") {
        stop("FMP APIKEY is not set. Please set the APIKEY environment variable.")
      }

      self$flex_reports_xml = flex_reports_xml
      self$start_date = start_date
      self$end_date = end_date
    },

    #' @description
    #' Extract node from the XML report. The function clean data so that it can
    #' be easly used for further analysis.
    #'
    #' @param node The node to be extracted
    #' @param filter_date Filter date after start_date for column tradeDate if tradeDate column exist in data.table
    #'
    #' @return The extracted node in data.table format
    extract_node = function(node, filter_date = TRUE) {
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

      # Cleaning
      xml_extracted = xml_extracted[, .SD, .SDcols = !colSums(is.na(xml_extracted)) == nrow(xml_extracted)]
      xml_extracted = xml_extracted[, .SD, .SDcols = colSums(xml_extracted != 0, na.rm = TRUE) > 0]
      xml_extracted = xml_extracted[, .SD, .SDcols = (!colSums(xml_extracted == "", na.rm = TRUE) == nrow(xml_extracted))]
      xml_extracted = unique(xml_extracted)

      # Type conversion
      xml_extracted = xml_extracted[, lapply(.SD, type.convert, as.is = TRUE)]
      date_columns = colnames(xml_extracted)[grepl("date", colnames(xml_extracted), ignore.case = TRUE)]
      time_columns = colnames(xml_extracted)[grepl("time", colnames(xml_extracted), ignore.case = TRUE)]
      date_columns = setdiff(date_columns, time_columns)
      # xml_extracted[, (date_columns) := lapply(.SD, as.Date, format = "%Y-%m-%d"), .SDcols = date_columns] # OLD WAY
      xml_extracted[, ..date_columns]
      xml_extracted[, (date_columns) := lapply(.SD, function(x) {
        x = as.character(x)
        x = anydate(x)
      }), .SDcols = date_columns]
      xml_extracted[, (time_columns) := lapply(.SD, as.POSIXct, format = "%Y-%m-%d;%H:%M:%S"), .SDcols = time_columns]

      # Filter date after start_date for column tradeDate if tradeDate column exist in data.table
      if (filter_date == TRUE) {
        for (date in date_columns) {
          xml_extracted = private$filter_date(xml_extracted, date)
        }
      }

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
        p = weighted.mean(tradePrice, quantity)), by = tradeDate]
      setorder(trades, tradeDate)
      return(trades)
    },

    #' @description
    #' Calculate NAV units
    #'
    #' @param benchmark_symbol The benchmark symbol
    #' @param start_date The start date
    #' @param end_date The end date
    #' @param unit The unit
    #'
    #' @return The NAV units
    calculate_nav_units = function(benchmark_symbol = NULL,
                                   start_date = self$start_date,
                                   end_date = self$end_date,
                                   unit = NULL) {
      # Debug
      # x$FlexQueryResponse$FlexStatements$FlexStatement$
      # self$extract_node("TierInterestDetail", FALSE)
      # self$extract_node("CashTransaction", FALSE)
      # self$extract_node("CashReportCurrency", FALSE)
      # self$extract_node("PriorPeriodPosition", FALSE)
      # self$extract_node("Transfer", FALSE)
      # self$extract_node("EquitySummaryByReportDateInBase", FALSE)
      # self$extract_node("UnbundledCommissionDetail", FALSE)
      # self$extract_node("ChangeInDividendAccrual", FALSE)
      # self$extract_node("SecurityInfo", FALSE)
      # self$extract_node("MTDYTDPerformanceSummaryUnderlying", FALSE)
      # self$extract_node("ChangeInNAV", FALSE)
      # self$extract_node("AccountInformation", FALSE)
      # self$extract_node("UnsettledTransfers", FALSE)
      # self$extract_node("ChangeInPositionValues", FALSE)
      # self$extract_node("PriorPeriodPosition", FALSE)
      # self$extract_node("OpenPosition", FALSE)
      # self$extract_node("NetStockPositionSummary", FALSE)

      # Get transfers
      transfers = self$extract_node("Transfer", FALSE)
      transfers = transfers[, .(date, cashTransfer)]
      transfers = transfers[, .(cashTransfer = sum(cashTransfer)), by = date]
      transfers = transfers[cashTransfer != 0]
      setnames(transfers, c("timestamp", "NAV"))

      # Get NAV values
      equity_curve = self$extract_node("EquitySummaryByReportDateInBase", FALSE)
      equity_curve = equity_curve[, .(timestamp = reportDate, NAV = total)]
      equity_curve = equity_curve[NAV > 0]
      equity_curve = unique(equity_curve, by = "timestamp", fromLast = TRUE)
      setorder(equity_curve, "timestamp")

      # see this issue: https://github.com/enricoschumann/PMwR/issues/1#issuecomment-1533207687
      if (!is.null(transfers) & nrow(transfers) > 2) {
        transfers[3:nrow(transfers), timestamp := as.Date(
          vapply(timestamp,
                 FUN = function(x) qlcal::advanceDate(x, -1L),
                 FUN.VALUE = double(1L))
        )]
        equity_curve[timestamp %in% transfers[3:nrow(transfers), timestamp],
                     NAV := NAV + transfers[3:nrow(transfers), NAV]]
      }

      # Remove CFD costs and / or interests
      cfd_charges = self$extract_node("CFDCharge", FALSE)
      interests = self$extract_node("TierInterestDetail", FALSE)
      interests = interests[, .(totalInterest = sum(totalInterest, na.rm = TRUE)), by = valueDate]
      equity_curve_gross = cfd_charges[equity_curve, on = c("date" = "timestamp")]
      equity_curve_gross = interests[equity_curve_gross, on = c("valueDate" = "date")]
      equity_curve_gross[, cum_cfd_cost := cumsum(nafill(total, fill = 0))]
      equity_curve_gross[, cum_interests := cumsum(nafill(totalInterest, fill = 0))]
      equity_curve_gross[, NAV := NAV - cum_cfd_cost - cum_interests]
      equity_curve_gross = equity_curve_gross[, .(timestamp = valueDate, NAV)]

      # Set start_date if it is not provided
      if (is.null(start_date)) {
        start_date = equity_curve[, min(timestamp)]
      }
      if (is.null(end_date)) {
        end_date = equity_curve[, max(timestamp)]
      }

      # Filter by dates
      nav_units = private$get_unit_prices(
        equity_curve,
        transfers,
        start_date = start_date,
        end_date = end_date)
      nav_units_gross = private$get_unit_prices(
        equity_curve_gross,
        transfers,
        start_date = start_date,
        end_date = end_date)

      # Scale if unit is provided
      if (!is.null(unit)) {
        # unit = 2
        private$calculate_unleveraged_NAV(nav_units, unit)
        private$calculate_unleveraged_NAV(nav_units_gross, unit)
      }

      # Add benchmark
      if (!is.null(benchmark_symbol)) {
        # benchmark_symbol = "SPY"
        benchmark_yahoo = Ticker$new(benchmark_symbol)
        benchmark = benchmark_yahoo$get_history(start = nav_units[, min(timestamp)],
                                                end = NULL,
                                                interval = '1d')
        setDT(benchmark)
        benchmark[, date := as.Date(date)]
        benchmark[, adj_close := as.numeric(adj_close)]
        nav_units = benchmark[nav_units, on = c("date" = "timestamp")]
        nav_units[, close_unit := adj_close / data.table::first(adj_close) * 100]

        # # benchmark_symbol = "SPY"
        # url = "https://financialmodelingprep.com/stable/historical-price-eod/dividend-adjusted"
        # print(as.character(nav_units[, min(timestamp)]))
        # p = GET(
        #   url,
        #   query = list(
        #     symbol = benchmark_symbol,
        #     from = as.character(nav_units[, min(timestamp)]),
        #     apikey = Sys.getenv("APIKEY")
        #   ),
        # )
        # # p = GET(
        # #   "https://financialmodelingprep.com/stable/historical-price-eod/dividend-adjusted",
        # #   query = list(
        # #     symbol = "SPY",
        # #     from = "2022-11-21",
        # #     apikey = Sys.getenv("APIKEY")
        # #   ),
        # # )
        # res = content(p)
        # res = lapply(res, as.data.table)
        # benchmark = rbindlist(res, fill = TRUE)
        # setDT(benchmark)
        # benchmark[, date := as.Date(date)]
        # benchmark[, adj_close := as.numeric(adjClose)]
        # setorder(benchmark, date)
        # print(benchmark)
        # nav_units = benchmark[nav_units, on = c("date" = "timestamp")]
        # nav_units = na.omit(nav_units, cols = c("adj_close"))
        # nav_units[, close_unit := (adj_close / data.table::first(adj_close)) * 100]
      }

      # Set names as Strategy and Benchmark
      nav_units_merged = merge(nav_units[, .(date, Strategy = price, Benchmark = close_unit)],
                               nav_units_gross[, .(date = timestamp, StrategyGross = price)],
                               by = "date", all = TRUE)
      nav_units_merged = na.omit(nav_units_merged, cols = c("Strategy", "Benchmark"))
      nav_units_merged = unique(nav_units_merged, by = c("date", "Strategy", "Benchmark"))
      return(nav_units_merged)
    }
  ),
  private = list(
    filter_date = function(dt, date_) {
      # dt = copy(xml_extracted)
      # date = "reportDate"
      if (date_ %in% colnames(dt)) {
        if (!is.null(self$start_date) & !is.null(self$end_date)) {
          dt = dt[get(date_) %between% c(self$start_date, self$end_date)]
        } else if (!is.null(self$start_date) & is.null(self$end_date)) {
          dt = dt[get(date_) >= self$start_date]
        } else if (is.null(self$start_date) & !is.null(self$end_date)) {
          dt = dt[get(date_) <= self$end_date]
        }
      }
      return(dt)
    },
    calculate_unleveraged_NAV = function(dt, unit = 2) {
      # Calculate daily returns from NAV values
      dt[, daily_return := c(NA, diff(price) / head(price, -1))]

      # Adjust the returns to reflect what they would be without leverage
      dt[, unleveraged_return := daily_return / unit]

      # Initial NAV value
      initial_NAV = dt$price[1]

      # Reconstruct the unleveraged NAV series
      dt[, price := initial_NAV * cumprod(1 + ifelse(is.na(unleveraged_return), 0, unleveraged_return))]

      # Remove unnecessary columns
      dt[, c("daily_return", "unleveraged_return") := NULL]

      # Return the modified data.table
      return(dt)
    },
    get_unit_prices = function(equity_curve, transfers, start_date, end_date) {
      dt_ = equity_curve[timestamp %between% c(start_date, end_date)]
      # If there are no transfers between start date and end date, we don't need
      # to adjust for transfers. WE just need to scale
      transfer_test = transfers[timestamp %between% dt_[, .(min(timestamp), max(timestamp))]]
      if (nrow(transfer_test) == 0) {
        nav_units = scale1(as.xts.data.table(dt_), level = 100)
        nav_units = as.data.table(xts::as.xts(nav_units))
        setnames(nav_units, c("timestamp", "price"))
      } else {
        cf = transfers[, .(timestamp, NAV = as.numeric(NAV))]
        cf[timestamp < start_date, timestamp := dt_[, min(timestamp)]]
        cf = cf[, .(NAV = sum(NAV)), by = timestamp]
        if (cf[1, timestamp] == dt_[1, timestamp]) {
          cf[1, NAV := as.numeric(dt_[1, NAV])]
        }
        nav_units = unit_prices(
          as.data.frame(dt_),
          cashflow = as.data.frame(cf),
          initial.price = 100
        )
        nav_units = as.data.table(nav_units)
      }
    }
  )
)

# DEBUG
# library(R6)
# library(httr)
# library(rvest)
# library(xml2)
# library(data.table)
# library(PMwR)
# library(yahoofinancer)
# library(anytime)
# FLEX_PRA = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/pra_2023.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/pra_old_account.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/pra.xml"
# )
# FLEX_MINMAX = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax_2022.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax_2023.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax_old_account.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax_2023.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax.xml"
# )
# FLEX_EXUBER = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/exuberbondsml_2023.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/exuberv1.xml"
# )
# FLEX_RISKCOMBO = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/riskcombo.xml"
# )
# # PRA
# pra_start = as.Date("2023-04-25")
# strategy = Strategy$new(lapply(FLEX_PRA[[2]], read_xml), start_date = as.Date("2024-07-01"))
# strategy = Strategy$new(lapply(FLEX_PRA, read_xml), start_date = pra_start)
# self = strategy$clone()
# MINMAX
# strategy = Strategy$new(lapply(FLEX_MINMAX, read_xml), start_date = as.Date("2022-11-01"))
# self = strategy$clone()
# strategy$calculate_nav_units("SPY")
# # EXUBER
# strategy = Strategy$new(lapply(FLEX_EXUBER, read_xml), start_date = pra_start)
# self = strategy$clone()
# strategy$calculate_nav_units("SPY")
# # RISKCOMBO
# strategy = Strategy$new(lapply(FLEX_RISKCOMBO, read_xml), start_date = as.Date("2025-05-21"))
# self = strategy$clone()
# strategy$calculate_nav_units("SPY")
# Try other imports
# strategy$extract_node("CashReportCurrency", FALSE)[]
# strategy$extract_node("EquitySummaryByReportDateInBase")[]
# strategy$extract_node("Trade")[]
# strategy$extract_node("CFDCharge")[]
# strategy$extract_node("PriorPeriodPosition")[]
# strategy$extract_node("OpenPosition")[]
# strategy$summary_cfd_trades()[]

# flex_report_2023 = read_xml(FLEX_PRA[1])
# flex_report_2024 = read_xml(FLEX_PRA[2])
# report = read_xml(FLEX_PRA[3])
# flex = Flex$new(token ='537279325090849431098493', query = '1056812')
# report = flex$get_flex_report()
# flex_reports_xml = list(flex_report_2022, flex_report_2023, report)
# flex_reports_xml = list(flex_report_2022, flex_report_2023)
#
# strategy = Strategy$new(flex_reports_xml, start_date = as.Date("2024-01-01"))
# self = strategy$clone()

# strategy = Strategy$new(flex_reports_xml, as.Date("2023-04-25")) # PRA
# self = strategy$clone()
# strategy = Strategy$new(flex_reports_xml, as.Date("2024-01-01"), end_date = as.Date("2024-03-01")) # PRA
#
# strategy$calculate_nav_units("SPY", unit = NULL)
#
# find node
# flex_reports_xml = lapply(FLEX_MINMAX, read_xml)
# x = as_list(flex_reports_xml[[2]])
# x = x$FlexQueryResponse$FlexStatements$FlexStatement
# x = x[sapply(x, function(y) !all(y == "\n"))]
# names(x)
# x$OpenPositions$OpenPosition
# strategy$extract_node("TierInterestDetail")
# strategy_pra = Strategy$new(lapply(FLEX_PRA, read_xml, "2023-04-25"))
#
# > exuber_start
# [1] "2024-05-01"
# > minmax_start
# [1] "2023-02-10"
# > pra_start
# [1] "2023-04-25"
