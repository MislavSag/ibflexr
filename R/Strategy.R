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
      # node = "CashReportCurrency"
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
      xml_extracted = xml_extracted[, .SD, .SDcols = colSums(xml_extracted != 0, na.rm = TRUE) > 0]

      # Keep only unique rows
      xml_extracted = unique(xml_extracted)

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
        p = weighted.mean(tradePrice, quantity)), by = dateTime]
      setorder(trades, dateTime)
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
      # Get transfers
      transfers = self$extract_node("Transfer", FALSE)
      transfers = transfers[, .(date, cashTransfer)]
      setnames(transfers, c("timestamp", "NAV"))

      # Get NAV values
      equity_curve = self$extract_node("EquitySummaryByReportDateInBase", FALSE)
      equity_curve = equity_curve[, .(timestamp = reportDate, NAV = total)]
      equity_curve = equity_curve[NAV > 0]
      setorder(equity_curve, "timestamp")

      # see this issue: https://github.com/enricoschumann/PMwR/issues/1#issuecomment-1533207687
      if (!is.null(transfers) && nrow(transfers) > 2) {
        transfers[3:nrow(transfers), timestamp := timestamp - 1]
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
      nav_units = private$get_unit_prices(equity_curve, transfers, start_date = start_date)
      nav_units_gross = private$get_unit_prices(equity_curve_gross, transfers, start_date = start_date)

      # Scale if unit is provided
      if (!is.null(unit)) {
        private$calculate_unleveraged_NAV(nav_units, unit)
        private$calculate_unleveraged_NAV(nav_units_gross, unit)
      }

      # Add benchmark
      if (!is.null(benchmark_symbol)) {
        # benchmark_symbol = "SPY"
        benchmark_yahoo = Ticker$new(benchmark_symbol)
        benchmark = benchmark_yahoo$get_history(start = nav_units[, min(timestamp)-5],
                                                end = nav_units[, max(timestamp)+1],
                                                interval = '1d')
        setDT(benchmark)
        benchmark[, date := as.Date(date)]
        benchmark[, adj_close := as.numeric(adj_close)]
        nav_units = benchmark[nav_units, on = c("date" = "timestamp")]
        nav_units[, close_unit := adj_close / first(adj_close) * 100]
      }

      # Set names as Strategy and Benchmark
      nav_units_merged = merge(nav_units[, .(date, Strategy = price, Benchmark = close_unit)],
                               nav_units_gross[, .(date = timestamp, StrategyGross = price)],
                               by = "date", all = TRUE)
      nav_units_merged = na.omit(nav_units_merged, cols = c("Strategy", "Benchmark"))
      nav_units_merged = unique(nav_units_merged, by = c("date", "Strategy", "Benchmark"))

      # plot(as.xts.data.table(nav_units[, .(date, Strategy, Benchmark)]))
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
        cf = copy(transfers)
        cf[timestamp < start_date, timestamp := dt_[, min(timestamp)]]
        cf = cf[, .(NAV = sum(NAV)), by = timestamp]
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

# library(R6)
# library(httr)
# library(rvest)
# library(xml2)
# library(data.table)
# library(PMwR)
# library(yahoofinancer)
# FLEX_PRA = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/pra_2023.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/pra.xml"
# )
# FLEX_MINMAX = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax_2022.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax_2023.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/minmax.xml"
# )
# FLEX_EXUBER = c(
#   "https://snpmarketdata.blob.core.windows.net/flex/exuberbondsml_2023.xml",
#   "https://snpmarketdata.blob.core.windows.net/flex/exuberv1.xml"
# )
# flex_report_2022 = read_xml(FLEX_PRA[1])
# flex_report_2023 = read_xml(FLEX_PRA[2])
# report = read_xml(FLEX_PRA[3])
# flex = Flex$new(token ='22092566548262639113984', query = '803831')
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
# x = as_list(flex_reports_xml[[1]])
# x = x$FlexQueryResponse$FlexStatements$FlexStatement
# x = x[sapply(x, function(y) !all(y == "\n"))]
# names(x)
# x$TierInterestDetails$TierInterestDetail
# strategy$extract_node("TierInterestDetail")
# strategy_pra = Strategy$new(lapply(FLEX_PRA, read_xml, "2023-04-25"))
#
# > exuber_start
# [1] "2024-05-01"
# > minmax_start
# [1] "2023-02-10"
# > pra_start
# [1] "2023-04-25"
