#' @title Flex
#'
#' @description
#' Get flex reports using Intreractive Brokers Flex API.
#'
#' @details
#' The Flex class is used to get flex reports from Interactive Brokers.
#'
#' @param token The token to be used for the IB Flex API
#' @param query The query to be used for the IB Flex API
#'
#' @return A Flex object
#' @export
Flex = R6::R6Class(
  "Flex",

  public = list(
    #' @field token The token to be used for the IB Flex API
    token = NULL,

    #' @field query The query to be used for the IB Flex API
    query = NULL,

    #' @description
    #' Initialize the Flex object
    #'
    #' @param token The token to be used for the IB Flex API
    #' @param query The query to be used for the IB Flex API
    #'
    #' @return The Flex object
    initialize = function(token, query) {
      self$token = token
      self$query = query
    },

    #' @description
    #' Send the request to the IB Flex API
    #'
    #' @return The new token to be used for the IB Flex API
    send = function() {
      url_send = sprintf("%s.SendRequest", private$url_flex)
      resp_flex_req = GET(url_send,
                          query = list(
                            t = self$token,
                            q = self$query,
                            v = private$flex_version
                          ),
                          httr::config(ssl_verifypeer = FALSE)
      )
      root = content(resp_flex_req, encoding = "UTF-8")
      newtoken = root |>
        html_element("ReferenceCode") |>
        html_text()
      return(newtoken)
    },

    #' @description
    #' Get the flex report
    #'
    #' @param newtoken The new token to be used for the IB Flex API
    #'
    #' @return The flex report
    get = function(newtoken) {
      url_get = sprintf("%s.GetStatement", private$url_flex)

      # While loop since we want to try multiple times
      try = TRUE
      num_ = 0
      while (try && num_ < 100) {
        # get XML
        p = RETRY(
          "GET",
          url_get,
          query = list(
            q = newtoken,
            t = self$token,
            v = private$flex_version
          ),
          # write_disk(file_name, overwrite = TRUE),
          httr::config(ssl_verifypeer = FALSE),
          times = 5L
        )
        res = content(p, encoding = "UTF-8")

        # checkif report is generated
        test = res |> html_element("ErrorCode")
        if (length(test) != 0) {
          print(res)
          Sys.sleep(2L)
          next
        } else {
          try = FALSE
        }

        num_ = num_ = 0 + 1
      }

      return(res)
    },

    #' @description
    #' Get the flex report
    #'
    #' @return The flex report
    get_flex_report = function() {
      newtoken = self$send()
      xml_data = self$get(newtoken)
      return(xml_data)
    }
  ),
  private = list(
    url_flex = "https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService",
    flex_version = 3
  )
)
