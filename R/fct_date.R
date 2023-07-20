#' IsDate
#'
#' @description A function to detect the desired date format
#' \cite{\link{median_data_computation function}}.
#'
#' @param mydate list of dates
#'
#' @return true or false
#'
#' @export

IsDate <- function(mydate, date.format = "%Y-%m-%d") {
    tryCatch(!is.na(as.Date(mydate, date.format)),
             error = function(err) {FALSE})
  }
