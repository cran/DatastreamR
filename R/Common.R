#' @importFrom logger log_error
#' @importFrom logger log_info
#' @importFrom stringr str_extract
#' @importFrom httr POST
#' @importFrom httr content_type
#' @importFrom httr content
#' @importFrom httr use_proxy
#' @importFrom httr status_code
#' @importFrom ini read.ini
#'
library(stringr)
library(R6)
# Method to convert Json Ticks to date time

jsonDateToDatetime = function(jsonDate)
{
  if (!is.null(jsonDate))
  {
    jdate = NULL
    match1 = tryCatch({grep(pattern = "^/Date\\(-?\\d*\\)/",
                            x = jsonDate, value = TRUE)},
                      error = function(e) {
                        logger::log_error(message(e))})

    if (identical(match1, character(0)))
    {
      match2 <- tryCatch({grep(pattern = "^/Date\\(-?\\d*\\+|-\\d*\\)/",
                               x = jsonDate, value = TRUE)},
                         error = function(e) {
                           logger::log_error(message(e))})

      if (identical(match2, character(0)))
      {
        logger::log_error(paste('Common.R', 'jsonDateToDatetime', "No match for the Date string :",
                                jsonDate))
        return (jsonDate)
      }
      else
        jdate <- stringr::str_extract(match2, "\\(-?\\d*\\+|-")
    }
    else
      jdate <- stringr::str_extract(match1, "\\(-?\\d*\\)")

    if (!is.null(jdate))
    {
      jdate <- substr(jdate, 2, nchar(jdate) - 1)
      ndate = as.POSIXct(as.double(jdate)/1000, origin = "1970-01-01", tz = "UTC")
      return (ndate)
    }
    else
    {
      errMsg = paste('Common.R', 'jsonDateToDatetime', 'No match for the Date string : ', jsonDate )
      logger::log_error(errMsg)
      return (jsonDate)
    }
  }
  else
    return (jsonDate)
}


# Datetime to JSON Ticks conversion

toJSONdate = function(inputDate)
{
  # convert to /Date() object with no of ticks relative to epoch_date
  epoch_date = as.Date("1970-01-01", tz = "UTC")
  if (inherits(inputDate, c("Date", "POSIXt")))
  {
    ticks = difftime(inputDate, epoch_date, units = "secs", tz = "UTC") * 1000
    jsonTicks = paste("/Date(", as.character(floor(ticks)), ")/", sep = "")
    return (jsonTicks)
  }
  else #should not call with unsupported type
  {
    errMsg = paste('Common.R', 'toJSONdate', 'Invalid Date format', inputDate)
    logger::log_error(errMsg)
    return (inputDate)
  }
}
