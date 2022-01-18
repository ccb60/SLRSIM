
#' Utility function to extract data from a list of lists to a data frame
#'
#' Applying `httr::content()` to the (json) data returned by `httr::GET()`
#' request sent to the NOAA API generates a nested list of lists.  Each sub-list
#' has four (five) elements, including t = the time , and v = the value (water
#' depth).  This function takes the list of lists and converts it into a
#' dataframe with two data columns datetime (a 'POSIXct' value), and
#' water_depth.
#'
#' @param lst_o_lsts Source data, as a list of lists, as created by.
#' @param tz Designation of timezone, to correctly convert from string to R
#'        dates and times.  Correct value depends on the parameter sent to the
#'        NOAA API when requesting the data. Defaults to 'Etc/GMT+4',
#'        which is east coast US standard time.
#'
#' @return
#' a tibble
#' @export
#'
.lst_2_df <- function(lst_o_lsts, tz = 'UTC') {

  # The data from the API is all characters.
  # We convert to a matrix and restore the original names.
  mat <- matrix(unlist(lst_o_lsts), nrow = length(lst_o_lsts), byrow = TRUE)
  #browser()
  colnames (mat) <- names(lst_o_lsts[[1]])

  # Then we convert to a dataframe, change names, and convert the time to
  # POSIXct values.
  df2 <- data.frame(mat) %>%
    dplyr::transmute(datetime = as.POSIXct(t, tz = tz),
              water_level = as.numeric(v)) %>%
    dplyr::select(datetime, water_level)
  return(df2)
}


#' Check if a timezone specification is likely to work
#'
#' Compares a candidate timezone string to the list of recognized time zones in
#' the `OlsonNames()` list. available in base R.  While this is not necessarily
#' a complete list of valid time zones (which issystem-specific), it's a good
#' portlable starting point.
#'
#' @param timezone
#'
#' @return Boolean.
#'
#' @export
#' @examples
#' .is_timezone(c('Etc/GMT+5', 'EST', 'America/New_York'))

.is_timezone <- function(.timezone) {
  return(.timezone %in% (OlsonNames()))
}


#' Check if a value is (close to) an Integer
#'
#' Function is based on the code provided as an example for the function
#' `base::is.integer()` (which does NOT do what you might expect).
#' @param .x   Value or numerical vector to check
#' @param .tol The tolerance.  How close to an integer do you want to count as an
#'            integer?  Default value is related to precision of floats, and
#'            thus depends on the computer and specific R installation.  The
#'            default may be far too precise for many use cases, where you want
#'            to accept numbers somewhat close to an integer value.
#'
#' @return Boolean (`TRUE` or `FALSE`) vector, with length matching the
#'         length of `.x`.
#' @export
#'
#' @examples
#' .is_whole(4)
#' .is_whole(4.2)
#' .is.wholer(4.2, .tol = 0.25)
.is_whole <- function(.x, .tol = .Machine$double.eps^0.5) {
  if (! missing(.tol)) {
    stopifnot(length(.tol) == 1 && is.numeric(.tol))
  }
  stopifnot(is.numeric(.x))

  abs(.x - round(.x)) < .tol
}

