
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
    transmute(datetime = as.POSIXct(t, tz = tz),
              water_level = as.numeric(v)) %>%
    select(datetime, water_level)
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

