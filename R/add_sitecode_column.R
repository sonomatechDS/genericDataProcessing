#' Add AQS_SITECODE column to dataframe
#'
#' Adds a column names aqs_sitecode to a dataframe by concatenating the
#' state_code, county_code, and site_number. The dataframe must contain these
#' named columns, each of type character.
#'
#' @param df A data frame with columns state_code, county_code, and site_number.
#'
#' @return data.frame
#'
#' @importFrom tidyr unite
#' @importFrom assertthat assert_that
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' df <- data.frame(state_code = '32', county_code = '003',
#'                  site_number = '0002')
#' add_sitecode_column(df)
add_sitecode_column <- function(df) {
  assertthat::assert_that(all(c('state_code',
                                'county_code',
                                'site_number') %in% colnames(df)),
            msg = 'One of state_code, county_code, site_number missing from df')
  df <- df %>%
    tidyr::unite(aqs_sitecode,
                 c(state_code, county_code, site_number),
                 sep = '',
                 remove = FALSE)
  return(df)
}
