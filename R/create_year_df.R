#' Create year_DF dataframe
#'
#' @param year_master data.frame The master year list, with column 'year'
#'                               all years in the data
#'
#' @return data.frame year_DF with columns start/end_year, start/end_date
#' @importFrom dplyr rename mutate distinct
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @export
#'
#' @return data.frame
create_year_df <- function(year_master) {
  assertthat::assert_that('year' %in% colnames(year_master),
                          msg = "year_master requires column 'year'")
  # Add end_year, start_date, and end_date columns
  year_df <- year_master %>%
    dplyr::select(year) %>%
    dplyr::distinct() %>%
    # start_year is the year in the master year list
    dplyr::rename(start_year = year) %>%
    # end year is the same as the start year
    dplyr::mutate(end_year = start_year,
           # Start and end dates are the first/last days of the year, respectively
           start_date = as.Date(paste0(start_year, '-01-01'), format = '%Y-%m-%d'),
           end_date = as.Date(paste0(end_year, '-12-31'), format = '%Y-%m-%d'))
  return(year_df)
}
