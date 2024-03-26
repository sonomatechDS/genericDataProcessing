#' Generate annual trend data
#'
#' Returns a dataframe with the mean annual value the specified .agg_col
#' by year and any other grouping columns passed to the ... argument.
#'
#' Takes in annual data data.frame,
#' and  (optionally) passed through the `tidy_annual_data()` function to filter
#' data to only aqs_sitecode-parameter-method-poc-sample_duration combinations
#' with data from >= 75% of all years.
#'
#' @param tidy_annual_data data.frame Annual data that has ideally been
#'                                          passed through the
#'                                          `tidy_annual_data()` function.
#' @param .agg_col key Column to calculated statistics for (unquoted).
#' @param ... key Columns to group annual data by (unquoted).
#'
#' @return data.frame Annual trend data
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select group_by summarize ungroup distinct filter mutate enquo enquos
#' @importFrom tidyr spread gather
#' @export
#'
#' @examples
#' tidy_annual_data <- tidy_annual_data(annual_data)
#' annual_trend_data(tidy_annual_data,
#'                   .agg_col = arithmetic_mean,
#'                   aqs_sitecode, parameter, method,
#'                   sample_duration, pollutant_standard)
annual_trend_data <- function(tidy_annual_data,
                              .agg_col = arithmetic_mean,
                              ...) {

  agg_col <- dplyr::enquo(.agg_col)

  dots <- dplyr::enquos(...)
  n <- length(dots)
  #browser()
  trend_data <- tidy_annual_data %>%
    dplyr::select(year, !!agg_col, ...) %>%
    dplyr::distinct() %>%
    dplyr::group_by(year, ...) %>%
    dplyr::summarize(annual_mean = mean(!!agg_col, na.rm=T)) %>%
    tidyr::spread(year, annual_mean) %>%
    #dplyr::mutate(index = row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::gather(year, annual_mean, (n+1):(ncol(.))) %>%
    dplyr::filter(!is.na(annual_mean)) %>%
    dplyr::mutate(year = as.integer(year))

  return(trend_data)

}

