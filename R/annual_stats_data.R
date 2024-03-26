#' Generate annual statistics
#'
#' Returns a dataframe with summary statistics of the specified .agg_col
#' for groupings passed to the ... argument.
#'
#' Takes in annual data data.frame,
#' and (optionally) passed through the `annual_gt75pyears_data()` function to filter
#' data to only aqs_sitecode-parameter-method-poc-sample_duration combinations
#' with data from >= 75% of all years.
#'
#' @param tidy_annual_data data.frame Annual data that has ideally been
#'                                          passed through the
#'                                          `tidy_annual_data()` function.
#' @param .agg_col key Column to calculated statistics for (unquoted).
#' @param ... key Columns to group annual data by (unquoted).
#'
#' @return data.frame Annual summary statistics
#' @importFrom magrittr `%>%`
#' @importFrom dplyr n group_by summarize ungroup mutate enquo
#' @export
#'
#' @examples
#' tidy_annual_data <- tidy_annual_data(annual_data)
#' annual_stats_data(tidy_annual_data,
#'                   .agg_col = arithmetic_mean,
#'                   aqs_sitecode, parameter, method,
#'                   sample_duration, pollutant_standard)
annual_stats_data <- function(tidy_annual_data,
                              .agg_col = arithmetic_mean,
                              ...) {

  agg_col <- dplyr::enquo(.agg_col)

  annual_stats <- tidy_annual_data %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(count = dplyr::n(),
                mean = mean(!!agg_col, na.rm = T),
                median = median(!!agg_col, na.rm = T),
                pct90 = quantile(!!agg_col, 0.9, na.rm = T),
                pct10 = quantile(!!agg_col, 0.1, na.rm = T),
                min = min(!!agg_col, na.rm = T),
                max = max(!!agg_col, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(method = ifelse(is.na(method), 'Not Listed', method))

  return(annual_stats)
}

