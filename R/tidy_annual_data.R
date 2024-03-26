#' Tidy annual data
#'
#' Filters data for validity_indicator == 'Y' and event_type == 'No Events'.
#'
#' @details Filters annual data per aqs_sitecode, parameter, method, poc,
#' sample_duration and pollutant_standard to only groups with data for
#' more than 75% of total years available.
#'
#' @param annual_data_adj_poc data.frame Annual data passed through `adjust_annual_poc()`
#'
#' @return data.frame
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select group_by n summarize ungroup distinct filter left_join
#' @export
#'
#' @examples
#' annual_data_adj_poc <- adjust_annual_poc(annual_data)
#' tidy_annual_data <- tidy_annual_data(annual_data_adj_poc)
tidy_annual_data <- function(annual_data_adj_poc) {

  tidy_annual_data <- annual_data_adj_poc %>%
    dplyr::select(aqs_sitecode, parameter_code, parameter, poc, sample_duration,
                pollutant_standard, metric_used, method, year, units_of_measure,
                event_type, observation_count, observation_percent,
                validity_indicator, valid_day_count, required_day_count,
                exceptional_data_count, null_observation_count, arithmetic_mean,
                fourth_max_value, ninety_eighth_percentile,
                fiftieth_percentile) %>%
    dplyr::filter(validity_indicator == 'Y',
           event_type == 'No Events')

    total_years <- tidy_annual_data %>%
      dplyr::select(year) %>%
      unique() %>%
      nrow()

    filter_tidy_data <- tidy_annual_data %>%
      dplyr::distinct() %>%
      dplyr::group_by(aqs_sitecode, parameter, method, poc,
               sample_duration, pollutant_standard) %>%
      dplyr::summarize(countYears = dplyr::n(),
                minYear = min(year),
                maxYear = max(year),
                yearRange = maxYear-minYear) %>%
      dplyr::ungroup() %>%
      dplyr::filter(countYears >= ceiling(0.75*total_years)) %>%
      dplyr::left_join(tidy_annual_data)

    return(filter_tidy_data)
}
