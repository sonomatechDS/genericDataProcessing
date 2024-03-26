#' Generate completeness data
#'
#' Returns dataframe with site completeness metrics, and site-parameter
#' completeness metrics.
#'
#' Requires annual_data data.frame. Optionally accepts
#' a character vector of required parameters to filter by.
#'
#' @param annual_data_adj_poc data.frame Annual data with POC adjusted by
#'                                    through `adjust_annual_poc()`.
#' @param required_param_codes character vector (optional) List of required
#'                                               parameters to filter results
#'                                               by.
#'
#' @return data.frame Site and site-parameter completeness data
#' @importFrom magrittr `%>%`
#' @importFrom dplyr n group_by summarize ungroup mutate filter arrange left_join select row_number
#' @export
#'
#' @examples
#' annual_data_adj_poc <- adjust_annual_poc(annual_data)
#' network_completeness_data <- completeness_data(annual_data_adj_poc)
#' network_completeness_data <- completeness_data(annual_data_adj_poc, c('42101'))
completeness_data <- function(annual_data_adj_poc, required_param_codes = NULL) {

  if (!is.null(required_param_codes)) {
    annual_data_adj_poc <- annual_data_adj_poc %>%
      dplyr::filter(parameter_code %in% required_param_codes)
  }

  complete_parameter_site_avg <- annual_data_adj_poc %>%
    dplyr::filter(year >= 2017)

  # Get stats per site (i.e. stats across all parameters at a site)
  complete_site_avg <- complete_parameter_site_avg %>%
    dplyr::group_by(aqs_sitecode, year) %>%
    dplyr::summarize(site_parameter_count = dplyr::n(),
              site_avg_percent = mean(observation_percent),
              site_min_percent = min(observation_percent),
              site_max_percent = max(observation_percent), .groups = 'drop') %>%
    dplyr::group_by(year) %>%
    dplyr::arrange(desc(site_avg_percent), .by_group = TRUE, .groups = 'drop') %>%
    dplyr::mutate(order = row_number()) %>%
    dplyr::ungroup()

  completeness_data <- complete_parameter_site_avg %>%
    dplyr::left_join(complete_site_avg) %>%
    dplyr::select(aqs_sitecode, parameter_code, poc, parameter, sample_duration,
           pollutant_standard, method, units_of_measure, order, year,
           observation_count, observation_percent, validity_indicator,
           valid_day_count, required_day_count, null_observation_count,
           site_parameter_count, site_avg_percent, site_min_percent,
           site_max_percent)

  return(completeness_data)
}
