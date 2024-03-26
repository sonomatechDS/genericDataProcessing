#' Site-method summary data
#'
#' Returns summary statistics for hourly observations for each site/method
#' including, min, max, mean, count valid, etc.
#'
#' @param sample_data data.frame Sample data data.frame
#' @param methods_list data.frame Methods metadata 
#'
#' @return data.frame Summary statistics per site-year-parameter-method-poc
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select group_by summarize ungroup mutate left_join n
#' @importFrom lubridate year ymd
#' @export
#'
#' @examples
#' site_method_summary_data(sample_data, methods_list)
site_method_summary_data <- function(sample_data,
                                methods_list) {
  summary_table <- sample_data %>%
    dplyr::mutate(invalid = ifelse(!is.na(sample_measurement), 0, 1),
           aboveMDL = ifelse(sample_measurement > detection_limit, 1, 0)) %>%
    dplyr::select(aqs_sitecode, parameter, parameter_code, method_code,
                  poc, sample_duration, method, sample_measurement, detection_limit,
                  date_local, units_of_measure, invalid, aboveMDL) %>%
    dplyr::mutate(year = lubridate::year(lubridate::ymd(date_local))) %>%
    dplyr::select(-date_local) %>%
    dplyr::group_by(year, aqs_sitecode, parameter, parameter_code, method,
             method_code, poc,
             sample_duration, units_of_measure) %>%
    dplyr::summarize(count = dplyr::n(),
              avgMDL = mean(detection_limit, na.rm = T),
              countInvalid = sum(invalid),
              countAboveMDL = sum(aboveMDL, na.rm=T),
              pctAboveMDL = 100*countAboveMDL/count,
              max = max(sample_measurement, na.rm=T),
              median = median(sample_measurement, na.rm = T),
              min = min(sample_measurement, na.rm = T),
              average = mean(sample_measurement,na.rm=T),
              countValid = count - countInvalid) %>%
    dplyr::ungroup()

  methods_join <- methods_list %>%
    dplyr::select(parameter_code, method_code, method_type,
           collection_description, analysis_description)

  summary_table <- summary_table %>%
    dplyr::left_join(methods_join, by = c("parameter_code", "method_code"))

  return(summary_table)

}
