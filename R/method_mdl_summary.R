#' Method MDL Summary Data
#'
#' Summary of detection limit stats per method, including:
#' - Mean, median, min, max detection limit per method
#' - % of data above detection limit per parameter/method
#'
#' @param sample_data data.frame Sample data as read in by `import_raw_data()`
#'
#' @return data.frame Summary statistics about detection limits for hourly data
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select group_by summarize ungroup distinct n filter mutate
#' @export
#'
#' @examples
#' method_mdl_summary(sample_data)
method_mdl_summary <- function(sample_data) {
  summary <- sample_data %>%
    dplyr::filter(!is.na(sample_measurement)) %>%
    dplyr::select(parameter, sample_measurement, method,
                  method_code, detection_limit, units_of_measure) %>%
    dplyr::mutate(GTDL = ifelse(sample_measurement > detection_limit, 1, 0)) %>%
    dplyr::group_by(parameter, method, method_code, units_of_measure) %>%
    dplyr::summarize(count = dplyr::n(),
              countGTDL = sum(GTDL),
              pct10 = stats::quantile(detection_limit, 0.1),
              pct90 = stats::quantile(detection_limit, 0.9),
              median = median(detection_limit)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pctGTDL = 100*countGTDL/count) %>%
    dplyr::distinct()

  return(summary)
}
