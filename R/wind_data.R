#' Tidy & extract wind data
#'
#' Extract wind data from sample data and returns data frame with columns for
#' date-time, formatted as posixct, aqs sitecode, wind direction, & wind speed.
#'
#' @param sample_data data.frame Hourly sample data. Should include
#'                               met parameters for wind direction and wind
#'                               speed.
#'
#' @return data.frame Tidied wind data with columns for wind direction,
#'                    wind speed, sitecode, and datetime (posixct).
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select group_by summarize mutate rename filter
#' @importFrom tidyr spread
#' @export
wind_data <- function(sample_data) {
  browser()
  wind_data <- sample_data %>%
    dplyr::select(aqs_sitecode, parameter, date_local, time_local,
                  sample_duration, sample_measurement, units_of_measure,
                  qualifier) %>%
    dplyr::filter(sample_duration == '1 HOUR') %>%
    dplyr::filter(parameter %in% c('Wind Direction - Resultant',
                                   'Wind Direction - Scalar',
                                   'Wind Speed - Resultant',
                                   'Wind Speed - Scalar')) %>%
    dplyr::select(-sample_duration, -qualifier) %>%
    dplyr::group_by(aqs_sitecode, parameter, date_local, time_local,
                    units_of_measure) %>%
    dplyr::summarize(sample_measurement = mean(sample_measurement)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(parameter, sample_measurement) %>%
    dplyr::rename(WDR = 'Wind Direction - Resultant',
                  WDS = 'Wind Direction - Scalar',
                  WSR = 'Wind Speed - Resultant',
                  WSS = 'Wind Speed - Scalar' ) %>%
    dplyr::mutate(WD = ifelse(!is.na(WDR), WDR, WDS),
                  date_time = paste(date_local, time_local),
                  WS = ifelse(!is.na(WSR), WSR, WSS)) %>%
    dplyr::filter(!is.na(WD)) %>%
    dplyr::select(aqs_sitecode, date_time, WS, WD, units_of_measure) %>%
    dplyr::mutate(date_time = as.POSIXct(date_time,
                                         format = "%Y-%m-%d %H:%M",
                                         tz = ''))
  return(wind_data)
}
