#' Fixes leading zeros
#'
#' Fixes leading zeros issues tribal, pqao and agency
#' codes. Fills NA pqao/pqao_code with monitoring_agency/code.
#' Any of the following present columns will be padded with zeros:
#'     - tribal_code (3)
#'     - monitoring_agency_code (3/4) depending of tribal status
#'     - pqao_code (3/4) depending of tribal status
#'
#' @param df  A dataframe
#' @return data.frame
#' @import data.table
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate if_else
#' @importFrom stringr str_pad
#' @keywords internal
clean_codes <- function(df) {
  if ('tribal_code' %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(tribal_code = stringr::str_pad(as.character(tribal_code),
                                                   3, side = "left", pad = "0"))
  }
  if ('agency_code' %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(agency_code = dplyr::if_else(agency_type != "Tribal",
                            stringr::str_pad(agency_code, 4, "left", "0"),
                            stringr::str_pad(agency_code, 3, "left", "0")))
  }
  if ('monitoring_agency_code' %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(monitoring_agency_code = dplyr::if_else(is.na(tribal_code),
                                                  stringr::str_pad(monitoring_agency_code, 4, "left", "0"),
                                                  stringr::str_pad(monitoring_agency_code, 3, "left", "0")))
  }
  if ('pqao_code' %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(pqao = dplyr::if_else(is.na(pqao), monitoring_agency, pqao),
                    pqao_code = dplyr::if_else(is.na(pqao_code),
                                               monitoring_agency_code,
                                               as.character(pqao_code))) %>%
      dplyr::mutate(pqao_code = dplyr::if_else(is.na(tribal_code),
                                       stringr::str_pad(pqao_code, 4,
                                                        "left", "0"),
                                       stringr::str_pad(pqao_code, 3,
                                                        "left", "0")))
  }
  return(df)
}

