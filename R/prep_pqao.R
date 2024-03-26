#' Prepare pqao values in qc and raw data
#'
#' Creates full list of pqaos by combining qc data and monitors data.
#' Adds agency type column from pqao_list
#' Updates pqaos in qc data.
#' Joins pqaos to raw data.
#'
#' @param pqao_list data.frame The pqao_list imported with import_metadata
#' @param monitors_list data.frame The monitors_list metadata
#' @param qc_data data.frame QC dataframe#'
#' @param df data.frame A data frame to join pqao data to.
#' @param df_type string Either 'raw' for annual, sample, or daily data, or
#'                       'qc' for any qc data.
#'
#' @importFrom dplyr select distinct left_join mutate rename group_by summarize ungroup
#' @importFrom magrittr "%>%"
#' @export
#' 
#' @return data.frame
#'
#' @examples
#' prep_pqao(pqao_list, monitors_list, one_point_qc, daily_data, 'raw')
prep_pqao <- function(pqao_list, monitors_list, qc_data, df, df_type) {
  # add type2 column to pqao list
  type2_def <- data.frame(
    agency_type_code = c('F', 'G', 'H', 'I', 'T', 'A', 'N', 'K'),
    type2 = c(1,1,1,1,1,2,2,3)
  )

  pqao_list <- pqao_list %>%
    dplyr::left_join(type2_def, by = 'agency_type_code') %>%
    dplyr::arrange(type2, agency)

  # 1) Create list of unique pqao from pqao_list, monitors and qc data
  #  a) create full list of pqaos across all qc data
  qc_pqao <- qc_data %>%
    dplyr::select(pqao_code, pqao, monitoring_agency,
                  aqs_sitecode, poc, method_code) %>%
      distinct()

  #   b) add agency type from pqao_list
  cross_ref_pqao <- qc_pqao %>%
    dplyr::left_join(pqao_list, by = c("pqao" = "agency")) %>%
    dplyr::select(pqao_code, pqao, monitoring_agency, aqs_sitecode,
                  poc, method_code, agency_type, agency_type_code, type2) %>%
    dplyr::distinct()

  #   c) Create list of unique pqao/site/poc/method in monitors and qc data
  unique_site_poc_pqao <- monitors_list %>%
    # combine monitors and qc agencies (on aqs_sitecode, monitoring agency, poc)
    dplyr::left_join(cross_ref_pqao) %>%
    # fill pqao/code that exist in qc but not monitors
    #     with monitoring agency name/code
    dplyr::mutate(pqao = ifelse(is.na(pqao), monitoring_agency, pqao),
                  pqao_code = ifelse(is.na(pqao_code),
                                     monitoring_agency_code, pqao_code)) %>%
    # get list of distinct agencies in MONITOR and QC data per site/poc/method
    dplyr::group_by(aqs_sitecode, poc, parameter_code,
                    pqao_code, pqao, last_method_code) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(site_poc = paste0(aqs_sitecode, poc)) %>%
    dplyr::distinct(., site_poc, .keep_all = T) %>% select(-site_poc) %>%
    # set method_code == last_method_code
    dplyr::rename(method_code = last_method_code)

  # d) Overwrite qc pqao/site/poc/param with unique site pqao, Keep df method code
  if (df_type == 'qc') {
    df_tidy <- df %>%
      dplyr::left_join(unique_site_poc_pqao,
                       by = c('aqs_sitecode', "poc", "parameter_code")) %>%
      dplyr::select(-pqao.x, -pqao_code.x, -method_code.y, -count) %>%
      dplyr::rename(pqao = pqao.y, pqao_code = pqao_code.y, method_code = method_code.x)
  }

  # 2) Join unique pqao/site/poc/method to raw data
  if (df_type == 'raw') {
    # join pqao information to raw data by sitecode, poc, paramter, method_code
    df_tidy <- df %>%
      dplyr::left_join(unique_site_poc_pqao)
  }

  # Export dataframe with joined/corrected pqao data
  return(df_tidy)
}
