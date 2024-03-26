#' Join methods information to requested df(s)
#'
#' Joins method metadata information to df(s) requested in
#' name_df_list and re-assigns updated df to variable of
#' same name. Dfs should have columns "method_code", 'parameter',
#' 'parameter_code'.
#'
#' Drops rows with NA in method_code
#'
#' @param methods_list data.frame The methods_list imported with import_metadata
#' @param df data.frame A data frame to add methods to. Should have columns
#'                      "method code", 'parameter'.
#'                      
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select left_join mutate
#' @importFrom tidyr drop_na
#' @export
#' 
#' @return data.frame
#'
#' @examples
#' prep_method(methods_list, one_point_qc)
prep_method <- function(methods_list, df) {
  param_methods <- methods_list %>%
    dplyr::select ("parameter","parameter_code","method_code",
                   "collection_description", "analysis_description", "method_type",
                   "reference_method_id","equivalent_method")


  assertthat::assert_that(all(c('method_code',
                                'parameter_code',
                                'parameter') %in% colnames(df)),
                          msg = "dataframes in name_df_list must have columns
                          method_code, parameter, parameter_code")
  df <- df %>%
      # Reset NA values... converted to ' NA' on import while padding values
      dplyr::mutate(method_code = dplyr::na_if(method_code, ' NA')) %>%
      tidyr::drop_na('method_code') %>%
      dplyr::left_join(param_methods, by = c("method_code",
                                             "parameter",
                                             "parameter_code"))
  return(df)
}


