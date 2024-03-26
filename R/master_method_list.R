#' Create master pqao method list
#'
#' Requested df's should have the columns "pqao", "method", "method_code",
#' "method_type", "equivalent_method". The latter two columns are joined
#' from the methods_list (metadata) dataframe via the tidy_method() function.
#'
#' @param df_vec vector Collection of dataframes to include in creating of master
#'                      pqao-method list. Dfs should have columns "pqao",
#'                      "method", "method_code", "method_type",
#'                      "equivalent_method" (see tidy_method() function).
#'
#' @return data.frame The master pqao-method list
#' @importFrom dplyr group_by summarize arrange
#' @importFrom magrittr "%>%"
#' @export
master_method_list <- function(df_vec) {
  # Create df shell to hold master list
  df_method_list <- data.frame()
  for (df in df_vec) {
    df_methods <- df %>%
      # Group df by pqao, method_code, method, method_type, equivalent_method
      dplyr::group_by(pqao, method_code, method, method_type, equivalent_method) %>%
      # Count rows per grouping
      dplyr::summarize(count = n())

    # Bind df summary to others
    df_method_list <- rbind(df_method_list, df_methods)
  }
  # Final summary of bound summary df's
  method_list <- df_method_list %>%
    dplyr::group_by(pqao, method_code, method, method_type, equivalent_method) %>%
    dplyr::summarize(count = sum(count)) %>%
    # order alphabetically by pqao name
    dplyr::arrange(pqao)
  return(method_list)
}
