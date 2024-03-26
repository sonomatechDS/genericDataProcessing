#' Adjust POC value for summaries of annual data
#'
#' Where multiple POCs exist for a given site, parameter, method, year,
#' set all POC ids to the minimum for that site to calculate summaries
#'
#' @param annual_data data.frame Annual data data.frame.
#'
#' @return data.frame Annual data with adjusted POC values
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select group_by summarize ungroup left_join n
#' @export
#'
#' @examples
#' adjust_annual_poc(annual_data)
adjust_annual_poc <- function(annual_data) {
  ad <- annual_data
  adj_annual_data <- annual_data %>%
    dplyr::group_by(aqs_sitecode, parameter, method, year) %>%
    dplyr::summarize(count = dplyr::n(),
              poc = min(poc, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-count) %>%
    dplyr::left_join(ad)

  return(adj_annual_data)
}
