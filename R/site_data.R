#' Simple Site Data
#'
#' Generates site data for plotting on map.
#'
#' Requires site metadata and monitors metadata, as read-in by the
#' `import_metadata()` function.
#'
#' @param site_metadata data.frame Site metadata
#' @param monitors data.frame Monitors metadata
#' @param site_list character vecotr List of program sites to be included in
#'                                   data frame
#' @param sitecode_col string (optional) Quoted name of column with aqs site
#'                            codes in all 3 of the above files. Default = 'aqs_sitecode'.
#'
#' @return data.frame Site data ready for plotting
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select rename distinct filter left_join sym
#' @export
site_data <- function(site_metadata,
                             monitors,
                             site_list,
                             sitecode_col = 'aqs_sitecode') {

  monitors_join <- monitors %>%
    dplyr::filter(!!dplyr::sym(sitecode_col) %in% site_list) %>%
    dplyr::select(aqs_sitecode, pqao,
                  city_name, local_site_name) %>%
    dplyr::distinct()

  site_metadata <- site_metadata %>%
    dplyr::select(aqs_sitecode, location_setting, latitude, longitude, state_name,
           county_name) %>%
    dplyr::filter(!!dplyr::sym(sitecode_col) %in% site_list) %>%
    dplyr::left_join(monitors_join) %>%
    dplyr::rename(lat = latitude,
           lng = longitude) %>%
    dplyr::distinct()

  return(site_metadata)
}
