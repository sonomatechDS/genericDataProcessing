#' Create maser DV Sitelist
#'
#' @param dv_df data.frame The design values of interest imported with the
#'                         import_design_values() function
#' @param site_master data.frame Site master dataframe created with the
#'                              master_combination_list() function
#' @param duration string The duration of design values input as dv_df. Options
#'                        include '8hr', '1hr', '24hr', 'annual'
#' @param dv_yr_prev integer The number of years previous to the most recent
#'                           available in dv_df to use for design values (i.e.
#'                           if 2019 is the most recent, dv_yr_prev = 2 would
#'                           select the 2017 design values; dv_yr_prev = 0 would
#'                           select the 2019 design values)
#'
#' @importFrom dplyr filter rename summarize left_join group_by select ungroup
#' @importFrom assertthat assert_that
#' @export
#' 
#' @return data.frame
master_dv_sitelist <- function(dv_df, site_master, duration, dv_yr_prev) {
  assertthat::assert_that(all(c('aqs_sitecode', 'state_name',
                                'county_name', 'cbsa_name', 'epa_region')
                              %in% names(dv_df)),
      "The dv_df is missing one of these columns: 'aqs_sitecode', 'state_name',\
      'county_name', 'cbsa_name', 'epa_region'")
  assertthat::assert_that(all(c('aqs_sitecode', 'pqao') %in% names(site_master)),
      "The input site_master is missing column aqs_sitecode or pqao")
  assertthat::assert_that(class(dv_df[[ncol(dv_df) - dv_yr_prev]]) == 'numeric',
      "The chosen dv_yr_prev is not available in dv_df")
  # Identify the column with the design values
  # for the year of interest, which is the nth column from the end if the
  # DV from n years ago is desired.
  dv_col <- names(dv_df)[ncol(dv_df) - dv_yr_prev]
  # Define name to set as colname for DV column
  dv_var_name <- sym(paste0('dv_', duration))
  # Remove rows without a listed aqs_sitecode
  dv_df_select <- dv_df %>%
    dplyr::filter(!(aqs_sitecode == '')) %>%
    # Select relevant columns
    dplyr::select(state_name, county_name, cbsa_name, epa_region, aqs_sitecode,
           dv_col) %>%
    # Rename the chosen design value column for the given duration
    dplyr::rename(!!dv_var_name := !!dv_col)

  # Find max DV per pqao/site
  # -- Join dv_df to site_master
  default_dv_sitelist <- site_master %>%
    dplyr::left_join(dv_df_select, by = 'aqs_sitecode') %>%
    # Group by unique pqao and site
    dplyr::group_by(pqao, aqs_sitecode) %>%
    # Identify the maximum DV per pqao/site
    dplyr::summarize(max_dv = max(!!dv_var_name),
              # Count POC's per pqao/site
              poc_count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::na.omit()

  # Find default design values per pqao
  # -- Count sites per pqao and find max DV per PQAO
  default_design_values <- default_dv_sitelist %>%
    dplyr::group_by(pqao) %>%
    dplyr::summarize(max_dv = max(max_dv),
              site_count = n()) %>%
    dplyr::ungroup()

  # Join default design values to dv_sitelist
  dv_sitelist_i <- default_design_values %>%
    dplyr::left_join(default_dv_sitelist, by = c('max_dv', 'pqao'))

  # Final dv sitelist: find max DV per PQAO and a site that has this DV
  dv_sitelist <- dv_sitelist_i %>%
    dplyr::group_by(pqao, max_dv) %>%
    dplyr::summaraize(aqs_Sitecode = min(aqs_sitecode)) %>%
    dplyr::ungroup()

  return(dv_sitelist)
}
