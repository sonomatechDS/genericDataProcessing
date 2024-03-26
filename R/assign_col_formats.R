#' Parse columns with possible leading zeros
#'
#' Retrives the column names for each type of service file and parses
#' existing columns into formatting groups (see set_column_classes) and columns
#' that are unused in the codes (drop columns).
#' Returns a list of lists, where each nested list is a set of filenames
#' for each format type in the following order:
#' list(pad2_cols, pad3_cols, pad4_cols, pad9_cols, date_cols, drop_cols)
#'
#' The following columns, if they exist in a file, are assigned to each group:
#' - pad2 = state_code
#' - pad3 = method_code, method code, county_code in any case
#' - pad4 = site_number, agency code, agency_code, monitoring_agency_code,
#'          pqao_code in any case
#' - pad9 = aqs_site_id
#' - date_cols = assessment_date, select_date, date_local
#' - drop_cols = columns from full list of unused columns that exist in file
#'
#' @param path string Filepath
#' @param files_to_import list List of file names
#'
#' @return list Lists of columns to be formatted as each custom class
#' @keywords internal
assign_col_formats <- function(path, files_to_import, subfolder = '') {
  cols <- strsplit(readLines(paste0(path, subfolder, files_to_import[1]), n = 1), ',')[[1]]
  cols <- gsub('\"', '', cols)
  # state code == pad 2
  pad2_cols <- cols[regexpr('state[_ ]code', tolower(cols)) > 0]
  # method_code, method code, last_method_code, county_code in any case == pad 3
  pad3_cols <- append(cols[regexpr('.*method[_ ]code', tolower(cols)) > 0],
                      cols[regexpr('county[_ ]code', tolower(cols)) > 0])
  # site_number, agency code, agency_code, monitoring_agency_code, pqao_code
  pad4_cols <- cols[regexpr('site[ _]number', tolower(cols)) > 0]
  # pad4_cols <- append(cols[regexpr('site_number', tolower(cols)) > 0],
  #                     cols[regexpr(".*(agency|pqao)[_ ]?code", tolower(cols)) > 0])
  pad9_cols <- cols[regexpr('aqs[_ ]site[_ ]id', tolower(cols)) > 0]
  # assessment_date, select_date, date_local, open_date, close_date
  date_cols <- cols[regexpr('(assessment|select|open|close)?([_ ])?date([_ ]local)?$', tolower(cols)) > 0]
  # Full list of unused columns across datasets
  full_drop <- tolower(c('DATUM', 'DATE_GMT', 'TIME_GMT',"LAT_LON_ACCURACY",
                 'CSA', 'CSA_NAME', 'CSA_CODE',
                 'SITE_ADDRESS', "ADDRESS",
                 'PERFORMING_AGENCY',
                 'AUDITING_AGENCY',
                 'Contact Name', 'Contact Phone', 'Contact Email',
                 "CONCURRED_EXCLUSIONS",
                 "DOMINANT_SOURCE",
                 "MEASUREMENT_SCALE_DEF",
                 "NETWORKS", "SI_ID",
                 "ELEVATION","PROBE_HEIGHT","PL_PROBE_LOCATION",
                 "Land Use", "Zip Code",
                 "Met Site State Code","Met Site County Code",
                 "Met Site Site Number","Met Site Type","Met Site Distance"
                 ))
  # Unused columns in this df
  drop_cols <- full_drop[full_drop %in% cols]

  return(list('pad2_cols' = pad2_cols,
               'pad3_cols' = pad3_cols,
               'pad4_cols' = pad4_cols,
               'pad9_cols' = pad9_cols,
               'date_cols' = date_cols,
               'drop_cols' = drop_cols))
}

