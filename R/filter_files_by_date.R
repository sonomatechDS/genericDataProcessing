#' Filter files by date range
#'
#' From a list of filenames in the format
#'     ppppp_yyyymmdd_yyyymmdd_service
#'       - first date is the begin-date of the data
#'       - second date is the end-date of the data
#'       - ppppp is a parameter code
#' returns the files that contain data within a specified year or
#' date range.
#'
#' @param date_range character vector A start date/year (string) or date/year
#'                                    range (vector) in the format 'yyyymmdd' or
#'                                    'yyyy'. If range given, vector should have
#'                                    two elements, c(start_date/year,
#'                                                    end_date/year)
#'                                    If only start date/year given, all files
#'                                    with data >= start date will be included.
#' @param files_to_import character vector A list of filenames in the format
#'                                         'ppppp_yyyymmdd_yyyymmdd_service.*'
#'
#' @importFrom stringr str_extract_all
#' @keywords internal
#' 
#' @return character Subest of files_to_import that meets specified criteria.
filter_files_by_date <- function(date_range, files_to_import) {
  if (nchar(date_range[1]) == 8) {
    # convert specified start date to numeric
    start_date <- as.numeric(date_range[1])
    # Parse end dates from filenames and convert to number
    # e.g. for filename 44201_20200815_20201012_sample, extract 20201012
    file_end_dates <- as.numeric(stringr::str_extract_all(files_to_import,
                                                          '[0-9]{8}(?=_\\D)'))
    # Filter files to those whose end dates are greater than the specified start
    files_to_import <- files_to_import[start_date <= file_end_dates]
  } else if (nchar(date_range[1]) == 4) {
    # convert specified start date to numeric
    start_year <- as.numeric(date_range[1])
    # Parse end years from filenames and convert to number
    # e.g. for filename 44201_20200815_20191012_sample, extract 2019
    file_end_years <- as.numeric(stringr::str_extract_all(files_to_import,
                                                          '\\d{4}(?=\\d{4}_\\D)'))
    files_to_import <- files_to_import[start_year <= file_end_years]
  }
  if (length(date_range) == 2) {
    if (nchar(date_range[2]) == 8) {
      # If an end date is specified, convert to numeric
      end_date <- as.numeric(date_range[2])
      # Extract start dates from filenames
      # e.g. for filename 44201_20200815_20201012_sample, extract 20200815
      file_start_dates <- as.numeric(stringr::str_extract_all(files_to_import,
                                                              '(?<=^[0-9]{5}_)[0-9]{8}'))
      # Filter files to those whose start dates are less than the specified end
      files_to_import <- files_to_import[end_date >= file_start_dates]
    } else if (nchar(date_range[2]) == 4) {
      # convert specified start date to numeric
      end_year <- as.numeric(date_range[2])
      # Parse end years from filenames and convert to number
      # e.g. for filename 44201_20200815_20191012_sample, extract 2020
      file_start_years <- as.numeric(stringr::str_extract_all(files_to_import,
                                                              '(?<=^\\d{5}_)\\d{4}'))
      files_to_import <- files_to_import[end_year >= file_start_years]
    }
  }
  return(files_to_import)
}
