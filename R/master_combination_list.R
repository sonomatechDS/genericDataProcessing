#' Creates df with all combinations of variables
#'
#' Specify dataframe/type, variables to include in combinations list.
#' Preset lists of variables are available to pass into 'combo'.
#' Either 'cols' or a valid 'combo' must be specified.
#'
#' **ASSUMES THAT DF's ARE PROCESSED and all variables are "cleaned"**
#'
#' All DF's requested must contain set of cols.
#'
#' @param data Dataframe with all variables to be in be included in master list,
#'             specified in the ... argument.
#' @param ... Columns to group by (unquoted)
#'
#' @return data.frame
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select mutate distinct case_when
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' master_combination_list(mtcars, am, gear, carb)
master_combination_list <- function(data, ...) {

  master_df <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    ungroup()

  master_df<- master_df %>%
    tidyr::drop_na()

  return(master_df)
}
