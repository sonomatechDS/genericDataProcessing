#' Calculate CV value
#'
#' Calculates the CVs within a grouping of data. Percent difference value
#' must already be calculated/available as a column in the data.
#'
#' Groupings may be specified in the ... argument.
#'
#' @param data data.frame Data with a column for percent difference
#' @param .pct_diff key The unquoted column name of the column with percent
#'                      difference values
#' @param ... key Unquoted column names of columns to group by.
#'
#' @return data.frame CV and count values per grouping
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by summarize ungroup mutate n enquo filter select arrange
#' @importFrom assertthat assert_that
#' @importFrom stats qchisq median quantile
#' @export
#'
#' @examples
#' calculate_cv(one_point_qc,
#'              .pct_diff = percent_difference,
#'              method,
#'              pqao)
calculate_cv <- function(data, .pct_diff, ...) {

  pct_diff <- dplyr::enquo(.pct_diff)

  sqrt_catchneg <- function(x) {
    assertthat::assert_that(all(is.na(x) | x > (-1e-10)),
                            msg = "sqrt() applied to negative number.")

    return(sqrt(max(0,x)))
  }

  summary <- data %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(n = dplyr::n(),
                  sum1 = sum(!!pct_diff),
                  sum2 = sum((!!pct_diff)^2),
                  sum3 = sum(abs(!!pct_diff)),
                  sum4 = sum(abs((!!pct_diff)^2))) %>%
    dplyr::ungroup()


  cv <- summary %>%
    dplyr::filter(n > 1)%>%
    dplyr::mutate(X = stats::qchisq(0.1, df = (n - 1))) %>%
    dplyr::mutate(CV_unadj = sqrt_catchneg((n*sum2 - sum1^2)/(n*(n - 1))),
           chisq_adj=sqrt_catchneg((n - 1)/X),
           CV = CV_unadj * chisq_adj) %>%
    dplyr::select(..., n, CV)%>%
    dplyr::arrange(CV)

  return(cv)
}
