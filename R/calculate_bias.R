#' Calculate bias value
#'
#' Calculates the bias within a grouping of data. Percent difference value
#' must already be calculated as a column in the data.
#'
#' Groupings may be specified in the ... argument.
#'
#' @param data data.frame Data with a column for percent difference
#' @param .pct_diff key The unquoted column name of the column with percent
#'                      difference values
#' @param ... key Unquoted column names of columns to group by.
#'
#' @return data.frame Bias and count values per grouping
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by summarize ungroup mutate n enquo filter select arrange
#' @importFrom assertthat assert_that
#' @importFrom stats qt median
#' @export
#'
#' @examples
#' calculate_bias(one_point_qc,
#'                .pct_diff = percent_difference,
#'                method,
#'                pqao)
calculate_bias <- function(data, .pct_diff, ...) {
  pct_diff <- dplyr::enquo(.pct_diff)

  sqrt_catchneg <- function(x) {
    assertthat::assert_that(all(is.na(x) | x > (-1e-10)),
                            msg = "sqrt() applied to negative number.")

    return(sqrt(max(0,x)))
  }

  summary <- data %>%
    dplyr::group_by(...) %>%
    mutate(n = dplyr::n(),
           sum1 = sum(!!pct_diff),
           sum2 = sum((!!pct_diff)^2),
           sum3 = sum(abs(!!pct_diff)),
           sum4 = sum(abs((!!pct_diff)^2))) %>%
    dplyr::ungroup()

  bias <- summary %>%
    dplyr::mutate(AB_adj = 1/n*sum3)%>%
    dplyr::mutate(t_adj = stats::qt(c(0.95), df=(n-1)))%>%
    dplyr::mutate(AS_adj = sqrt_catchneg((n*sum4-sum3^2)/(n*(n-1))))%>%
    dplyr::mutate(abs_bias = AB_adj+ t_adj*AS_adj/sqrt_catchneg(n))%>%
    dplyr::select(..., n, abs_bias)%>%
    dplyr::arrange(abs_bias)

  return(bias)
}
