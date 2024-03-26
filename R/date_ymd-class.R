#' "date_ymd" class
#'
#' @name date_ymd-class
#' @aliases date_ymd
#' @family date_ymd
#'
#' @exportClass date_ymd
setClass('date_ymd')

#' As("character", "date_ymd")
#'
#' @name as
#' @family date_ymd
setAs("character","date_ymd", function(from) as.Date(from,
                                                     tryFormats = c('%Y-%m-%d',
                                                                    '%Y/%m/%d',
                                                                    '%m/%d/%Y',
                                                                    '%m-%d-%Y')))
