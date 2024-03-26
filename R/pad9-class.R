#' "pad9" class
#'
#' @name pad9-class
#' @aliases pad9
#' @family pad9
#'
#' @exportClass pad9
setClass('pad9')

#' As("character", "pad9")
#'
#' @name as
#' @family pad9
setAs('character', 'pad9', function(from) sprintf('%09d', as.numeric(from)))
