#' "pad4" class
#'
#' @name pad4-class
#' @aliases pad4
#' @family pad4
#'
#' @exportClass pad4
setClass('pad4')

#' As("character", "pad4")
#'
#' @name as
#' @family pad4
setAs('character', 'pad4', function(from) sprintf('%04d', as.numeric(from)))
