#' "pad2" class
#'
#' @name pad2-class
#' @aliases pad2
#' @family pad2
#'
#' @exportClass pad2
setClass('pad2')

#' As("character", "pad2")
#'
#' @name as
#' @family pad2
setAs('character', 'pad2', function(from) sprintf('%02d', as.numeric(from)))
