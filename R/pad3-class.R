#' "pad3" class
#'
#' @name pad3-class
#' @aliases pad3
#' @family pad3
#'
#' @exportClass pad3
setClass('pad3')

#' As("character", "pad3")
#'
#' @name as
#' @family pad3
setAs('character', 'pad3', function(from) sprintf('%03d', as.numeric(from)))
