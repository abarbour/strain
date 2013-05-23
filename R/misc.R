#' @title Miscellaneous utilities
# @keywords methods S3methods tapers
#' @name strain-miscellany
#' @rdname strain-miscellany
#' @author A.J. Barbour <andy.barbour@@gmail.com>
#' @docType methods
# @import RColorBrewer
# @param B object with class\code{bsm}
# @param ... additional arguments
NULL

#' Return a unit circle.
#' 
#' @details Origin is for a right-hand (ccw) system.
#' 
#' @param dtheta numeric; the disretization in degrees.
#' @param origin numeric; where the first point begins, in degrees
#' @rdname strain-miscellany
#' @export
circle <- function (dtheta = 1, origin=0) {
  thetas <- seq.int(from = origin, to = (origin+360), by = dtheta) * pi / 180
  return(cbind(x = cos(thetas), y = sin(thetas)))
}