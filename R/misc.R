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
#' @param dtheta numeric; the disretization in degrees.
#' @rdname strain-miscellany
#' @export
circle <- function (dtheta = 1) {
  thetas <- seq.int(from = 0, to = 360, by = dtheta) * pi / 180
  return(cbind(x = cos(thetas), y = sin(thetas)))
}