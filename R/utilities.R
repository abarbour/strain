#' @title Utilities for strain-related calculations
# @keywords methods S3methods tapers
#' @name strain-utilities
#' @rdname strain-utilities
#' @author A.J. Barbour <andy.barbour@@gmail.com>
#' @docType methods
# @import RColorBrewer
# @param B object with class\code{bsm}
# @param ... additional arguments
NULL

#' Choose a GTSM gap distance
#' 
#' @details 
#' The gap is either \code{"one"} representing 100 microns, or
#' \code{"two"} for 200 microns.
#' 
#' @param gap character; the type of gap
#' @export
gap_distance <- function(gap=NULL){
  gaps <- strain:::.constants$bsm.gaps
  gap <- match.arg(gap, c("one","two"))
  switch(gap, one=gaps[1], two=gaps[2])
}

#' Make raw strain linear
#' 
#' @name linearize
#' @param X numeric; gauge strains
#' @export
linearize <- function(X, ...) UseMethod("linearize")
#' @rdname linearize
#' @method linearize default
#' @S3method linearize default
linearize.default <- function(X, gap=NULL, ref.strn=FALSE){
  #
  # Apply linearization correction to bsm datum
  #
  X <- as.matrix(X)
  #
  gapd <- gap_distance(gap)
  diam <- strain:::.constants$bsm.diam
  #
  LIN <- function(d, i.gap=gapd, i.diam=diam, ref=ref.strn) {
    dc <- strain:::.constants$bsm.R
    D. <- d/dc
    C. <- i.gap/i.diam
    E. <- C. * D. / (1 - D.)
    if (ref & length(E.)>1) E. <- E. - E.[1]
    return(E.)
  }
  #
  return(apply(X,MARGIN=2,FUN=LIN))
}
# # @rdname linearize
# # @export
# invert_linear_strain.default <- function(linbsm, gap=100e-6, diam=87e-3, constant=0){
#   #
#   # inverts linearized gauge strain to
#   # raw counts
#   #
#   D <- diam
#   d <- gap
#   E <- linbsm + constant
#   dR <- 1e8
#   toret <- dR*D*E/(d + D*E)
#   return(toret)
# }

#' Map an angle into the corresponding strain angle.
#' 
#' Uniaxial strains in parallel directions are indistinguishible; this
#' function maps an angle into a physically realistic range: \eqn{(-90,90]}
#' 
#' The mapping is relative to the zero-angle.  The mapping uses 
#' \code{\link{.az_wrap}} with \code{wraps=180}.
#' 
#' @rdname azimuth
#' @param Az numeric; an angle, in degrees
#' @param wraps numeric; the bounding range to map angles into.
#' @export
##
## Uniaxial strain can only be in the range -90 --- 0 --- 90
## however, most stations are reported in the -360 --- 0 --- 360 range.
## Fixes this.
##
strain_azimuth <- function(Az){
  # Map an angle in the range
  # <-- -360 --- -180 --- 0 --- 180 --- 360 -->
  # to 
  # (-90 --- 0 --- 90]
  #  
  w.ang <- 180
  Az <- .az_wrap(Az, w.ang)
  ai <- (Az > (w.ang/2))
  Az[ai] <- Az[ai] - w.ang
  return(Az)
}
#' @rdname azimuth
#' @export
.az_wrap <- function(Az, wraps=c(360)){
  FUN <- function(X, w) X %% w
  for (wrap in wraps) Az <- FUN(Az, wrap)
  return(Az)
}