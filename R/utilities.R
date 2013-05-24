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

#' Make raw strain linear or linear strain raw
#' 
#' @name linearize
#' @param X numeric; gauge strains
#' @param gap numeric or character; the gap distance
#' @param ref.strn logical; should the results be centered?
#' @param constant numeric; value to use for de-linearizing
#' @param unref.strn logical; should the centering be reversed?
#' @param ... additional parameters
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
  if (is.numeric(gap)){
    gapd <- gap
  } else {
    gapd <- gap_distance(gap)
  }
  diam <- strain:::.constants$bsm.diam
  #
  LIN <- function(d, i.gap=gapd, i.diam=diam) {
    dc <- strain:::.constants$bsm.R
    D. <- d/dc
    C. <- i.gap/i.diam
    E. <- C. * D. / (1 - D.)
    return(E.)
  }
  X <- apply(X, MARGIN=2, FUN=LIN)
  if (ref.strn) X <- scale(X, scale=F)
  return(X)
}
#' @rdname linearize
#' @export
unlinearize <- function(X, ...) UseMethod("unlinearize")
#' @rdname linearize
#' @method unlinearize default
#' @S3method unlinearize default
unlinearize.default <- function(X, gap=NULL, constant=0, unref.strn=TRUE){
  #
  # inverts linearized gauge strain to
  # raw counts
  #
  X <- as.matrix(X)
  Xcenters <- attr(X,"scaled:center")
  #
  if (is.numeric(gap)){
    gapd <- gap
  } else {
    gapd <- gap_distance(gap)
  }
  diam <- strain:::.constants$bsm.diam
  #
  UNLIN <- function(ld, ldc=constant, i.gap=gapd, i.diam=diam) {
    dc <- strain:::.constants$bsm.R
    E. <- ld + ldc
    d <- dc * i.diam * E. / (i.gap + i.diam*E.)
    return(d)
  }
  #
  X <- apply(X, MARGIN=2, FUN=UNLIN)
  #
  if (!is.null(Xcenters)){
    xcenters <- UNLIN(Xcenters)
    if (unref.strn){
      X <- round(t(apply(X, 1, function(x){x + xcenters})))
    } else {
      X <- signif(X)
      attr(X,"scaled:center") <- xcenters
    }
  }
  #if (ref.strn) X <- scale(X, scale=F)
  return(X)
}

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
  Az <- as.numeric(Az)
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