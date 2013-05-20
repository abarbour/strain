#' Map an angle into the corresponding strain angle.
#' 
#' Uniaxial strains in parallel directions are indistinguishible; this
#' function maps an angle into a physically realistic range: \eqn{(-90,90]}
#' 
#' The mapping is relative to the zero-angle.  The mapping uses 
#' \code{\link{.az_wrap}} with \code{wraps=180}.
#' 
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
#' @rdname strain_azimuth
#' @export
.az_wrap <- function(Az, wraps=c(360)){
  FUN <- function(X, w) X %% w
  for (wrap in wraps) Az <- FUN(Az, wrap)
  return(Az)
}


#' @title Functions to get information about a \code{bsm} object.
# @keywords methods S3methods tapers
#' @name strain-getters
#' @rdname strain-getters
#' @author A.J. Barbour <andy.barbour@@gmail.com>
#' @docType methods
# @import RColorBrewer
#' @param B object with class\code{bsm}
# @param ... additional arguments
NULL

#' @rdname strain-getters
#' @export
get_az <- function(B) UseMethod("get_az")
#' @S3method get_itype bsm
get_az.bsm <- function(B){
  aXi <- attributes(B)$g0az
  if (is.null(aXi)){
    warning("Cannot find an instrument orientation!")
  } else {
    return(aXi)
  }
}

#' @rdname strain-getters
#' @export
get_itype <- function(B) UseMethod("get_itype")
#' @S3method get_itype bsm
get_itype.bsm <- function(B){
  aXi <- attributes(B)$i.type
  if (is.null(aXi)){
    warning("Cannot find an instrument type!")
  } else {
    return(aXi)
  }
}