#' @title Functions to get information about a \code{bsm} object.
# @keywords methods S3methods tapers
#' @name strain-getters
#' @rdname strain-getters
#' @author A.J. Barbour <andy.barbour@@gmail.com>
#' @docType methods
# @import RColorBrewer
#' @param B object with class\code{bsm}
#' @param strn.type character; the type of strain to retrieve from \code{B}
#' @param ... additional arguments
NULL

#' @rdname strain-getters
#' @export
get_strains <- function(B, ...) UseMethod("get_strains")
#' @rdname bsm-methods
#' @method get_strains bsm
#' @S3method get_strains bsm
get_strains.bsm <- function(B, strn.type=c("gauge","calib"), ...){
  strn.type <- match.arg(strn.type)
  if (strn.type=="gauge"){
    aXi <- B$G
  } else {
    ##: if (!is.calibrated(B)) B <- calibrate(B, ...)
    aXi <- B$E
    aXi[is.na(aXi)] <- 0
  }
  if (is.null(aXi)){
    warning(sprtinf("Couldn't find %s strains!",strn.type))
  } else {
    return(aXi)
  }
}

#' @rdname strain-getters
#' @export
get_az <- function(B) UseMethod("get_az")
#' @rdname bsm-methods
#' @method get_az bsm
#' @S3method get_az bsm
get_az.bsm <- function(B){
  aXi <- attributes(B)$g0az
  if (is.null(aXi)){
    warning("Couldn't find an instrument orientation!")
  } else {
    return(aXi)
  }
}

#' @rdname strain-getters
#' @export
get_station <- function(B) UseMethod("get_station")
#' @rdname bsm-methods
#' @method get_station bsm
#' @S3method get_station bsm
get_station.bsm <- function(B){
  aXi <- attributes(B)$station
  if (is.null(aXi)){
    warning("Couldn't find a station name!")
  } else {
    return(aXi)
  }
}

#' @rdname strain-getters
#' @export
get_itype <- function(B) UseMethod("get_itype")
#' @rdname bsm-methods
#' @method get_itype bsm
#' @S3method get_itype bsm
get_itype.bsm <- function(B){
  aXi <- attributes(B)$i.type
  if (is.null(aXi)){
    warning("Couldn't find an instrument type!")
  } else {
    return(aXi)
  }
}