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
get_rawstrain <- function(B) UseMethod("get_gaugestrain")
#' @rdname bsm-methods
#' @method get_rawstrain bsm
#' @S3method get_rawstrain bsm
get_rawstrain.bsm <- function(B){
  aXi <- B$G
  if (is.null(aXi) | !is.raw_strain(aXi)){
    warning("Couldn't find gauge strains!")
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