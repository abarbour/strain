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