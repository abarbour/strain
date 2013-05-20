#' Borehole strainmeter (bsm) methods and classes
#' @aliases bsm
#' @rdname bsm-methods
#' @export
#' @param X  generic object
#' @param i.type character; instrument type
#' @param B  object of class \code{bsm}
#' @param ...  additional arguments
as.bsm <- function(X, i.type="gtsm", g0az=0, ...) UseMethod("as.bsm")
# @method as.bsm default
#' @S3method as.bsm default
as.bsm.default <- function(X, i.type, g0az, ...){
  i.type <- match.arg(i.type)
  # class, inherits
  class(X) <- c("bsm")
  attr(X, "i.type") <- i.type
  attr(X, "g0az") <- strain_azimuth(g0az)
  return(X)
}

#' @rdname bsm-methods
print.bsm <- function(B, ...){
  cat(paste0(get_itype(B),"-style borehole strainmeter data\n:\t"), head(B), "\t...")
}

#' Logical test for 'bsm' class
#' @rdname bsm-methods
#' @export
is.bsm <- function(X) inherits(X, "bsm")

