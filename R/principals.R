#' Calculate principal strains
#' 
#' Strainmeters are generally designed to resemble common
#' strain-gauge rosette configurations to simplify gauge
#' combinations.
#' 
#' @section Rosette classifications:
#' \itemize{
#'   \item{BSM}{T-Delta}
#'   \item{LSM-3}{Rectangular three-gauge \eqn{45^{\circ}}, star shaped}
#'   \item{LSM-2}{Rectangular two-gauge \eqn{90^{\circ}}}
#' }
#' 
#' @details 
#' If there are only two components of the LSM the angle of maximum shear
#' cannot be solved for.
#' 
#' @param S a strain object
#' @param ... additional parameters
#' @export
#' @references Altamani 19XX, "Strain Rosettes" by J.H.Meir, p. 412
principals <- function(S, ...) UseMethod("principals")
#' @rdname bsm-methods
#' @method principals bsm
#' @S3method principals bsm
principals.bsm <- function(B){
  E <- get_strains(B)
  stopifnot(is.raw_strain(E))
  e1 <- E[,1]
  e2 <- E[,2]
  e3 <- E[,3]
  e4 <- E[,4]
  A1 <- (e3+e4)/2
  A2 <- (e1+e2+e3)/3
  B1 <- sqrt((e1 - e2)**2 + (e2 - e3)**2 + (e1 - e3)**2) * sqrt(2)/3
  B2 <- sqrt((e1 - e2)**2/3 + (e3 - e4)**2/4)
  Tan2alph <- sqrt(3)*(e2 - e3)/(2*e1 - e2 - e3)
  #...
}

#' @rdname principals 
#' @S3method principals lsm3
principals.lsm3 <- function(L){
  .NotYetImplemented()
}

#' @rdname principals 
#' @S3method principals lsm2
principals.lsm2 <- function(L){
  .NotYetImplemented()
}