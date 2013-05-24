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
#' The calculation is based on 2-D Mohr's circle  with the 1-axis being normal
#' and the 2-axis being shear.  
#' 
#' 
#' @param S strain information
#' @param ... additional parameters
#' @export
#' @references Altamani 19XX, "Strain Rosettes" by J.H.Meir, p. 412
principals <- function(S, ...) UseMethod("principals")
#' @rdname bsm-methods
#' @method principals bsm
#' @S3method principals bsm
principals.bsm <- function(B, ...){
  S <- get_strains(B, strn.type="calib")
  #
  stopifnot(is.calibrated(S))
  #
  # The angle of maximum shear is relative to the angle
  # of the zeroth gauge.
  E <- extensions(S, strain:::.constants$bsm.relative_orientations)
  e0 <- E[,1]
  e1 <- E[,2]
  e2 <- E[,3]
  e3 <- E[,4]
  rm(E)
  A1 <- (e1+e3)/2
  A2 <- (e0+e2+e1)/3
  A <- cbind(A1,A2)
  rm(A1,A2)
  B1 <- sqrt((e0 - e2)**2 + (e2 - e1)**2 + (e0 - e1)**2) * sqrt(2)/3
  B2 <- sqrt((e0 - e2)**2/3 + (e1 - e3)**2/4)
  B <- cbind(B1,B2)
  rm(B1,B2)
  alpha <- atan(sqrt(3)*(e2 - e1)/(2*e0 - e2 - e1))/2
  rm(e0, e1, e2, e3)
  # four combinations: A1-B1, A1-B2, A2-B1, A2-B2
  combs <- expand.grid(1:2,1:2)
  principals(A[,1],B[,1], alpha)
}

#' @rdname principals
#' @method principals default
#' @S3method principals default
principals.default <- function(A, B, alpha, is.radians=TRUE){
  Eps_max <- A + B
  Eps_min <- A - B
  if (!is.radians) alpha <- alpha * pi / 180
  Shear_max <- A + B*cos(2*alpha)
  Shear_azim <- - 2 * B * sin(2*alpha)
  return(data.frame(Emax=Eps_max, Emin=Eps_min, Shear=Shear_max, ShearAzimuth=Shear_azim))
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