#' Return a calibration matrix, inverted or not
#' 
#' @details
#' The argument \code{C} may be a matrix 4with three columns representing
#' \code{C}, \code{D}, and \code{H} respectively.  If that's the case, any
#' arguments given for \code{D}, and \code{H} are ignored.
#' 
#' Coefficients are in the form presented by Roeloffs 2010.
#' 
#' The pseudo-inverse is computed using \code{corpcor::pseudoinverse} with
#' default tolerances.
#' 
#' @name calibMatrix
#' @param C numeric; 
#' @param D numeric; 
#' @param H numeric; 
#' @param axis_deg numeric; 
#' @param delta_deg numeric;
#' @param pinvert logical; should the matrix be (pseudo-)inverted? 
#' @param ... additional parameters
#' @export
calibMatrix <- function(C, ...) UseMethod("calibMatrix")
#' @rdname bsm-methods
#' @method calibMatrix bsm
#' @S3method calibMatrix bsm
calibMatrix.bsm <- function(C, ...){
  sta <- get_station(C)
  # get station
  #calmats <- stationCalibration(sta, methods="all")
  # check if calibration exists
  #   if not: default isotropic calibration?
  #   if: change calmat to list of calibration matrices
  .NotYetImplemented()
}
#' @rdname calibMatrix
#' @method calibMatrix default
#' @S3method calibMatrix default
calibMatrix.default <- function(C, D=NULL, H=NULL, axis_deg=0, delta_deg=0, invert=FALSE, verbose=TRUE, ...){
  ##
  ## 
  ##
  C <- as.matrix(C)
  #
  if ((is.null(D) & is.null(H)) | ncol(C)==3){
    stopifnot(ncol(C)==3)
    H <- C[,3]
    D <- C[,2]
    C <- C[,1]
  }
  #
  C <- as.vector(C)
  D <- as.vector(D)
  H <- as.vector(H)
  #
  theta <- delta_deg + axis_deg
  theta2 <- 2*theta
  #
  RDH <- rotate(D, H, theta2, ...)
  CMAT <- cbind(C, RDH)
  #
  #if (scale.gam){gam2 <- "gam2"; CMAT[,3]<-CMAT[,3]/2} else {gam2 <- "2gam2"}
  #
  strnames <- c("ar","gam1","gam2")
  chanames <- paste0("BS",0:3)
  dochan <- nrow(CMAT) == 4
  #
  
  if (invert){
    CMAT <- corpcor::pseudoinverse(CMAT)
    rownames(CMAT) <- strnames
    if (dochan) colnames(CMAT) <- chanames
    if (verbose) message("pseudo-inverse calibration matrix")
  } else {
    colnames(CMAT) <- strnames
    if (dochan) rownames(CMAT) <- chanames
    if (verbose) message("forward calibration matrix")
  }
  attr(CMAT,"p_inverted") <- invert
  return(CMAT)
}
