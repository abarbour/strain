

#' Test whether strain is calibrated
#' 
#' @param X object to test
#' @export
is.calibrated <- function(X) UseMethod("is.calibrated")
#' @rdname bsm-methods
#' @method is.calibrated bsm
#' @S3method is.calibrated bsm
is.calibrated.bsm <- function(X) !is.na(X$calmat)
#' @rdname is.calibrated
#' @method is.calibrated default
#' @S3method is.calibrated default
is.calibrated.default <- function(X) attr(X, "straintype") == "calib"

#' Apply a calibration to strain data
#' @export
calibrate <- function(B, ...) UseMethod("calibrate")
#' @rdname calibrate
#' @method calibrate default
#' @S3method calibrate default
calibrate.default <- function(G, calmat, invert=FALSE){
  if (invert) calmat <- corpcor::pseudoinverse(calmat)
  E. <- calmat %*% G
  return(E)
}

#' Return a calibration matrix, inverted or not
#' 
#' @details
#' The argument \code{MC} may be a matrix 4with three columns representing
#' \code{MC}, \code{MD}, and \code{MH} respectively.  If that's the case, any
#' arguments given for \code{MD}, and \code{MH} are ignored.
#' 
#' Coefficients are in the form presented by Roeloffs 2010.
#' 
#' The pseudo-inverse is computed using \code{corpcor::pseudoinverse} with
#' default tolerances.
#' 
#' @name calibMatrix
#' @param MC numeric; 
#' @param MD numeric; 
#' @param MH numeric; 
#' @param axis_deg numeric; 
#' @param delta_deg numeric;
#' @param pinvert logical; should the matrix be (pseudo-)inverted? 
#' @param ... additional parameters
#' @export
calibMatrix <- function(MC, ...) UseMethod("calibMatrix")
#' @rdname bsm-methods
#' @method calibMatrix bsm
#' @S3method calibMatrix bsm
calibMatrix.bsm <- function(MC, ...){
  sta <- get_station(MC)
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
calibMatrix.default <- function(MC, MD=NULL, MH=NULL, axis_deg=0, delta_deg=0, invert=FALSE, verbose=TRUE, ...){
  ##
  ## 
  ##
  MC <- as.matrix(MC)
  #
  if ((is.null(MD) & is.null(MH)) | ncol(MC)==3){
    stopifnot(ncol(MC)==3)
    MH <- MC[,3]
    MD <- MC[,2]
    MC <- MC[,1]
  }
  #
  MC <- as.vector(MC)
  MD <- as.vector(MD)
  MH <- as.vector(MH)
  #
  theta <- delta_deg + axis_deg
  theta2 <- 2*theta
  #
  RDH <- rotate(MD, MH, theta2, ...)
  CMAT <- cbind(MC, RDH)
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
