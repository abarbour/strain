#' Return a calibration matrix, inverted or not
#' 
#' @details
#' Coefficients are in the form presented by Roeloffs 2010.
#' 
#' The pseudo-inverse is computed using \code{corpcor::pseudoinverse} with
#' default tolerances.
#' 
#' @name calibMatrix
#' @param C. numeric; 
#' @param D. numeric; 
#' @param H. numeric; 
#' @param axis_deg numeric; 
#' @param delta_deg numeric;
#' @param pinvert logical; should the matrix be (pseudo-)inverted? 
#' @param ... additional parameters
#' @export
calibMatrix <- function(C.=1:4, D.=C., H.=C., axis_deg=0, delta_deg=0, pinvert=FALSE, verbose=TRUE, ...) UseMethod("calibMatrix")
#' @rdname calibMatrix
#' @S3method calibMatrix default
calibMatrix.default <- function(C.=1:4, D.=C., H.=C., axis_deg=0, delta_deg=0, pinvert=FALSE, verbose=TRUE, ...){
  ##
  ## 
  ##
  C. <- as.matrix(C., ncol=1)
  D. <- as.vector(D.)
  H. <- as.vector(H.)
  theta <- delta_deg + axis_deg
  theta2 <- 2*theta
  RDH <- rotate(D., H., theta2, ...)
  CMAT <- cbind(C., RDH)
  #
  #if (scale.gam){gam2 <- "gam2"; CMAT[,3]<-CMAT[,3]/2} else {gam2 <- "2gam2"}
  #
  colnames(CMAT) <- c("ar","gam1","gam2")
  if (nrow(CMAT)==4) rownames(CMAT) <- paste0("BS",0:3)
  #
  if (verbose) message("forward calibration matrix created")
  if (pinvert){
    CMAT <- corpcor::pseudoinverse(CMAT)
    if (verbose) message("pseudo-inverse calculated")
  }
  return(CMAT)
}
