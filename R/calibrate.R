#' Calibration matrices for BSM gauge data
#' 
#' @name calibration-matrix
#' @aliases calibrations
#' @rdname calibration-matrix
#' @author A.J. Barbour <andy.barbour@@gmail.com>
#' @docType methods
#' @param x object to print
#' @param ... additional parameters
NULL

#' @rdname calibration-matrix
#' @method print cal
#' @S3method print cal
print.cal <- function(x, ...) message("Strainmeter calibration coefficients")
#' @rdname calibration-matrix
#' @method print.cal roel1
#' @S3method print.cal roel1
print.cal.roel1 <- function(x, ...){
  print.cal(x)
  message("\tTidal potential method, from Roeloffs 2010 (I)")
}
#' @rdname calibration-matrix
#' @method print.cal roel2
#' @S3method print.cal roel2
print.cal.roel2 <- function(x, ...){
  print.cal(x)
  message("\tTidal potential method, from Roeloffs 2010 (II)")
}
#' @rdname calibration-matrix
#' @method print.cal pbou
#' @S3method print.cal pbou
print.cal.pbou <- function(x, ...){
  print.cal(x)
  message("\tTidal potential method, PBO unofficial (e.g. xml) (4x3)")
}
#' @rdname calibration-matrix
#' @method print.cal pbo13
#' @S3method print.cal pbo13
print.cal.pbo13 <- function(x, ...){
  print.cal(x)
  message("\tTidal potential method, from Hodgkinson et al (2013)")
}
#' @rdname calibration-matrix
#' @method print.cal.pbo13 err
#' @S3method print.cal.pbo13 err
print.cal.pbo13.err <- function(x, ...){
  message("Calibration metadata")
  message("\t\torientations and RMSE/I data")
}
#' @rdname calibration-matrix
#' @method print.cal.pbo13 ANC34
#' @S3method print.cal.pbo13 ANC34
print.cal.pbo13.ANC34 <- function(x, ...){
  print.cal.pbo13(x)
  message("\t\tunconstrained solution (3X4) -- pinv")
}
#' @rdname calibration-matrix
#' @method print.cal.pbo13 ANC33
#' @S3method print.cal.pbo13 ANC33
print.cal.pbo13.ANC33 <- function(x, ...){
  print.cal.pbo13(x)
  message("\t\tunconstrained solution (3X3) -- pinv")
}
#' @rdname calibration-matrix
#' @method print.cal.pbo13 AFC34
#' @S3method print.cal.pbo13 AFC34
print.cal.pbo13.AFC34 <- function(x, ...){
  print.cal.pbo13(x)
  message("\t\tfully-constrained solution (3X4) -- pinv")
}
#' @rdname calibration-matrix
#' @method print.cal surf
#' @S3method print.cal surf
print.cal.surf <- function(x, ...){
  print.cal(x)
  message("\tWave-gradiometery method, from Grant (2010)")
}
#' @rdname calibration-matrix
#' @method print.cal.surf AS33
#' @S3method print.cal.surf AS33
print.cal.surf.AS33 <- function(x, ...){
  print.cal.surf(x)
  message("\t\txxx (3X3)")
}
#' @rdname calibration-matrix
#' @method print.cal.surf AS43
#' @S3method print.cal.surf AS43
print.cal.surf.AS43 <- function(x, ...){
  print.cal.surf(x)
  message("\t\txxx (4X3)")
}

#' @title Extract a calibration matrix from an object
#' @name calmat
#' @param X calibration table
#' @param station
#' @param inv.type
#' @param coeff.names
#' @param ... additional parameters
#' @export
calmat <- function(X, station, ...) UseMethod("calmat")
#' @rdname calmat
#' @method calmat default
#' @S3method calmat default
calmat.default <- function(X, station, ...){
  if (missing(X)){
    Cal <- strain:::.default.caltbl
    do.call("data",list(Cal))
    stopifnot(exists("caltbl"))
    X <- caltbl
  }
  calmat(X, station, ...)
}
#' @rdname calmat
#' @method calmat cal.pbou
#' @S3method calmat cal.pbou
calmat.cal.pbou <- function(X, station, ...){
  X <- as.data.frame(unclass(X))
  sta <- NULL
  CML <- subset(X, sta==station, select=-c(sta))
  if (is.null(CML) | nrow(CML)==0){
    warning(paste("No  pbou  calibration coeffs. for station", station))
    return(NA)
  }
  invs <- CML$inv
  allmat <- sapply(X=invs, FUN=function(x){
    x <- calmat_reshape(CML, inv=x)
    attr(x,"inv.type") <- x
    }, simplify=F)
  return(allmat)
}
#' @rdname calmat
#' @method calmat cal.surf1
#' @S3method calmat cal.surf1
calmat.cal.surf1 <- calmat.cal.pbou
#' @rdname calmat
#' @export
calmat_reshape <- function(X, ...) UseMethod("calmat_reshape")
#' @rdname calmat
#' @method calmat_reshape default
#' @S3method calmat_reshape default
calmat_reshape.default <- function(X, inv.type=NULL, coeff.names=c("S11","S21","S31","S41","S12","S22","S32","S42","S13","S23","S33","S43"), ...){
  stopifnot(!missing(X))
  inv <- NULL # to shutup checks
  if (!is.null(inv.type)) X <- subset(X, inv==as.character(inv.type)) #, select=-c(inv))
  X <- as.matrix(X)
  dims <- dim(X)
  pdims <- prod(dims)
  num.coeff <- 12
  if (pdims > num.coeff & length(intersect(colnames(X), coeff.names))==num.coeff){
    stopifnot(length(coeff.names)==num.coeff)
    X <- X[,coeff.names]
  } else if (pdims < num.coeff){
    stop("not enough coefficients to reshape!")
  }
  X <- matrix(as.numeric(X), ncol=4, nrow=3, byrow=TRUE)
  return(X)
}

  
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
#' @param B calibratee
#' @param Cmat calibrator
#' @param invert logical; should \code{Cmat} be (pseudo) inverted?
#' @param ... additional objects
#' @export
calibrate <- function(B, Cmat, invert=FALSE) UseMethod("calibrate")
#' @rdname bsm-methods
#' @method calibrate bsm
#' @S3method calibrate bsm
calibrate.bsm <- function(B, ...){
  G <- strains(B)
  Cmat <- calibMatrix(B)
  # add Cmat to B
  E <- calibrate(G, Cmat)
  # add E to B
  return(B)
}
#' @rdname calibrate
#' @method calibrate default
#' @S3method calibrate default
calibrate.default <- function(B, Cmat, invert=FALSE){
  if (invert) Cmat <- corpcor::pseudoinverse(Cmat)
  E. <- Cmat %*% B
  return(E.)
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
