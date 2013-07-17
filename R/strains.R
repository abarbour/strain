#' Extract strains from an abject
#' 
#' @param B object
#' @param ... additional parameters
#' @rdname strains
#' @export
#' @seealso \code{\link{bsm-methods}}
strains <- function(B, ...) UseMethod("strains")
#' @rdname bsm-methods
#' @param strn.type character; the type of strain to retrieve
#' @method strains bsm
#' @S3method strains bsm
strains.bsm <- function(B, strn.type=c("gauge","calib"), ...){
  strn.type <- match.arg(strn.type)
  if (strn.type=="gauge"){
    aXi <- B$G
  } else {
    ##: if (!is.calibrated(B)) B <- calibrate(B, ...)
    aXi <- B$E
    aXi[is.na(aXi)] <- 0
  }
  if (is.null(aXi)){
    warning(sprintf("Couldn't find %s strains!",strn.type))
  } else {
    return(aXi)
  }
}

#' Calculate vertical and dilatational strain from horizontal tensor strains
#' 
#' @param E11 numeric; 1-axis strain
#' @param E22 numeric; 2-axis strain
#' @param nu numeric; Poisson's ratio
#' @param ... additional parameters
#' @export
dilatation <- function(E11, E22, nu, ...) UseMethod("dilatation")
#' @rdname dilatation
#' @S3method dilatation default
#' @method dilatation default
dilatation.default <- function(E11, E22, nu, ...){
  if (missing(nu)) nu <- constants(FALSE)$Poisson$nu
  Ear <- as.matrix(E11+E22)
  sc <- -nu/(1-nu)
  Ezz <- sc*Ear
  attr(Ezz,"nu") <- nu
  Ekk <- Ear+Ezz
  return(list(E33=Ezz, Ekk=Ekk))
}

#' Calculate maximum horizontal shear from horizontal tensor strains
#' 
#' @param E11 numeric; 1-axis strain
#' @param E22 numeric; 2-axis strain
#' @param E12 numeric; Shear strains
#' @param ... additional parameters
#' @export
max_shear <- function(E11, E22, E12, ...) UseMethod("max_shear")
#' @rdname max_shear
#' @S3method max_shear default
#' @method max_shear default
max_shear.default <- function(E11, E22, E12, ...){
  Ediff <- as.matrix(E11-E22)
  Esh <- sqrt(Ediff/4 + E12**2)
  return(list(Esh_max=Esh))
}

#' Calculate strain invariants from horizontal tensor strains
#' 
#' @param E11 numeric; 1-axis strain
#' @param E22 numeric; 2-axis strain
#' @param E12 numeric; Shear strains
#' @param nu numeric; Poisson's ratio
#' @param ... additional parameters passed to \code{\link{dilatation}}
#' @seealso \code{\link{dilatation}} and \code{\link{max_shear}}
#' @export
invariants <- function(E11, E22, E12, nu, ...) UseMethod("invariants")
#' @rdname invariants
#' @S3method invariants default
#' @method invariants default
invariants.default <- function(E11, E22, E12, ...){
  Ekk <- dilatation(E11, E22, ...)
  if (!missing(E12)){
    Esh <- max_shear(E11, E22, E12)
  } else {
    Esh <- NULL
  }
  return(list(E33=Ekk$E33, Dil=Ekk$Ekk, Esh=Esh$Esh_max))
}

#' Transform tensor strains into geodesic reference frame (radial, transverse)
#' 
#' @param E11 numeric; 1-axis strain
#' @param E22 numeric; 2-axis strain
#' @param E12 numeric; Shear strains
#' @param theta numeric; the azimuth of the geodesic in the 1-2 axis system
#' @param ... additional parameters passed to \code{\link{invariants}}
#' @seealso \code{\link{invariants}}
#' @export
geodesic_strains <- function(E11, E22, E12, geod.deg=0, ...) UseMethod("geodesic_strains")
#' @rdname geodesic_strains
#' @S3method geodesic_strains default
#' @method geodesic_strains default
geodesic_strains.default <- function(E11, E22, E12, geod.deg=0, ...){
  # rotate into radial/transverse
  ERT <- geod_rotate(E11, E22, E12, geod.deg, NsEwNw=FALSE)
  Err <- ERT[,"Err"]
  Ett <- ERT[,"Ett"]
  Ert <- ERT[,"Ert"]
  # then calc invariants
  Einv <- invariants(Err, Ett, Ert)
  Ezz <- Einv$E33
  Dil <- Einv$Dil
  Esh <- Einv$Esh
  #
  return(list(Err=Err, Ett=Ett, Ert=Ert, Ezz=Ezz, Dil=Dil, Esh=Esh))
}

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
#' @param S strain information
#' @param ... additional parameters
#A, B, alpha, is.radians=TRUE
#' @export
#' @references Meir, J.H. (19XX),"Strain Rosettes" , in XX, pp. 412, edited by
#' Altamani 19XX
principals <- function(S, ...) UseMethod("principals")
#' @rdname bsm-methods
#' @method principals bsm
#' @S3method principals bsm
principals.bsm <- function(S, ...){
  S <- strains(S, strn.type="calib")
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
principals.lsm3 <- function(S, ...){
  .NotYetImplemented()
}

#' @rdname principals 
#' @S3method principals lsm2
principals.lsm2 <- function(S, ...){
  .NotYetImplemented()
}

#' Calculate extensions along specified axes
#' 
#' @details \code{orientations} is right-hand rule (ccw) relative the 1-axis (x);
#' in a compass system 0 would be to the West, and 90 to the North.
#' 
#' @param S calibrated strain data
#' @param orientations numeric; the angles to evaluate extensional strain at
#' @param ... additional parameters
#' @export
extensions <- function(S, orientations=c(0,45,90)) UseMethod("extensions")
#' @rdname extensions
#' @S3method extensions default
#' @method extensions default
extensions.default <- function(S, orientations=c(0,45,90)){
  orientations <- sort(unique(strain_azimuth(orientations)))
  S <- as.matrix(S)
  stopifnot(ncol(S)==3)
  #Jaeger and Cook 1976
  EFUN <- function(theta){
    rads <- 2 * theta * pi/180
    ct <- cos(rads)
    st <- sin(rads)
    areal <- S[,1]
    shear <- S[,2]
    diff <- S[,3]
    E <- (areal + shear*st + diff*ct)/2
  }
  E <- matrix(sapply(X=orientations, FUN=EFUN), ncol=length(orientations))
  colnames(E) <- paste("e", orientations, sep=".")
  return(E)
}
#' @rdname bsm-methods
#' @S3method extensions bsm
#' @method extensions bsm
extensions.bsm <- function(S, ...){
  stopifnot(is.calibrated(S))
  S <- strains(S, strn.type="calib")
  extensions(S, ...)
}