#' Apply a rotation of vectors in the 1-2 plane about the 3-axis
#' 
#' @details
#' If \code{X2} is null the function assumes \code{X1} is a two column
#' object, and that the first column is the 1-axis information.
#' 
#' \code{hand.rule} determines whether the rotation is clockwise ("left")
#' or--the default--anti-clockwise ("right").
#' 
#' @name rotate
#' @param X1 numeric; 1-axis components
#' @param X2 numeric; 2-axis components
# @param B object with class \code{'bsm'}
#' @param theta.deg numeric; the rotation, in degrees, to apply
#' @param hand.rule character; the sense of rotation
#' @param ... additional parameters
#' @export
rotate <- function(X1, X2=NULL, theta.deg=0, hand.rule=c("right","left")) UseMethod("rotate")
#' @rdname rotate
#' @S3method rotate default
rotate.default <- function(X1, X2=NULL, theta.deg=0, hand.rule=c("right","left")){
  ##
  ## 2D rotation
  ##
  if (is.null(X2)){
    M <- matrix(X1[,1:2],ncol=2)
  } else {
    M <- cbind(as.vector(X1), as.vector(X2))
  }
  #
  if (theta.deg != 0){
    theta <- theta.deg*pi/180
    st <- sin(theta)
    ct <- cos(theta)
    hand.rule <- match.arg(hand.rule) #,c("right","left"))
    matdim <- 2
    MR <- switch(hand.rule,
                right=matrix(c(ct,-st,st,ct), ncol=matdim),
                left=matrix(c(ct,st,-st,ct), ncol=matdim))
    stopifnot(is.matrix(MR))
    return(M %*% MR)
  } else {
    return(M)
  }
}
#' @rdname bsm-methods
#' @method rotate bsm
#' @S3method rotate bsm
rotate.bsm <- function(X1, ...){.NotYetImplemented()}

#' Transform tensor strains in a 1-2 system into an r-t system
#' 
#' @name geod_rotate
#' @param E11 numeric; 1-axis strain
#' @param E22 numeric; 2-axis strain
#' @param E12 numeric; shear strain
#' @param geodesic.deg numeric; the azimuth of the geodesic (in degrees)
#' @param NsEwNw unused??
#' @param BSM numeric matrix with columns \code{c("ar","gam1","gam2")}
#' @param ... additional parameters
#' @export
geod_rotate <- function(E11, E22, E12, geodesic.deg=0, NsEwNw=FALSE, ...) UseMethod("geod_rotate")
#' @rdname geod_rotate
#' @export
#' @param check.columns logical; should column names be checked?
#' @details \code{\link{geod_rotate2}} is a kludge for the moment, to
#' prove tensor strain rotations
geod_rotate2 <- function(BSM, geodesic.deg=0, check.columns=FALSE, ...){
  #E <- strains(B, "calib")
  E <- as.matrix(BSM)
  cn <- colnames(E)
  cn.req <- c("ar","gam1","gam2")
  if (!all(cn %in% cn.req) & check.columns){
    stop("Column names must be: ", paste(cn.req))
  } else {
    cn.req <- 1:3
  }
  Ear <- E[,cn.req[1]]
  Ediff <- E[,cn.req[2]]
  Eshr <- E[,cn.req[3]]
  E22 <- (Ear - Ediff) / 2
  E11 <- Ear - E22
  E12 <- Eshr/2
  #
  theta <- geodesic.deg*pi/180
  st <- sin(theta)
  sst <- st*st
  ct <- cos(theta)
  cct <- ct*ct
  sct <- st*ct
  #
  # Transormation defined as
  #
  # [E]' = [R][E]
  # |E11|'     |  cct  sst    sct   | |E11|
  # |E22|'  =  |  sst  cct   -sct   |*|E22|
  # |E12|'     | -sct  sct  cct-sst | |E12|
  #
  MR <- matrix(c( cct,sst,2*sct, 
                  sst,cct,-2*sct,
                 -sct,sct,cct-sst), ncol=3, byrow=TRUE)
  E <- cbind(E11,E22,E12)
  E <- E %*% t(MR)  # so the result is columnar
  #
  E11 <- E[,1]
  E22 <- E[,2]
  E12 <- E[,3]
  ar <- E11 + E22
  gam1 <- E11 - E22
  gam2 <- 2*E12
  #print(all.equal(ar,Ear)) # warning("Areal strain was *NOT* invariant!  Something is wrong.")
  BSM <- cbind(ar,gam1,gam2)
  return(BSM)
}

#' @rdname bsm-methods
#' @S3method geod_rotate bsm
geod_rotate.bsm <- function(){.NotYetImplemented()}

#' @rdname geod_rotate
#' @S3method geod_rotate default
geod_rotate.default <- function(E11, E22, E12, geodesic.deg=0, NsEwNw=FALSE, ...){
  E <- cbind(as.vector(E11), as.vector(E22), as.vector(E12))
  cn <- paste0("E",c("rr","tt","rt"))
  #
  if (geodesic.deg != 0){
    theta <- geodesic.deg*pi/180
    st <- sin(theta)
    sst <- st*st
    ct <- cos(theta)
    cct <- ct*ct
    sct <- st*ct
    #
    # Transormation defined as
    #
    # [Egeod] = [R][E]
    # |Err|     | cct  sst   2*sct  | |E11|
    # |Ett|  =  | sst  cct  -2*sct  |*|E22|
    # |Ert|     | sct  sct  cct-sst | |E12|
    #
    MR <- matrix(c(cct,sst,2*sct, 
                   sst,cct,-2*sct,
                   sct,sct,cct-sst), ncol=3, byrow=TRUE)
    if (NsEwNw){
      # Correct if input is extensions from LSM (assumes N-E as 1-2 coords)
      # [E] = [Re][e]  thus  [Egeod] = [R][Re][e]
      MR2 <- matrix(c(0,   1,  0, 
                      1,   0,  0,
                      0.5, 1,  1), ncol=3, byrow=TRUE)
      MR <- MR %*% MR2
    }
    #
    stopifnot(is.matrix(MR))
    E <- E %*% t(MR)  # so the result is columnar
  }
  colnames(E) <- cn
  attr(E,"theta") <- geodesic.deg
  return(E)
}
