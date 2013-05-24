#' Calculate extensions along specified axes
#' 
#' @details \code{orientations} is right-hand rule (ccw) relative the 1-axis (x);
#' in a compass system 0 would be to the West, and 90 to the North.
#' 
#' @param S calibrated strain data
#' @param orientations numeric; the angles to evaluate extensional strain at
#' @param ... additional parameters
#' @export
extensions <- function(S, ...) UseMethod("extensions")
#' @rdname bsm-methods
#' @S3method extensions bsm
#' @method extensions bsm
extensions.bsm <- function(B, ...){
  stopifnot(is.calibrated(B))
  S <- get_strains(B, strn.type="calib")
  extensions(S, ...)
}
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