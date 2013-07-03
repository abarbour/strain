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
