#' Apply a rotation of vectors in the 1-2 plane about the 3-axis
#' 
#' @details 
#' \code{hand.rule} determines whether the rotation is clockwise ("left")
#' or--the default--anti-clockwise ("right").
#' 
#' @param X1 numeric; 1-axis components
#' @param X2 numeric; 2-axis components
#' @param theta.deg numeric; the rotation, in degrees, to apply
#' @param hand.rule character; the rotation sense
#' @export
rotate.default <- function(X1, X2, theta.deg, hand.rule){
  ##
  ## 2D rotation
  ##
  M <- cbind(as.vector(X1), as.vector(X2))
  if (theta.deg != 0){
    theta <- theta.deg*pi/180
    st <- sin(theta)
    ct <- cos(theta)
    hand.rule <- match.arg(hand.rule,c("right","left"))
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
#' @S3method rotate default
rotate <- function(X, Y, theta.deg=0, hand.rule=NULL) UseMethod("rotate")
#'