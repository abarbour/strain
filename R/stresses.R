#' Compute plane stress tensor from plane strain tensor
#' @param E11,E22,E12  numeric; strain components in 1-2 coordinate system
#' @param G numeric; elastic shear modulus; 30 GPa is a reasonable value
#' for deep crustal rock
#' @param nu numeric; drained Poissons ratio; 1/4 is a reasonable value
#' for a Poissonian solid
#' @param extension.positive logical; positive extensions are extensional
#' @param as.mat logical; should the result be returned as a matrix?
#' @param S symmetric stress tensor
#' @export
#' @seealso \code{\link{CoulombStress}}
stresses <- function(E11, E22, E12, G=30e9, nu=0.25, extension.positive=TRUE, as.mat=TRUE){
  mu2 <- 2*G
  mu2nu <- mu2/(1-nu)
  T11 <- mu2nu * (E11 + nu*E22)
  T22 <- mu2nu * (nu*E11 + E22)
  T12 <- mu2 * E12
  T33  <- T13 <- T23 <- 0
  Sv <- c(S11=T11,S12=T12,S13=T13, S12=T12,S22=T22,S23=T23, S13=T13,S23=T23,S33=T33)
  if (as.mat){
    matrix(Sv, nrow=3, byrow=TRUE)
  } else {
    Sv
  }
}

#' @rdname stresses
#' @export
principal_stresses <- function(S){
  S <- as.matrix(S)
  Sp <- eigen(S, symmetric=TRUE)
  n <- 1:3
  nms <- paste0("S",n)
  # S1 > S2 > S3
  names(Sp$values) <- nms
  # direction cosines
  nms <- paste0("n",n)
  colnames(Sp$vectors) <- nms
  rownames(Sp$vectors) <- c("i","j","k")
  Sp
}

#' Calculate Coulomb stresses on a plane from a given stress tensor
#' @export
#' @param S symmetric stress tensor, in Pascals
#' @param n an optional vector normal to the plane on which to project
#' the stresses... not used
#' @param B Skempton's coefficient for an undrained material;
#' 0.85 is a reasonable number for bulk Westerly granite
#' @param mu coefficient of internal friction; Byerlee
#' puts this at 0.6 for deep rock
#' @param in.bar logical; should the stresses be in units of bar rather than Pa?
#' @param unit.scale numeric; stresses will be multiplied by this value.  
#' Useful to return, say, gigaPascals (use \code{unit.scale=1e-9} then); but, note
#' that because Pore pressure is derived from volume stress, that too will be
#' scaled by this value.
#' @seealso \code{\link{stresses}}
CoulombStress <- function(S, n=NULL, mu=0.6, B=0.85, in.bar=FALSE, unit.scale=1){
  #
  Sp <- principal_stresses(S)
  Vec <- Sp$vectors
  if (!is.null(n)){
    stopifnot(length(as.vector(n))==3)
    #Rm <- projection_matrix(n)
    #Vec <- apply(Vec, FUN=function(v){Rm %*% v}, MARGIN=2)
  } else {
    n <- NA
  }
  # Coefficient of internal frction
  theta <- (pi/4) - atan(mu)/2
  ct <- cos(theta)
  st <- sin(theta)
  CS <- matrix(c(ct,st))
  #
  #calc, for each eigenvector: n1*cos(theta) + n3*sin(theta)
  # (the direction normal to the plane with max total CFF stress)
  rnmax <- t(apply(Vec[c(1,3),], function(a){t(a) %*% CS}, MARGIN=2))
  # Principal stresses
  S <- Sp$values
  if (in.bar) S <- S/1e5
  Sunits <- paste("scaled", ifelse(in.bar,"bars","Pascals"))
  S <- unit.scale * S
  #
  # Dilatation (trace of S)
  Dil <- sum(diag(S))/3
  # undrained Pore Pressure (Dil * Skemptons)
  Pp <- -B * Dil
  # traction on the plane
  Trac <- matrix(rowSums(S %*% rnmax), ncol=1)
  # normal and shear vector, and their magnitudes
  Sigmag <- rnmax %*% Trac
  Tau <- Trac - t(Sigmag[,] * rnmax)
  Taumag <- norm(Tau, "F") # Frobenius norm: qrt(sum(squares))
  # CFF magnitude
  Cff <- Taumag  +  theta * (Sigmag + Pp)
  #
  list(params = data.frame(internal.fric=mu, angle=theta*180/pi, Skemp=B, stress.units=Sunits, scale=1/unit.scale),
       Traction = data.frame(Orientation=as.vector(rnmax), Values=as.vector(Trac)),
       Pore.pressure = Pp,
       Stresses = data.frame(Dilatational = Dil, Normal = Sigmag, Shear = Taumag, Coulomb = Cff)
  )
}
#' @rdname CoulombStress
#' @export
projection_matrix <- function(n=c(1,1,1)){
  n <- as.matrix(n)
  if (prod(n)>1) stop("'n' must be normalized")
  crossprod(t(n))
}
