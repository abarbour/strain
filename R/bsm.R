#' Methods for borehole strainmeter (bsm) data
#'
#' @name bsm-methods
#' @aliases bsm_methods
#' @rdname bsm-methods
#' 
#' @param X  generic object
#' @param g0az numeric; the orientation of the zeroth gauge
#' @param station character; name of the station associated with the data
#' @param i.type character; instrument type
#' @param linearize logical; Should \code{X} be linearized?
#' @param B  object of class \code{bsm}
#' @param ...  additional arguments
NULL

#' @rdname bsm-methods
#' @export
print.bsm <- function(B, ...){
  cat(paste0(get_itype(B),"-style borehole strainmeter data:\n"))
  str(B, nest.lev=1)
}

#' Convert an object to one with class \code{'bsm'}
#' 
#' @name as.bsm
#' @aliases bsm
#' 
#' @param X  generic object
#' @param g0az numeric; the orientation of the zeroth gauge
#' @param station character; name of the station associated with the data
#' @param i.type character; instrument type
#' @param linearize logical; Should \code{X} be linearized?
#' @param ...  additional arguments
#' @export
as.bsm <- function(X, ...) UseMethod("as.bsm")
#' @rdname as.bsm
#' @method as.bsm default
#' @S3method as.bsm default
as.bsm.default <- function(X, g0az=0, station="", i.type="GTSM", linearize=TRUE, ...){
  i.type <- match.arg(i.type)
  if (linearize) X <- linearize(X)
  B <- list(G=X, #gauge strain
            E=matrix(rep(NA,3),ncol=3), # calibrated strain
            calmat=NA, # gauge strains
            P=NA) # principal strains
  # cannot set attributes on NULL!
  attr(B$G, "straintype") <- "gauge"
  attr(B$G, "linear") <- linearize
  attr(B$E, "straintype") <- "calib"
  attr(B, "station") <- station
  attr(B, "g0az") <- strain_azimuth(g0az)
  attr(B, "i.type") <- i.type
  class(B) <- c("bsm")
  return(B)
}

#' @rdname as.bsm
#' @export
is.bsm <- function(X) inherits(X, "bsm")

#' @rdname as.bsm
#' @export
is.raw_strain <- function(X) inherits(X, "gauge")

#' Return gauge orientations, restricted or not.
#' 
#' @details
#' The restriction is done with \code{\link{strain_azimuth}}
#' 
#' @param B object
#' @param restrict.range logical; should the values be in strain azimuths
#' @export
gaugeOrientations <- function(B, ...) UseMethod("gaugeOrientations")

#' @rdname bsm-methods
#' @method gaugeOrientations bsm
#' @S3method gaugeOrientations bsm
gaugeOrientations.bsm <- function(B, restrict.range=TRUE){
  ch0 <- get_az(B)
  ch1 <- ch0-60
  ch2 <- ch1-60
  ch3 <- ch2-30
  orients<-c(ch0,ch1,ch2,ch3)
  if (restrict.range) orients <- strain_azimuth(orients)
}

#' Plot the orientations of the instrument gauges.
#' @param angs numeric; angles of gauges to plot
#' @param name character; add something to the title
#' @param opar logical; should the original graphics parameter be set upon exit?
#' @param ... additional parameters
#' @export
plot_orientations <- function(angs, ...) UseMethod("plot_orientations")
#' @rdname plot_orientations
#' @method plot_orientations default
#' @S3method plot_orientations default
plot_orientations.default <- function(angs, gauge.labels=seq_along(angs), name="", opar=TRUE, ...){
  angs <- as.vector(angs)
  lims <- c(-1,1)
  sc <- 1.2
  if (opar){  
    oploc <- par(no.readonly = TRUE)
    par(mar=c(0,0,3,0), bty="n", xaxt="n", yaxt="n", pty="s")
  }
  plot(circle(30, -angs[1]), 
       main=sprintf("%s Gauge Orientations", name), asp=1,
       xlab="", ylab="", col="gray", type="l", xlim=sc*lims, ylim=sc*lims, ...)
  stopifnot(length(angs)==length(gauge.labels))
  invisible(mapply(FUN=plot_gaugeline, angs, gauge.label=as.character(gauge.labels), SIMPLIFY=T))
  if (opar) on.exit(par(oploc))
}
#' @rdname bsm-methods
#' @method plot_orientations bsm
#' @S3method plot_orientations bsm
plot_orientations.bsm <- function(B, ...){
  angs <- gaugeOrientations(B, restrict.range=TRUE)
  plot_orientations(angs, 
                    gauge.labels=c("CH0","CH1","CH2","CH3"), 
                    name=paste(get_station(B), get_itype(B)), 
                    ...)
  return(invisible(angs))
}

#' Plots lines for gauge orientations
#' 
#' @details The rotation is in a left-hand (cw) sense with zero being the y-axis.
#' This is because in the field the gauges are generally oriented from magnetic
#' North in the East direction.
#'
# because I'm dislexic:
#' @aliases plot_guageline
#' 
#' @param theta.deg numeric; the angle to rotate from \eqn{x,y}
#' @param gauge.label character; the label to associate with the line
#' @param x numeric; default 1-axis range
#' @param y numeric; default 2-axis range
#' @param ... additional parameters passed to \code{lines}
#' @param restrict.range logical; should the values be in strain azimuths
#' @export
plot_gaugeline <- function(...) UseMethod("plot_gaugeline")
#' @rdname plot_gaugeline
#' @method plot_gaugeline default
#' @S3method plot_gaugeline default
plot_gaugeline.default <- function(theta.deg=0, gauge.label=NULL, x=c(0,0), y=c(-1,1), ...){
  newxy <- rotate(x, y, theta.deg, "left")
  gauge.label <- match.arg(gauge.label, c("","CH0","CH1","CH2","CH3"))
  lines(newxy, ...)
  newxy <- 1.17*newxy[2,]
  text(newxy[1],newxy[2], sprintf("%s\n(%.1f)", gauge.label, theta.deg))
  return(NULL)
}
