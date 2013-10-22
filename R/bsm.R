#' Methods for borehole strainmeter (bsm) data
#'
#' @name bsm-methods
#' @aliases bsm_methods
#' @rdname bsm-methods
#' 
#' @param x  object of class \code{bsm}
#' @param ...  additional arguments
#'
#' @author A.J. Barbour <andy.barbour@@gmail.com>
#' @docType methods
NULL

#' @rdname bsm-methods
#' @export
print.bsm <- function(x, ...){
  cat(paste0(get_itype(x),"-style borehole strainmeter data:\n"))
  str(x, nest.lev=1)
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
#' @param cal.tbl character; retrieve calibration coefficients from this table
#' @param ...  additional arguments
#' @export
as.bsm <- function(X, ...) UseMethod("as.bsm")
#' @rdname as.bsm
#' @method as.bsm default
#' @S3method as.bsm default
as.bsm.default <- function(X, 
                           g0az=0, 
                           station="", 
                           i.type="GTSM", 
                           linearize=TRUE, 
                           cal.tbl=c("pbou"), # add more!
                           ...){
  i.type <- match.arg(i.type)
  if (linearize) X <- linearize(X)
  # see hydrogeo (shepard plot) for fix:
  cal.tbl <- match.arg(cal.tbl)
  do.call("data", list(cal.tbl)) # assumed loading will give caltbl, which is
  # now false, meaning operations below will fail
  E <- NULL
  B <- list(G=X, #gauge strain
            calib=calmat(cal.tbl, station), # gauge calib coeffs
            E=matrix(rep(NA,3),ncol=3), # calibrated strain
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
gaugeOrientations <- function(B, restrict.range=TRUE) UseMethod("gaugeOrientations")

#' @rdname bsm-methods
#' @param restrict.range logical; should the orientiations be plotted in appropriate uniaxial directions?
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
plot_orientations <- function(angs, gauge.labels=seq_along(angs), name="", opar=TRUE, ...) UseMethod("plot_orientations")
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
plot_orientations.bsm <- function(angs, ...){
  B <- angs
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

#' Remind, with ASCII art, the gauge numbering/naming conventions
#' @export
orientation_conventions <- function(){
  cat("
  PBO/GTSM:
    #      0  3  2
    #       \\ | /  dTheta=60
    #      ---|---1
    #       / | \\  dTheta=60
    
  Hodgkinson et al 2013:
    #      1  4  3
    #       \\ | /  dTheta=60
    #      ---|---2
    #       / | \\  dTheta=60

  Roeloffs 2010 (x1 in ch1 and x2 in ch3 directions):    
    #      gauges                 extensions
    #      0  3  2                2  3  0
    #       \\ | /  dTheta=60       \\ | /
    #      ---|---1               ---|---1
    #       / | \\  dTheta=60       / | \\
  ")
}


#' Assemble high-frequency strain data
#' @details
#' This uses a set of scripts included with the package, namely 
#' \code{hfbsm},
#' \code{bottle.py}, \code{bottle_merge.py}
#' 
#' @param sta4
#' @param sta16
#' @param yst
#' @param dst
#' @param st
#' @param yen
#' @param den
#' @param en
#' @param sampling
#' @param ...
#' @export
hfbsm <- function(sta4, yst, dst, st="00:00:00", yen, den, en="00:00:00", sampling=1, ...) UseMethod("hfbsm")
#' @rdname hfbsm
#' @method hfbsm default
#' @S3method hfbsm default
hfbsm.default <- function(sta4, yst, dst, st="00:00:00", yen, den, en="00:01:00", sampling=1, ...){
	#
  sta16 <- sta16_from_sta4(sta4, meta="bsm")
  stopifnot(!is.null(sta16))
  #
  stopifnot(sampling==20 | sampling==1)
  if (sampling==1){
    message("minimum length returned is one hour")
  } else {
    message("minimum length returned is one minute")
  }
	#
	func <- "hfbsm"
	package.dir <- find.package('strain')
  hfbsm.dir <- pypath <- file.path(package.dir, func)
  script <- file.path(hfbsm.dir, func)
  
  pyver <- try(system(file.path(pypath,"version.py"), intern=TRUE))
  if (inherits(pyver, "try-error")) stop( "python version poke failed" )
  
  #hfbsm Bnum 16-character-code start_year  start_day_of_yr  start_time  end_year end_day_of_year end_time [samp]
	#hfbsm B073 varian073bcs2006  2009 105 13:00:00 2009 105 16:00:00 [[1] pypath]
  cmd <- sprintf("%s %s %s %04i %i '%s' %04i %i '%s' %i %s", 
                 script, sta4, sta16, 
                 yst, dst, st,
                 yen, den, en,
                 sampling,
                 pypath)
  message(cmd)
  results <- try(system(cmd, intern=TRUE))
  if (inherits(results, "try-error")) stop( "hfbsm failed" )
  #
  names(results) <- c("StationNames","DTfrom","DTto","SamplingHz","URLsrc","linfi","rawfi")
  toret <- list(cmd=cmd, res=results, py=pyver)
  class(toret) <- "hfbsm.nfo"
  return(toret)
}

#' @export
load_hfbsm <- function(X, ...) UseMethod("load_hfbsm")
#' @rdname hfbsm
#' @export
#' @method load_hfbsm hfbsm.nfo
#' @S3method load_hfbsm hfbsm.nfo
load_hfbsm.hfbsm.nfo <- function(object, file.type=c("lin","raw"), loc=".", ...){
  #
  otype <- match.arg(file.type)
  file.type <- paste0(otype,"fi")
  res <- object[["res"]]
  stanames <- strsplit(res[["StationNames"]],"\t")[[1]]
  sta4 <- stanames[1]
  sta16 <- stanames[2]
  samp <- strsplit(res[["SamplingHz"]],"\t")[[1]]
  hz <- as.numeric(samp[3])
  file <- res[[file.type]]
  nas <- switch(file.type, rawfi="999999", linfi="999999.000000")
  #
  op <- options(digits.secs=3)
  #
  POS <- function(x, TZ="UTC"){
    #2009-08-03T01:30:21
    #2009-04-15T13:00:00.350000
    lubridate::ymd_hms(x)
  }
  #
  #print(file)
  dat <- read.table(file, header=TRUE, colClasses=c("character",rep("numeric",5)), comment.char="#", na.strings=nas)
  #
  dat <- within(data=dat, expr={
    Datetime <- POS(Datetime)
    RelInd <- hz*RelInd
    })
  class(dat) <- c("hfbsm",otype)
  on.exit(options(op))
  return(invisible(dat))
}
