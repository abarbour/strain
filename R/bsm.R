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
#' @export
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
#' @export
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
#' @param circle.n numeric; the degree discretization of the reference circle.
#' Setting this to zero will blank the circle
#' @param ... additional parameters
#' @export
plot_orientations <- function(angs, gauge.labels=seq_along(angs), name="", opar=TRUE, circle.n=30, ...) UseMethod("plot_orientations")
#' @rdname plot_orientations
#' @method plot_orientations default
#' @export
plot_orientations.default <- function(angs, gauge.labels=seq_along(angs), name="", opar=TRUE, circle.n=30, ...){
  angs <- if (missing(angs)){
    gauge.labels <- paste0("BS",c(0,3,2,1))
    name <- "GTSM"
    c(-30,0,30,90)
  } else {
    as.vector(angs)
  }
  lims <- c(-1,1)
  sc <- 1.2
  if (opar){  
    oploc <- par(no.readonly = TRUE)
    par(mar=c(0,0,3,0), bty="n", xaxt="n", yaxt="n", pty="s")
    on.exit(par(oploc))
  }
  ccol <- ifelse(circle.n==0,NA,"gray")
  bsmcirc <- circle(ifelse(circle.n==0, 5, abs(circle.n)), angs[1])
  plot(bsmcirc, 
       main=sprintf("%s Gauge Orientations", name), asp=1, col=NA, #ccol,
       xlab="", ylab="", type="l", xlim=sc*lims, ylim=sc*lims, ...)
  stopifnot(length(angs)==length(gauge.labels))
  toret <- invisible(sapply(seq_along(angs), 
                   FUN=function(n, ...){plot_gaugeline(angs[n], as.character(gauge.labels[n]), ...)},
                   ...
                   ))
  lines(bsmcirc, col=ccol, ...)
}
#' @rdname bsm-methods
#' @method plot_orientations bsm
#' @export
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
#' @export
plot_gaugeline.default <- function(theta.deg=0, gauge.label=NULL, x=c(0,0), y=c(-1,1), ...){
  newxy <- rotate(x, y, theta.deg, "left")
  #gauge.label <- match.arg(gauge.label, c("","CH0","CH1","CH2","CH3"))
  lines(newxy, ...)
  newxy <- 1.17*newxy[2,]
  if (!is.null(gauge.label)){
    text(newxy[1], newxy[2], sprintf("%s\n(%.1f)", gauge.label, theta.deg))
  }
  return(invisible(newxy))
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
#' 
#' @details
#' This uses a set of scripts included with the package, namely 
#' \code{hfbsm}, and the python scripts 
#' \code{bottlefile.py} and \code{bottlefile_merge.py},
#' to create (from the raw source files) high frequency PBO strain 
#' records for each channel at a station.
#' 
#' @param sta character; the station code.  This can be either
#' the 4-character code (e.g., \code{'B084'}) of the
#' 16-character code (e.g., \code{'pinyon084bcs2006'}) because it
#' uses \code{\link{station_data}} with \code{use.regexp=TRUE}
#' @param year numeric; the year of the start time of the desired dataset
#' @param jday numeric; the Julian day (day of year) of the start time
#' @param st character; the hour:minute:second of the start time. Defaults to
#' \code{'00:00:00'} if missing.
#' @param duration numeric; the relative end-time, represented as the
#' number of seconds from the start time. Defaults to a reasonable length
#' that won't cause too much processing
#' time to elapse, which depends on \code{sampling}.
#' @param sampling numeric; the sampling rate, in Hz, of the data to be downloaded; can be either 1 or 20
#' @param verbose logical; should messages and warnings be given?
#' @param ... additional parameters 
#' @param object an object having class \code{'hfbsm.nfo'}
#' @param file.type character; the type of strain data to load: 
#' \code{'lin'} for linearized data in units of linear uncorrected strain, or 
#' \code{'raw'} for raw data in units of nonlinear counts. See [1] for
#' more information.
#' @param loc (unused)
#' 
#' @author A.J. Barbour.  
#' \code{bottlefile.py} and \code{bottlefile_merge.py} were
#' modified from those written by Jim Wright (previously at UNAVCO).
#' @export
#' 
#' @references [1] 
#' Barbour, A. J., and Agnew, D. C. (2011). 
#' Noise levels on plate boundary observatory borehole strainmeters in southern California. 
#' \emph{Bulletin of the Seismological Society of America}.
#' 101(5), 2453-2466. doi: 10.1785/0120110062
#' 
#' @return 
#' \code{\link{hfbsm}}: A list with information about the results, having class \code{'hfbsm.nfo'}.
#' 
#' @seealso \code{\link{strain-package}}
#' 
#' @examples
#' \dontrun{
#' # Download and assemble
#' # 1-Hz
#' res   <- hfbsm("B084", 2009, 215, "01:01:00", duration=1800, 1)
#' # 20-Hz (takes much longer)
#' res20 <- hfbsm("B084", 2009, 215, "01:01:00", duration=180, 20)
#' #
#' # Load data
#' dat <- load_hfbsm(res)
#' dat20 <- load_hfbsm(res20)
#' #
#' # Plot
#' plot(dat)
#' plot(dat20)
#' #
#' # Find out where the scripts live:
#' file.path(find.package('strain'), "hfbsm")
#' }
#' 
hfbsm <- function(sta, year, jday, ...) UseMethod("hfbsm")

#' @rdname hfbsm
#' @export
hfbsm.default <- function(sta, year, jday, st, duration, sampling=1, verbose=TRUE, ...){
  #
  POS <- function(y, jd, hms, delta=0, tz="UTC"){
    if (missing(hms)) hms <- "00:00:00"
    delta <- as.integer(delta)
    Dt <- as.POSIXct(strptime( paste(paste(y, jd, sep="-"), hms), format="%Y-%j %X", tz=tz)) + delta
    list(year = strftime(Dt, "%Y", tz=tz),
         jday = strftime(Dt, "%j", tz=tz),
         hms = strftime(Dt, "%X", tz=tz),
         oDt = Dt
    )
  }
  #
  stadat <- pborepo::station_data2(sta, use.regexp=TRUE)
  if (nrow(stadat)==0){
    stop("no stations found for: ", sta)
  } else if (nrow(stadat)>1){
    stop("multiple stations found for: ", sta)
  }
  sta4 <- as.character(stadat$sta4)
  stopifnot(!is.null(sta4))
  sta16 <- as.character(stadat$sta16)
  stopifnot(!is.null(sta16))
  #
  stopifnot(sampling==20 | sampling==1)
  #
  # the minus one accounts for the fact that
  # if the times given to 'hfbsm' script
  # are, say, "00:00:00" and "01:00:00" the result
  # will be _through_ hour 1, an end at 1:59:59.
  # this ensures (?) the end time will be "00:59:59" 
  # instead, as the user expects it to be
  min.dur <- ifelse(sampling==1, 3600, 60) - 1
  duration <- ifelse(missing(duration), min.dur, max(min.dur, duration - 1))
  if (missing(st)) st <- "00:00:00"
  Dt.st <- POS(year, jday, st, 0)
  Dt.en <- POS(year, jday, st, duration)
  #
  if (as.Date(Sys.time(), tz = "UTC") == as.Date(Dt.st[['oDt']], tz = "UTC")){
    msg <- c("!!! Strain data are imported once per day; hence, if you are looking for data for the present UTC day, the script likely failed.")
    on.exit(warning(msg, immediate. = FALSE))
  }
	#
	func <- "hfbsm" # the name of the script doing the assembly (which will call python code as needed)
	package.dir <- find.package('strain')
  hfbsm.dir <- pypath <- file.path(package.dir, func)
  script <- file.path(hfbsm.dir, func)
  
  pyver <- try(system(file.path(pypath,"version.py"), intern=TRUE, ignore.stderr=TRUE))
  if (inherits(pyver, "try-error")) stop( "python version-poke failed; check that python is installed" )
  
  #hfbsm Bnum 16-character-code start_year  start_day_of_yr  start_time  end_year end_day_of_year end_time [samp]
	#hfbsm B073 varian073bcs2006  2009 105 13:00:00 2009 105 16:00:00 [[1] pypath]
  TSTR <- function(Dt){sprintf("%s %s '%s'", Dt[['year']], Dt[['jday']], Dt[['hms']])}
  t.st <- TSTR(Dt.st)
  t.en <- TSTR(Dt.en)
  cmd <- sprintf("%s %s %s %s %s %i %s", script, sta4, sta16, t.st, t.en, sampling, pypath)
  if (is.null(cmd)) stop('hfbsm command generation failed')
  #
  # setup default (empty) results
  toret <- list(StationNames=list(sta4=NA, sta16=NA),
                DT=list(from=NA, to=NA),
                SamplingHz=NA,
                URLsrc=NA,
                files=list(rawfi=NA, linfi=NA))
  # run the command
  if (verbose) message(paste("assembling", sta4, "@", sampling, "Hz:", t.st, t.en, "(", duration, " sec )", "..."))
  results <- try(system(cmd, intern=TRUE))
  success <- !inherits(results, "try-error")
  #
  if (success){
    #
    stanames <- strsplit(results[1],"\t")[[1]]
    toret$StationNames$sta4 <- stanames[1]
    toret$StationNames$sta16 <- stanames[2]
    #
    dtf <- strsplit(results[2],"\t")[[1]]
    dtf <- dtf[3]
    toret$DT$from <- dtf
    dtt <- strsplit(results[3],"\t")[[1]]
    dtt <- dtt[3]
    toret$DT$to <- dtt
    #
    samp <- strsplit(results[4],"\t")[[1]]
    hz <- as.numeric(samp[3])
    toret$SamplingHz <- hz
    #
    # this is the length of results if only one file-url is returned
    nres <- 7 
    # ^^^ this will change if more echo content is returned from hfbsm (if it's changed)
    # this is the length of results as returned (can have more than one url)
    nreso <- length(results)
    # this is, then, the number of additional urls
    nd <- nreso - nres
    n <- 5
    m <- n + nd
    urls <- results[n:m]
    toret$URLsrc <- urls
    #
    n <- m + 1
    m <- n + 1
    fis <- results[n:m]
    # txt files (l then r)... alphabetical!
    toret$files$linfi <- fis[1]
    toret$files$rawfi <- fis[2]
  } else {
    if (verbose) warning( paste("hfbsm script failed:", cmd) )
  }
  #
  toret <- list(cmd=cmd, cmd.success=success, results=toret, python=pyver)
  class(toret) <- "hfbsm.nfo"
  return(toret)
}

#' @rdname hfbsm
#' @export
check_for_hfbsm <- function(sta4, starttime, endtime){
  sta4 <- as.character(sta4)
  starttime <- as.Date.POSIXlt(starttime)
  endtime <- as.Date.POSIXlt(endtime)
  
  query <- sprintf("http://service.iris.edu/irisws/availability/1/extent?network=PB&sta=%s&cha=BS*",sta4)
  #"http://service.iris.edu/irisws/availability/1/extent?network=PB&sta=AVN2&cha=BS*"
  G <- httr::GET(query)
  if (httr::http_error(G)){
    stat <- httr::http_status(G)
    stat[['query']] <- query
    print(stat)
    stop("failed query.")
  }
  readr::read_table(httr::content(G, encoding = "UTF-8")) %>%
  #[1] "#n"          "s"           "l"           "c"           "q"
  #[6] "sample-rate" "earliest"    "latest"      "updated"     "time-spans"
    dplyr::rename(net=`#n`, sta4=`s`, cha=`c`) -> restbl
  
  trng <- with(restbl, range(c(earliest, latest)))
  data.frame(Station=sta4, Start = starttime, End = endtime) %>%
      dplyr::mutate(Start.OK = (Start >= min(trng)) & (Start < max(trng)),
                    End.OK = (End > min(trng)) & (End <= max(trng))) -> avail
  return(avail)
}

#' @rdname hfbsm
#' @export
load_hfbsm <- function(object, ...) UseMethod("load_hfbsm")

#' @rdname hfbsm
#' @export
load_hfbsm.hfbsm.nfo <- function(object, file.type=c("lin","raw"), loc=".", stop.on.empty=TRUE, ...){
  #
  success <- object[["cmd.success"]]
  #
  cmd <- object[["cmd"]]
  res <- object[["results"]]
  stanames <- res[["StationNames"]]
  sta4 <- stanames$sta4
  sta16 <- stanames$sta16
  hz <- res[["SamplingHz"]]
  #
  otype <- match.arg(file.type)
  file.type <- paste0(otype,"fi")
  fis <- res[["files"]]
  fi <- fis[[file.type]]
  #
  naval <- 999999
  naval.c <- as.character(naval)
  nas <- switch(file.type, rawfi=naval.c, linfi=paste0(naval.c,".000000"))
  #
  op <- options(digits.secs=3)
  on.exit(options(op))
  #
  POS <- function(x, TZ="UTC"){
    #2009-08-03T01:30:21
    #2009-04-15T13:00:00.350000
    suppressWarnings(lubridate::ymd_hms(x))
  }
  #
  #print(file)
  dat <- read.table(fi, header=TRUE, 
                    colClasses=c("character", rep("numeric", 5)), 
                    comment.char="#", na.strings=nas)
  #
  if (nrow(dat) == 0){
    # the file exists, but is empty
    if (stop.on.empty){
      stop("file is empty")
    } else {
      dat[1,] <- naval
    }
  }
  #
  X <- dat==naval
  na.inds <- which(X, arr.ind=TRUE)
  dat[X] <- NA
  #
  toret <- list(srcdat=ts(dat[ , paste0("CH",0:3)], frequency=hz), 
                Datetime = POS(dat[ , 'Datetime']),
                RelInd = hz*dat[ , 'RelInd']
                )
  #
  #
  attr(toret, "sta4") <- sta4
  attr(toret, "file") <- fi
  attr(toret, "frequency") <- hz
  attr(toret, "cmd") <- cmd
  class(toret) <- c("hfbsm", otype)
  
  return(invisible(toret))
}

#' @rdname hfbsm
#' @aliases plot.hfbsm
#' @method plot hfbsm
#' @export
#' @param x an object of class \code{'hfbsm'}
#' @param sc numeric; a value to scale the strains by
#' @param main character; the plot title
#' @param xlab character; the x-axis label
#' @param note character; a note to place in the bottom left corner
#' @param v.markers numeric; dashed, red, vertical lines are drawn at these times
#' @param frame.plot logical; should boxes be drawn around each frame?
plot.hfbsm <- function(x, sc=1, main=NULL, xlab=NULL, note=NULL, v.markers=NULL, 
                       frame.plot=FALSE, ...){
  dat <-  sc*x$srcdat
  if (!is.ts(dat)) dat <- ts(dat, frequency=attr(x, "frequency"))
  if (is.null(main)) main <- attr(x, "file")
  if (is.null(xlab)) xlab <- paste("Time, seconds from", as.character(x$Datetime[1]))
  #
  my.ts.panel <- function(x, col, bg, pch, type, ...){
    lines(x, col = col, bg = bg, pch = pch, type = type, ...)
    if (!is.null(v.markers)) abline(v=as.numeric(v.markers), col="red", lty=2, lwd=1.5)
  }
  plot.ts(dat, main=main, xlab=xlab, frame.plot=frame.plot, 
          oma.multi = c(6, 1.2, 5, 1.2), las=1, panel=my.ts.panel, ...)
  #if (!is.null(v.markers)) abline(v=as.numeric(v.markers), col="red", lty=2, lwd=1.5)
  mtext(attr(x, "sta4"), adj=0, line=1)
  if (!is.null(note)) mtext(note, adj=0, side=1, line=2.7, cex=0.9, font=3)
}

#' @rdname hfbsm
#' @param new.location the new path to set for the source-data files in \code{object}; can be
#' any list-coercible object (i.e., with \code{\link{as.list}}) to specify multiple 
#' sub-directories while ensuring the path is constructed correctly
#' (i.e., with \code{\link{file.path}})
#' @export
update_file_location <- function(object, new.location, ...) UseMethod("update_file_location")

#' @rdname hfbsm
#' @export
update_file_location.hfbsm.nfo <- function(object, new.location, verbose=TRUE, ...){
  new.location <- do.call(file.path, as.list(new.location))
  fis <- object[['results']][['files']]
  rawfi <- fis[['rawfi']]
  linfi <- fis[['linfi']]
  if (verbose){
    message("old location(s):  ", unique(c(dirname(rawfi), dirname(linfi))))
    message("new location:     ", new.location)
  }
  object[['results']][['files']] <- list(rawfi = file.path(new.location, basename(rawfi)), 
                                         linfi = file.path(new.location, basename(linfi)))
  return(object)
}

