#' Minimum phase filtering
#' @description
#' Desc
#' @details
#' Det
#' @name strain-filtering
#' @aliases minfilt
#' @param y numeric
#' @param scenario character
#' @param verbose logical; should messages be given?
#' @references
#' D.C. Agnew and K. Hodgkinson (2007),
#' Designing compact causal digital filters for low-frequency strainmeter data,
#' Bulletin of the Seismological Society of America,
#' vol 97, 1B, 1-99, doi: 10.1785/0120060088
#' @seealso 
#' \code{\link{decimate}},
#' \code{\link{lpfilter}},
#' \code{\link{minphs}},
#' \code{\link{strain-package}}
NULL

#' @rdname strain-filtering
#' @export
minfilt <- function(y, scenario=c("ones_to_fivem","fivem_to_onehr"), verbose=TRUE) UseMethod("minfilt")
#' @rdname strain-filtering
#' @method minfilt default
#' @S3method minfilt default
minfilt.default <- function(y, scenario=c("ones_to_fivem","fivem_to_onehr"), verbose=TRUE){
  y <- as.vector(y)
  scen <- match.arg(scenario)
  #
  data("minphs", envir=strain:::.strnEnv) 
  minphs <- strain:::.strnEnv$minphs
  by2 <- minphs$by2
  by3 <- minphs$by3
  by5 <- minphs$by5
  #
  # Sets the minphs list indices, and the decimation factors
  # todo: Add ndec attr to minphs list components (?!)
  decs <- switch(scen, 
         ones_to_fivem=list(inds=c(1,1,2,3,3), ndecs=c(2,2,3,5,5)),
         fivem_to_onehr=list(inds=c(1,1,2), ndecs=c(2,2,3)))
  ndecs <- decs$ndecs
  decseq <- seq_along(decs$inds)
  tt <- try(
    for (n in decseq){
      ni <- decs$inds[n]
      ndec <- ndecs[n]
      if (verbose) message(sprintf("stage %i: by %i",n, ndec))
      y <- lpfilter(y, minphs[[ni]], ndec)
    }
  )
  if (class(tt)=="try-error") n <- n - 1
  attr(y,"ndec") <- ndecs[1:n]
  return(y)
}

#' Straight decimation
#' @description Performs a simple, straight decimation.
#' @details
#' This is to be used with a proper low-pass filtering algorithm to
#' reduce data volume without introducing aliasing effects, or to
#' decimate an index series (e.g. for plotting, etc.).
#' 
#' \emph{No filtering is applied, so this will introduce aliasing if
#' used improperly.}
#'  
#' @param y numeric
#' @param ndec integer; the decimation factor
#' @aliases decim
#' @export
#' @seealso \code{\link{lpfilter}}, \code{\link{strain-filtering}}
decimate <- function(y, ndec=1) UseMethod("decimate")
#' @rdname decimate
#' @method decimate ts
#' @S3method decimate ts
decimate.ts <- function(y, ndec=1){
  stopifnot(ndec>0)
  ndec <- as.integer(ndec)
  yf <- frequency(y)/ndec
  y <- decimate(as.vector(y), ndec)
  if (ndec >= 1) y <- ts(y, frequency=yf)
  attr(y,"ndec") <- ndec
  return(y)
}
#' @rdname decimate
#' @method decimate default
#' @S3method decimate default
decimate.default <- function(y, ndec=1){
  stopifnot(ndec>0)
  ndec <- as.integer(ndec)
  if (ndec>1){
    y <- as.vector(y)
    inds <- seq_along(y) %% ndec == 1
    y <- y[inds]
  }
  attr(y,"ndec") <- ndec
  return(y)
}

#' Causal lowpass filtering with decimation
#' @details Applies a single-sided convolution filter with the filter
#' weights (\code{wgts}) to the data (\code{y}).  The decimation factor
#' is set with \code{ndec} but is only applied when \code{ndec>1}.
#' @export
#' @aliases lpdfilter
#' @param y numeric
#' @param wgts numeric; convolution filter weights
#' @param ndec integer; the decimation factor
#' @seealso \code{\link{decimate}}, \code{\link{filter}}, \code{\link{strain-filtering}}
lpfilter <- function(y, wgts, ndec=1){
  y <- as.vector(y)
  #print(c(length(y),length(wgts)))
  y <- stats::filter(y, wgts, method="convolution", side=1)
  return(decimate(y, ndec))
}

