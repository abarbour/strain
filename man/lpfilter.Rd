\name{lpfilter}
\alias{lpdfilter}
\alias{lpfilter}
\alias{lpfilter.dec}
\title{Causal lowpass filtering with decimation}
\usage{
  lpfilter(y, wgts, ndec = 1)

  lpfilter.dec(y, frq = 1, ndecs = c(2, 5), plt = FALSE,
    ...)
}
\arguments{
  \item{y}{numeric}

  \item{wgts}{numeric; convolution filter weights}

  \item{ndec}{integer; the decimation factor}

  \item{frq}{integer; the sampling frequency of \code{y}}

  \item{plt}{logical; should the decimation stages be
  plotted?}

  \item{...}{additional parameters to \code{\link{ts}} in
  the case \code{y} does not have class \code{'ts'}.}
}
\description{
  Causal lowpass filtering with decimation
}
\details{
  Applies a single-sided convolution filter with the filter
  weights (\code{wgts}) to the data (\code{y}).  The
  decimation factor is set with \code{ndec} but is only
  applied when \code{ndec>1}.
}
\seealso{
  \code{\link{decimate}}, \code{\link{filter}},
  \code{\link{strain-filtering}}
}

