\name{lpfilter}
\alias{lpdfilter}
\alias{lpfilter}
\title{Causal lowpass filtering with decimation}
\usage{
  lpfilter(y, wgts, ndec = 1)
}
\arguments{
  \item{y}{numeric}

  \item{wgts}{numeric; convolution filter weights}

  \item{ndec}{integer; the decimation factor}
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

