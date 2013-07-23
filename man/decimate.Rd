\name{decimate}
\alias{decim}
\alias{decimate}
\title{Straight decimation}
\usage{
  decimate(y, ndec = 1)
}
\arguments{
  \item{y}{numeric}

  \item{ndec}{integer; the decimation factor}
}
\description{
  Performs a simple, straight decimation.
}
\details{
  This is to be used with a proper low-pass filtering
  algorithm to reduce data volume without introducing
  aliasing effects, or to decimate an index series (e.g.
  for plotting, etc.). \emph{No filtering is applied.}
}
\seealso{
  \code{\link{lpfilter}}, \code{\link{strain-filtering}}
}

