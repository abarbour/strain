\name{decimate}
\alias{decim}
\alias{decimate}
\alias{decimate.default}
\alias{decimate.ts}
\title{Straight decimation}
\usage{
decimate(y, ndec = 1, ...)

\method{decimate}{ts}(y, ndec = 1, ...)

\method{decimate}{default}(y, ndec = 1, ...)
}
\arguments{
  \item{y}{numeric}

  \item{ndec}{integer; the decimation factor}

  \item{...}{additional parameters; Used only for
  \code{\link{ts}} method.}
}
\description{
Performs a simple, straight decimation.
}
\details{
This is to be used with a proper low-pass filtering
algorithm to reduce data volume without introducing
aliasing effects, or to decimate an index series (e.g. for
plotting, etc.).

\emph{No filtering is applied, so this will introduce
aliasing if used improperly.}

The \code{\link{ts}} method uses \code{\link{window}}
}
\seealso{
\code{\link{lpfilter}}, \code{\link{strain-filtering}}
}

