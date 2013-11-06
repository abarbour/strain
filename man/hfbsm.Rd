\name{hfbsm}
\alias{hfbsm}
\alias{hfbsm.default}
\alias{load_hfbsm.hfbsm.nfo}
\alias{plot.hfbsm}
\title{Assemble high-frequency strain data}
\usage{
  hfbsm(sta, year, jday, st = "00:00:00", duration,
    sampling = 1, verbose = TRUE, ...)

  \method{hfbsm}{default} (sta, year, jday,
    st = "00:00:00", duration, sampling = 1,
    verbose = TRUE, ...)

  \method{load_hfbsm}{hfbsm.nfo} (object,
    file.type = c("lin", "raw"), loc = ".", ...)

  \method{plot}{hfbsm} (x, sc = 1, main = NULL,
    xlab = NULL, note = NULL, v.markers = NULL,
    frame.plot = FALSE, ...)
}
\arguments{
  \item{sta}{character; the station code.  This can be
  either the 4-character code (e.g., \code{'B084'}) of the
  16-character code (e.g., \code{'pinyon084bcs2006'})
  because it uses \code{\link{station_data}} with
  \code{use.regexp=TRUE}}

  \item{year}{numeric; the year of the start time of the
  desired dataset}

  \item{jday}{numeric; the Julian day (day of year) of the
  start time}

  \item{st}{character; the hour:minute:second of the start
  time}

  \item{duration}{numeric; the relative end-time,
  represented as the number of seconds from the start
  time.}

  \item{sampling}{numeric; the sampling rate of the
  downloaded data}

  \item{verbose}{logical; should messages and warnings be
  given?}

  \item{...}{additional parameters}

  \item{object}{an object having class \code{'hfbsm.nfo'}}

  \item{file.type}{character; the type of strain data to
  load: \code{'lin'} for linearized data in units of linear
  uncorrected strain, or \code{'raw'} for raw data in units
  of nonlinear counts. See [1] for more information.}

  \item{loc}{(unused)}

  \item{x}{an object of class \code{'hfbsm'}}

  \item{sc}{numeric; a value to scale the strains by}

  \item{main}{character; the plot title}

  \item{xlab}{character; the x-axis label}

  \item{note}{character; a note to place in the bottom left
  corner}

  \item{v.markers}{numeric; dashed, red, vertical lines are
  drawn at these times}

  \item{frame.plot}{logical; should boxes be drawn around
  each frame?}
}
\value{
  \code{\link{hfbsm}}: A list with information about the
  results, having class \code{'hfbsm.nfo'}.
}
\description{
  Assemble high-frequency strain data
}
\details{
  This uses a set of scripts included with the package,
  namely \code{hfbsm}, \code{bottle.py},
  \code{bottle_merge.py} to create (from source files) high
  frequency PBO strain records for a station.
}
\author{
  A.J. Barbour. \code{bottle.py} and \code{bottle_merge.py}
  were modified from those written by J.Wright (UNAVCO).
}
\references{
  [1] Barbour, A. J., and Agnew, D. C. (2011). Noise levels
  on plate boundary observatory borehole strainmeters in
  southern California. \emph{Bulletin of the Seismological
  Society of America}. 101(5), 2453-2466. doi:
  10.1785/0120110062
}
\seealso{
  \code{\link{strain-package}}
}

