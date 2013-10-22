\name{hfbsm}
\alias{hfbsm}
\alias{hfbsm.default}
\alias{load_hfbsm.hfbsm.nfo}
\title{Assemble high-frequency strain data}
\usage{
  hfbsm(sta4, yst, dst, st = "00:00:00", yen, den,
    en = "00:00:00", sampling = 1, ...)

  \method{hfbsm}{default} (sta4, yst, dst, st = "00:00:00",
    yen, den, en = "00:01:00", sampling = 1, ...)

  \method{load_hfbsm}{hfbsm.nfo} (object,
    file.type = c("lin", "raw"), loc = ".", ...)
}
\arguments{
  \item{sta4}{}

  \item{sta16}{}

  \item{yst}{}

  \item{dst}{}

  \item{st}{}

  \item{yen}{}

  \item{den}{}

  \item{en}{}

  \item{sampling}{}

  \item{...}{}
}
\description{
  Assemble high-frequency strain data
}
\details{
  This uses a set of scripts included with the package,
  namely \code{hfbsm}, \code{bottle.py},
  \code{bottle_merge.py}
}

