#' @title Tools to work with geodetic strainmeter data.
#'
#' @description
#' Some description
#'
#' @details
#' Some details
#'
#' @docType package
#' @name strain-package
#' @aliases strain, strainmeter
#' 
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' 
#' @import Matrix corpcor
# not needed since specified in DESCRIPTION-DEPENDS: stats utils graphics grDevices
# @useDynLib strain
#'
# @references some ref
# \url{http://biomet.oxfordjournals.org/content/82/1/201}
#'
#' @seealso \code{\link{bsm-methods}}
#'
NULL
.default.caltbl = "bsm.pbou" # default calibration table
.strainEnvName = ".strnEnv"
.strnEnv = new.env()
.constants = list(
  deg2rad=pi/180,
  rad2deg=180/pi,
  bsm.R=1e8,
  bsm.diam=87e-3,
  bsm.gaps=c(100,200)*1e-6,
  bsm.channels=0:3,
  bsm.relative_orientations=c(0,-60,30,60)
)

#
# Datasets
#

#' @title Roeloffs tidal calibration coefficients (I)
#' @name bsm.roel1
#' @docType data
#' @format A dataframe with calibration coefficients by station
#bsm.roel1.rda
#class(caltbl) <- "cal.roel1"
NULL

#' @title Roeloffs tidal calibration coefficients (II)
#' @name bsm.roel2
#' @docType data
#' @format A dataframe with calibration coefficients by station
# bsm.roel2.rda
# class(caltbl) <- "cal.roel2"
NULL

#' @title PBO (unofficial) calibration coefficients
#' @name bsm.pbou
#' @docType data
#' @format A dataframe with calibration coefficients by station
# bsm.pbo1.rda
# class(caltbl) <- "cal.pbou"
NULL

#' @title Grant surface-wave calibration coefficients (I)
#' @name bsm.surfwave1
#' @docType data
#' @format A dataframe with calibration coefficients by station
# bsm.surfwave1.rda
# class(caltbl) <- "cal.surf1"
NULL

#' @title Grant surface-wave calibration coefficients (II)
#' @name bsm.surfwave2
#' @docType data
#' @format A dataframe with calibration coefficients by station
# bsm.surfwave2.rda
# class(caltbl) <- "cal.surf2"
