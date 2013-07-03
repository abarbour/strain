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
#' @references Grant, E. B. (2010),
#' Gladwin Tensor Strainmeter Calibration Using Seismic Data: 
#' Instrument Calibration Methods and Wave Gradiometry Applications,
#' PhD thesis, University of Memphis.
#' 
#' @references Hart, R. H. G., M. T. Gladwin, R. L. Gwyther, D. C. Agnew, and F. K. Wyatt (1996), 
#' Tidal calibration of borehole strain meters: Removing the effects of small-scale inhomogeneity, 
#' J. Geophys. Res., 101(B11), 25553–25571, doi:10.1029/96JB02273.
#' 
#' @references Hodgkinson, K., J. Langbein, B. Henderson, D. Mencin, and A. Borsa (2013), 
#' Tidal calibration of plate boundary observatory borehole strainmeters, 
#' J. Geophys. Res. Solid Earth, 118, 447–458, doi:10.1029/2012JB009651.
#' 
#' @references Langbein, J. (2010), 
#' Effect of error in theoretical Earth tide on calibration of borehole strainmeters, 
#' Geophys. Res. Lett., 37, L21303, doi:10.1029/2010GL044454.
#' 
#' @references Roeloffs, E. (2010), 
#' Tidal calibration of Plate Boundary Observatory borehole strainmeters: 
#' Roles of vertical and shear coupling, 
#' J. Geophys. Res., 115, B06405, doi:10.1029/2009JB006407.
#' 
# @references some ref
# \url{http://biomet.oxfordjournals.org/content/82/1/201}
#'
#' @seealso \code{\link{bsm-methods}}
#'
NULL
.default.caltbl = "pbou" # default calibration table
.strainEnvName = ".strnEnv"
.strnEnv = new.env()
.constants = list(
  deg2rad=pi/180,
  rad2deg=180/pi,
  bsm.R=1e8,
  bsm.diam=87e-3,
  bsm.gaps=c(100,200)*1e-6,
  bsm.relative_orientations=c(0,-60,60,30), # clockwise (convention?)
  bsm.gauge_names=list(pbo=c(0,1,2,3), 
                       hodg=c(1,2,3,4), 
                       ext=c(2,1,0,3))
)

##
## Datasets
## 
#
##   calibration coeffs:
#
#' @title Roeloffs (2010) tidal calibration coefficients
#' @name roel10
#' @docType data
#' @format A list with dataframes with calibration coefficients by station
NULL

#' @title PBO (un) official tidal calibration coefficients
#' @name pbou
#' @docType data
#' @format A list with dataframes with calibration coefficients by station
NULL

#' @title Hodgkinson et al (2013) tidal calibration coefficients
#' @name hodg13
#' @docType data
#' @format A list with dataframes with calibration coefficients by station
NULL

#' @title Grant (2010) surface-wave calibration coefficients
#' @name grant10
#' @docType data
#' @format A list with dataframes with calibration coefficients by station
NULL
