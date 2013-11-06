#' @title Tools to work with geodetic strain data.
#'
#' @description
#' Some description
#'
#' @details
#' See the following pages for details on
#' 
#' @section Filtering:
#' \code{\link{strain-filtering}} is a good place to start to learn about
#' filtration tools.
#'
#' @docType package
#' @name strain-package
#' @aliases strain strainmeter
#' 
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' 
#' @import Matrix corpcor lubridate pborepo zoo
# not needed since specified in DESCRIPTION-DEPENDS: stats utils graphics grDevices
# @useDynLib strain
#'
#' @references Grant, E. B. (2010),
#' Gladwin Tensor Strainmeter Calibration Using Seismic Data: 
#' Instrument Calibration Methods and Wave Gradiometry Applications,
#' PhD thesis, University of Memphis.
#' 
#' @references Hart, R. H. G., M. T. Gladwin, R. L. Gwyther, D. C. Agnew, and F. K. Wyatt (1996), 
#' Tidal calibration of borehole strain meters: 
#'  Removing the effects of small-scale inhomogeneity, 
#' J. Geophys. Res., 101(B11), 25553-25571, doi:10.1029/96JB02273.
#' 
#' @references Hodgkinson, K., J. Langbein, B. Henderson, D. Mencin, and A. Borsa (2013), 
#' Tidal calibration of plate boundary observatory borehole strainmeters, 
#' J. Geophys. Res. Solid Earth, 118, 447-458, doi:10.1029/2012JB009651.
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

#' Constants used as defaults
#' 
#' @details The helper function \code{\link{constants}}
#' shows (the structure of, optionally)
#' and returns \code{.constants}.
#'
#' The following constants are assumed
#' \describe{
#' \item{Poisson's ratios}{ (Poisson solid)}
#' }
#' @name strain-constants
#' @seealso \code{\link{strain}}
.constants = list(
  Poisson=list(
    nu=0.25,      #for a Poisson solid
    nu_u=1/3,
    VpVs=sqrt(3) #for a Poisson solid
  ),
  conversions=list(
    to_radians=pi/180,
    from_radians=180/pi
  ),
  bsm=list(R=1e8,
           diam=87e-3,
           gaps=c(100,200)*1e-6,
           relative_orientations=c(0,-60,60,30), # clockwise (convention?)
           gauge_names=list(pbo=c(0,1,2,3),
                            hodg=c(1,2,3,4),
                            ext=c(2,1,0,3)
                            )
  )
)
#' @rdname strain-constants
#' @param do.str logical; should the structure be printed?
#' @export
# @example
# constants()
constants <- function(do.str=TRUE){
  const <- strain:::.constants
  if (do.str) str(const, comp.str = "++++++++\n\t", no.list=TRUE, digits.d = 9)
  return(invisible(const))
}

##
## Datasets
## 
##   Filter weights
#
#' @title Minimum phase lowpass filter weights for strain data
#' @references
#' D.C. Agnew and K. Hodgkinson (2007),
#' Designing compact causal digital filters for low-frequency strainmeter data,
#' Bulletin of the Seismological Society of America,
#' vol 97, 1B, 1-99, doi: 10.1785/0120060088
#' @name minphs
#' @docType data
#' @format A list with filter weights by decimation factor
NULL

##   calibration coeffs:
#
#' @title Roeloffs (2010) tidal calibration coefficients
#' @name roel10
#' @docType data
#' @format A list with dataframes with calibration coefficients by station
NULL

#' @title PBO (un)official tidal calibration coefficients
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
