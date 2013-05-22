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
#' @aliases strain, strains, strainmeter, straindata
#' 
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' 
#' @import Matrix
# not needed since specified in DESCRIPTION-DEPENDS: stats utils graphics grDevices
# @useDynLib strain
#'
# @references some ref
# \url{http://biomet.oxfordjournals.org/content/82/1/201}
#'
#' @seealso \code{\link{bsm}}
#'
NULL
.strainEnvName = ".strnEnv"
.strnEnv = new.env()
.constants = list(
  deg2rad=pi/180,
  rad2deg=180/pi
)