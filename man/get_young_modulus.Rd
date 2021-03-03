\name{get_young_modulus}
\alias{get_young_modulus}

\title{
get_young_modulus
}
\description{
Estimates the Young modulus from force-distance curves obtained by a compression cycle
}
\usage{
get_young_modulus(theData, approximate_gel_touch_point = NULL, gel_thickness = 1, strain = 0.1, doplot = FALSE, do_touch_point_estimation=TRUE,subtract_chuck_buoyancy=FALSE)
}

\arguments{
  \item{theData}{
Force-Distance data, typically from a "down"-"up" cycle of the chuck; the data needs to be smooth and contain already the ForceSlope column, which can be obtained by \link{smooth_texture_analyzer_data}
}
  \item{approximate_gel_touch_point}{
Initial guess for the gel touch point (will be refined by a call to \link{find_landmarks_gel_compression})}
  \item{gel_thickness}{
Gel thickness, in the same units as is the "Distance
}
  \item{strain}{
Compressive strain on which the evaluation of the Young modulus should be centered
}
  \item{doplot}{
Whether or not the routine should provide graphical output (regression lines in the pressure as a function of distance graph)
}
\item{do_touch_point_estimation}{Whether or not the touch point should be estimated from the data. If \code{do_touch_point_estimation=FALSE}, \code{approximate_gel_touch_point} needs to be provided and will be used as is for the gel touch point}
\item{subtract_chuck_buoyancy}{If TRUE, subtract the effect of the chuck buoyancy in water (this assumes the density of the medium to be 1000kg/m^3, and the distance to be in mm, and also that the chuck has uniform cross section area identical to sample cross section area.}
}

\value{
Vector of young moduli, one per direction (up or down)
}

\details{The Young modulus estimation here is by attempting to evaluate the slope of the stress-strain curve (ATSM E111, via \link{lm}) up to a given maximum \code{strain} value. No correction for possible non-linearity is attempted. If significant non-linearity exists, non-linear models such as the Ogden model (Ogden 1972, \link{ogden_model}), or, in the case of the existence of a plateau (Gibson 1997), \link{foam_mechanical_analysis} should be used instead.}

\author{
Thomas Braschler
}

\seealso{
\link{find_initial_touch_point}, used internally
}
\examples{
data(sampleGelSmooth)
get_young_modulus(sampleGelSmooth)

}

\keyword{ misc }

\references{
ATSM standard E111: https://www.astm.org/Standards/E111.htm\cr\cr
Gibson, L. J. & Ashby, M. F. Cellular Solids: Structure and properties. (Cambridge University Press, 1997). \cr\cr
Ogden, R. W. (1972), Large Deformation Isotropic Elasticity - On the Correlation of Theory and Experiment for Incompressible Rubberlike Solids,Proceedings of theRoyal Society A: Mathematical, Physical and Engineering Sciences, 326(1567), 565-584.

}

