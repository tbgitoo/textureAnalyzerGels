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
\item{subtract_chuck_buoyancy}{If TRUE, subtract the effect of the chuck buoyancy in water (this assumes the density of the medium to be 1000kg/m^3, and the distance to be in mm}
}

\value{
Vector of young moduli, one per direction (up or down)
}

\author{
Thomas Braschler
}

\seealso{
\link{find_landmarks_gel_compression}, used internally
}
\examples{
data(sampleGelSmooth)
get_young_modulus(sampleGelSmooth)

}

\keyword{ misc }

