\name{find_landmarks_gel_compression}
\alias{find_landmarks_gel_compression}
\title{
find_landmarks_gel_compression
}
\description{
Finds landmark points in as porous gel compression curve (Force - Distance curve). Currently, detection of the gel touch point in both the "down" and "up" cycle of the chuck is implemented. The analysis is based on the general curve for foams, comprising an initial steep elastic segment, followed by a plateau and then a densification region (Gibson &Ashby, 1997).
}
\usage{
find_landmarks_gel_compression(theData, approximate_gel_touch_point = NULL,gel_thickness=NULL)
}

\arguments{
  \item{theData}{
  Force-Distance data, typically from a "down"-"up" cycle of the chuck; the data needs to be smooth and contain already the ForceSlope column, which can be obtained by \link{smooth_texture_analyzer_data}
}
  \item{approximate_gel_touch_point}{
Initial guess of the gel touch point
}
\item{gel_thickness}{Macroscopic gel thickness estimation, can be used to improve initial guess for the densification limit (via \link{find_densification_limit})}
}
\value{
Vector of estimated touch points, one for the down movement, one for the up movement
}
\details{
The function makes use of \link{find_initial_touch_point} for each movement cycle
}

\author{
Thomas Braschler
}


\seealso{
\code{\link{smooth_texture_analyzer_data}} \cr\cr
And the functions used internally:
\code{\link{find_initial_touch_point}} \cr
\code{\link{find_elastic_limit}} \cr
\code{\link{find_densification_limit}}
}
\examples{
data(sampleGelSmooth)
find_landmarks_gel_compression(sampleGelSmooth)
}
\keyword{ misc }
\references{Gibson, L. J. & Ashby, M. F. Cellular Solids: Structure and properties. (Cambridge University Press, 1997).}

