\name{find_landmarks_gel_compression}
\alias{find_landmarks_gel_compression}
\title{
find_landmarks_gel_compression
}
\description{
Finds landmark points in the gel compression curve (Force - Distance curve). Currently, detection of the gel touch point in both the "down" and "up" cycle of the chuck is implemented
}
\usage{
find_landmarks_gel_compression(theData, approximate_gel_touch_point = NULL)
}

\arguments{
  \item{theData}{
  Force-Distance data, typically from a "down"-"up" cycle of the chuck; the data needs to be smooth and contain already the ForceSlope column, which can be obtained by \link{smooth_texture_analyzer_data}
}
  \item{approximate_gel_touch_point}{
Initial guess of the gel touch point
}
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
\code{\link{smooth_texture_analyzer_data}} \cr
\code{\link{find_initial_touch_point}}
}
\examples{
data(sampleGelSmooth)
find_landmarks_gel_compression(sampleGelSmooth)
}
\keyword{ misc }

