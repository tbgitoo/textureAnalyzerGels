\name{find_initial_touch_point}
\alias{find_initial_touch_point}

\title{
find_initial_touch_point
}
\description{
Estimates the point where the chuck touches the gel for the first time
}
\usage{
find_initial_touch_point(theData, approximate_touch_point = NULL,free_region=NULL)
}

\arguments{
  \item{theData}{
Smoothened texture analyzer data, as produced by the \link{smooth_texture_analyzer_data} function; should contain only a one-way movement (down or up) for the chuck
}
  \item{approximate_touch_point}{
In initial guess of the touch point. If necessary, this can be obtained visually by finding the point where the force curve in a Force - Distance diagram raises abruptly
}
\item{free_region}{Below this distance, the gel does not touch the compression piston, such that only buoyancy effects are observed}
}
\details{
The function uses both the slope and the actual force to find the initial touch point, hence the \code{theData} argument must have at least the columns Distance, Force and ForceSlope
}
\value{
Numerical value, indicating the position of the surface of the gel
}

\author{
Thomas Braschler
}



\seealso{
\code{\link{smooth_texture_analyzer_data}}
}
\examples{
data(sampleGelSmooth)
touch_point=find_initial_touch_point(sampleGelSmooth[sampleGelSmooth$direction=="down",])
plot(Force ~ Distance, sampleGelSmooth[sampleGelSmooth$direction=="down",]) 
lines(rep(touch_point,2),c(0,11),col="red")
text(touch_point,11.5,labels="Touch point",adj=0.5,col="red")

}

\keyword{ misc }

