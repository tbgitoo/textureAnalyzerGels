\name{find_elastic_limit}
\alias{find_elastic_limit}

\title{
find_elastic_limit
}
\description{
Estimates the end of the linear elastic segment where the compression force starts to plateau. In prototypical foams, this marks the transition from largely undeformed linear compression to wall buckling (Gibson&Ashby, 1997).
}
\usage{
find_elastic_limit(theData,touch_point,gel_thickness=NULL)
}

\arguments{
  \item{theData}{
Smoothened texture analyzer data, as produced by the \link{smooth_texture_analyzer_data} function; should contain only a one-way movement (down or up) for the chuck
}
  \item{touch_point}{
Point where the chuck touches the gel. This can be found from the Force - Distance diagram. where the force curve in a Force - Distance diagram raises abruptly
}
\item{gel_thickness}{Optionally, thickness of the gel to help make an educated guess of the extent of the elastic segment (20\% of the gel height).}
}
\details{
The function uses both the slope and the actual force to find the initial touch point, hence the \code{theData} argument must have at least the columns Distance, pressure and pressureSlope
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
elastic_limit = find_elastic_limit(sampleGelSmooth,touch_point)
plot(Force ~ Distance, sampleGelSmooth[sampleGelSmooth$direction=="down",]) 
lines(rep(touch_point,2),c(0,11),col="black")
lines(rep(elastic_limit,2),c(0,11),col="red")
text(touch_point,11.5,labels="Touch point",adj=0.5)
text(elastic_limit,11.5,labels="Elastic limit",adj=0.5,col="red")


}

\keyword{ misc }

\references{Gibson, L. J. & Ashby, M. F. Cellular Solids: Structure and properties. (Cambridge University Press, 1997).}

