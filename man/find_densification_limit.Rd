\name{find_densification_limit}
\alias{find_densification_limit}

\title{
find_densification_limit
}
\description{
For porous (foam-like) materials, estimates the end of the plateau segment where the compression force starts to raise dramatically. In foams, this marks the end of pore space compression in favour of increasing wall material loading (Gibson&Ashby, 1997).
}
\usage{
find_densification_limit(theData,touch_point,elastic_limit,gel_thickness=NULL)
}

\arguments{
  \item{theData}{
Smoothened texture analyzer data, as produced by the \link{smooth_texture_analyzer_data} function; should contain only a one-way movement (down or up) for the chuck
}
  \item{touch_point}{
Point where the chuck touches the gel. This can be found from the Force - Distance diagram. where the force curve in a Force - Distance diagram raises abruptly
}
\item{elastic_limit}{Output of \link{find_elastic_limit}, which should be run before this function.}
\item{gel_thickness}{If an estimation of the overall gel thickness is known, this is used to generate a better initial guess of the densification limit.}
}
\details{
The function uses both the slope and the actual stress (force per area, pressure) to find the initial touch point, hence the \code{theData} argument must have at least the columns Distance, pressure and pressureSlope. The estimation is based on the idea to find the point where the measured pressure starts to rise substantially above the expected plateau pressure. Quantitatively, this is judged by comparing measured pressure to extrapolated plateau pressure (linear extrapolation based on minimal slope), with the requirement that the excess pressure exceeds the actual plateau pressure value.
}
\value{
Numerical value, indicating the position of the densification limit
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
densification_limit=find_densification_limit(sampleGelSmooth[sampleGelSmooth$direction=="down",],touch_point,elastic_limit)
plot(Force ~ Distance, sampleGelSmooth[sampleGelSmooth$direction=="down",]) 
lines(rep(touch_point,2),c(0,11),col="black")
lines(rep(elastic_limit,2),c(0,11),col="black")
lines(rep(densification_limit,2),c(0,11),col="red")
text(touch_point,11.5,labels="Touch point",adj=0.5)
text(elastic_limit,11.5,labels="Elastic limit",adj=0.5)
text(densification_limit,11.5,labels="Densification limit",adj=0.5,col="red")

}

\keyword{ misc }

\references{Gibson, L. J. & Ashby, M. F. Cellular Solids: Structure and properties. (Cambridge University Press, 1997).}

