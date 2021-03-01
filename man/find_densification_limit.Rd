\name{find_densification_limit}
\alias{find_densification_limit}

\title{
find_densification_limit
}
\description{
Estimates the end of the plateau segment where the compression force starts to raise dramatically. In foams, this marks the end of pore space compression in favour of increasing wall material loading (Gibson&Ashby, 1997).
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
find_densification_limit(sampleGelSmooth[sampleGelSmooth$direction=="down",])
}

\keyword{ misc }

\references{Gibson, L. J. & Ashby, M. F. Cellular Solids: Structure and properties. (Cambridge University Press, 1997).}

