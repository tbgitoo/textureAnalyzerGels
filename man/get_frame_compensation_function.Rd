\name{get_frame_compensation_function}
\alias{get_frame_compensation_function}
\title{get_frame_compensation_function
}
\description{Provides a compensation function that allows to estimate how much distance is lost at a given force due to TextureAnalyzer compliance. Relevant only for very hard subtrates, for hydrogels, this is typically negligeable.
}
\usage{
get_frame_compensation_function(filename)
}
\arguments{
  \item{filename}{ Texture analyzer tab file containing compression data without sample, solely against the bench or other holder surface}
}
\value{
A numerical vector of the same length as xout}
\author{Thomas+Marina Braschler}
\examples{
xout=seq(from=0,to=6,length.out=500)
x=1:5
y=runif(length(x))
sd=2
smoothed=get_frame_compensation_function(x=x,y=y,sd=sd,xout=xout)
plot(x=x,y=y,type="p")
plot.xy(xy.coords(x=xout,y=smoothed),type="l",col="red")
}
\keyword{ misc }

\references{Filtering in the Time and Frequency Domains, Herman J. Blinchikoff, Anatol I. Zverev, Wiley, 1976}





