\name{gaussianSmoothingSlope}
\alias{gaussianSmoothingSlope}
\title{gaussianSmoothingSlope
}
\description{Slope of a curve smoothed by a gaussian filter
}
\usage{
gaussianSmoothingSlope(x,y,sd=1,xout=x,...)
}
\arguments{
  \item{x}{ The x values formatted as a vector
}
  \item{y}{ The y values, formatted as a vector of length identical to the length of \code{x}
}
\item{sd}{Standard deviation for the smoothing, in terms of x-coordinates}
\item{xout}{The x values where an output is desired}
\item{...}{Supplementary arguments to be passed to \link{gaussianSmoothing}, used internally}

 

}
\value{
A numerical vector of the same length as \code{xout}, giving the estimated slope at each point given in \code{xout}}
\author{Thomas+Marina Braschler}
\examples{

# First, without boundary extension. In theory, the smoothened curve should rise linearly, and the slope be constant at 1/100 (=10 when multiplied with 1000)

x=seq(from=0,to=1500,by=0.25)
xout=x
y=runif(length(x))+x/100
sd=50
smoothed=gaussianSmoothing(x=x,y=y,sd=sd,xout=xout)
slope=gaussianSmoothingSlope(x=x,y=y,sd=sd,xout=xout)
plot(x=x,y=y,type="p")
plot.xy(xy.coords(x=xout,y=smoothed),type="l",col="red")
plot.xy(xy.coords(x=xout,y=slope*1000),type="l",col="green")

# In particular the slope in the above example nicely shows the edge problem. This can be first-order corrected with linear boundary extension:


x=seq(from=0,to=1500,by=0.25)
xout=x
y=runif(length(x))+x/100
sd=50
boundary_extension=500
smoothed=gaussianSmoothing(x=x,y=y,sd=sd,xout=xout,boundary_extension=boundary_extension)
slope=gaussianSmoothingSlope(x=x,y=y,sd=sd,xout=xout,boundary_extension=boundary_extension)
plot(x=x,y=y,type="p")
plot.xy(xy.coords(x=xout,y=smoothed),type="l",col="red")
plot.xy(xy.coords(x=xout,y=slope*1000),type="l",col="green")





}
\keyword{ misc }





