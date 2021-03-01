\name{gaussianSmoothingSlopeBlock}
\alias{gaussianSmoothingSlopeBlock}
\title{gaussianSmoothingSlopeBlock
}
\description{Slope of a curve smoothed by a gaussian filter, done blockwise to reduce calculation cost
}
\usage{
gaussianSmoothingSlopeBlock(x,y,sd=1,xout=NULL,block_size=3,boundary_extension=0,lm_region_lower=boundary_extension,lm_region_upper=boundary_extension,...)
}
\arguments{
  \item{x}{ The x values formatted as a vector
}
  \item{y}{ The y values, formatted as a vector of length identical to the length of \code{x}
}
\item{sd}{Standard deviation for the smoothing, in terms of x-coordinates}
\item{xout}{The x values where an output is desired. If no value is provided, x is used for xout}
\item{block_size}{To speed up calculation, use only neighbouring values in the smoothing calculation. \code{block_size} indicates how many standard deviations away the \code{x} values are allowed to be from the \code{xout} value under consideration to be taken into account in the calculation}
\item{boundary_extension}{Number of elements to linearly extrapolate at the boundaries, to avoid edge effects. Default is 0, meaning no extension}
\item{lm_region_lower}{Indicates the number of elements on the initial portion of the curve that should be used to set the slope for extrapolation. In more detail, the extrapolation (if \code{boundary_extension}>0) is performed by linear extension of both the \code{x} and \code{y} values by \code{N=boundary_extension} elements. For this a linear model (\link{lm}) is calibrated on the first \code{N=lm_region_lower} elements of \code{x} or \code{y} and their indices as a regressor, and then used to extend \code{x} and \code{y} using continuation of the indices to the negative side.  }
\item{lm_region_upper}{Indicates the number of elements on the final portion of the curve that should be used to set the slope for extrapolation. In more detail, the extrapolation (if \code{boundary_extension}>0) is performed by linear extension of both the \code{x} and \code{y} values by \code{N=boundary_extension} elements. For this a linear model (\link{lm}) is calibrated on the last \code{N=lm_region_upper} elements of \code{x} or \code{y} and their indices as a regressor, and then used to extend \code{x} and \code{y} using continuation of the indices to the negative side.  }

\item{...}{Additional arguments passed to \link{gaussianSmoothing}, used internally}

 

}
\value{
A numerical vector of the same length as \code{xout}, giving the estimated slope at each point given in \code{xout}}
\author{Thomas+Marina Braschler}
\examples{

x=seq(from=0,to=1500,by=0.25)
xout=x
y=runif(length(x))+x/100
sd=50
smoothed=gaussianSmoothing(x=x,y=y,sd=sd,xout=xout)
slope=gaussianSmoothingSlope(x=x,y=y,sd=sd,xout=xout)
slopeBlock=gaussianSmoothingSlopeBlock(x=x,y=y,sd=sd,xout=xout,block_size=5)
plot(x=x,y=y,type="p")
plot.xy(xy.coords(x=xout,y=smoothed),type="l",col="red")
plot.xy(xy.coords(x=xout,y=slope*1000),type="l",col="green")
plot.xy(xy.coords(x=xout,y=slopeBlock*1000),type="l",col="blue")

# In particular the slope in the above example nicely shows the edge problem. This can be first-order corrected with linear boundary extension:


x=seq(from=0,to=1500,by=0.25)
xout=x
y=runif(length(x))+x/100
sd=50
boundary_extension=500
smoothed=gaussianSmoothing(x=x,y=y,sd=sd,xout=xout,boundary_extension=boundary_extension)
slope=gaussianSmoothingSlope(x=x,y=y,sd=sd,xout=xout,boundary_extension=boundary_extension)
slopeBlock=gaussianSmoothingSlopeBlock(x=x,y=y,sd=sd,xout=xout,boundary_extension=boundary_extension,block_size=5)
plot(x=x,y=y,type="p")
plot.xy(xy.coords(x=xout,y=smoothed),type="l",col="red")
plot.xy(xy.coords(x=xout,y=slope*1000),type="l",col="green")
plot.xy(xy.coords(x=xout,y=slopeBlock*1000),type="l",col="blue")


}
\keyword{ misc }





