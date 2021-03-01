\name{gaussianSmoothing}
\alias{gaussianSmoothing}
\title{gaussianSmoothing
}
\description{Smoothes a curve using a gaussian filter. Using default values, produces classical gaussian smoothing (Blinchikoff and Zverev 1976), but permits additional flexibility to specify the desired output points and specficic handling of the edges to reduce edge artefacts.
}
\usage{
gaussianSmoothing(x,y,sd=1,xout=NULL,boundary_extension=0,lm_region_lower=boundary_extension,lm_region_upper=boundary_extension)
}
\arguments{
  \item{x}{ The x values formatted as a vector
}
  \item{y}{ The y values, formatted as a vector of length identical to the length of \code{x}
}
\item{sd}{Standard deviation for the smoothing, in terms of x-coordinates}
\item{xout}{The x values where an output is desired; if NULL is provided, \code{xout} is set to \code{x} (default)}
\item{boundary_extension}{Number of elements to linearly extrapolate at the boundaries, to avoid edge effects. Default is 0, meaning no extension}
\item{lm_region_lower}{Indicates the number of elements on the initial portion of the curve that should be used to set the slope for extrapolation. In more detail, the extrapolation (if \code{boundary_extension}>0) is performed by linear extension of both the \code{x} and \code{y} values by \code{N=boundary_extension} elements. For this a linear model (\link{lm}) is calibrated on the first \code{N=lm_region_lower} elements of \code{x} or \code{y} and their indices as a regressor, and then used to extend \code{x} and \code{y} using continuation of the indices to the negative side.  }
\item{lm_region_upper}{Indicates the number of elements on the final portion of the curve that should be used to set the slope for extrapolation. In more detail, the extrapolation (if \code{boundary_extension}>0) is performed by linear extension of both the \code{x} and \code{y} values by \code{N=boundary_extension} elements. For this a linear model (\link{lm}) is calibrated on the last \code{N=lm_region_upper} elements of \code{x} or \code{y} and their indices as a regressor, and then used to extend \code{x} and \code{y} using continuation of the indices to the negative side.  }

 

}
\value{
A numerical vector of the same length as xout}
\author{Thomas+Marina Braschler}
\examples{
xout=seq(from=0,to=6,length.out=500)
x=1:5
y=runif(length(x))
sd=2
smoothed=gaussianSmoothing(x=x,y=y,sd=sd,xout=xout)
plot(x=x,y=y,type="p")
plot.xy(xy.coords(x=xout,y=smoothed),type="l",col="red")
}
\keyword{ misc }

\references{Filtering in the Time and Frequency Domains, Herman J. Blinchikoff, Anatol I. Zverev, Wiley, 1976}





