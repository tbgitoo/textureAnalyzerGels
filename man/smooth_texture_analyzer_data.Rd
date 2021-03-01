\name{smooth_texture_analyzer_data}
\alias{smooth_texture_analyzer_data}

\title{
smooth_texture_analyzer_data
}
\description{
Smoothes the force and pressure curves using a gaussian kernel
}
\usage{
smooth_texture_analyzer_data(theData, sd = 0.04,cycle_col="cycle",boundary_extension_mm=0.2,lm_region_lower_mm=boundary_extension_mm,lm_region_upper_mm=boundary_extension_mm,block_size=5)
}

\arguments{
  \item{theData}{
Data for the force and distance relationship. Needs to have at least the columns "direction", "Distance", "Force" and "pressure"
}
  \item{sd}{
Standard deviation for the Gaussian kernel, in the same units as the Distance column
}
  \item{cycle_col}{
Column indicating compression cycles, for data analyzed by \link{detect_compression_cycles}. Defaults to a column named "cycle", but if this column cannot be found in \code{theData}, this argument is ignored
}
\item{boundary_extension_mm}{To correct for boundary effects, use linear extension beyond the measured region. This should be typically a few standard deviations \code{sd}}
\item{lm_region_lower_mm}{Region used to establish the linear regression for the extension at the lower \code{Distance} values. Should be within the region where the gel is not touched}
\item{lm_region_upper_mm}{Region used to establish the linear regression for the extension at the upper \code{Distance} values. This is a bit delicate to set, as a longer region is more precise, but the pressure vs. Distance line should still be reasonable straight.}
\item{block_size}{For optimization of calculation time in the smoothing calculation, it is possible to carry out the smoothing block-wise. See \link{gaussianSmoothingBlock} and \link{gaussianSmoothingSlopeBlock} for details. Provide \code{block_size=0} to avoid block-wise calculation during smoothing} 

}
\details{
Calls  \link{gaussianSmoothing} internally
}
\value{
Variable of the same structure and values as the \code{theData} argument, with the following changes: \cr
- Force column now contains smoothened values\cr
- pressure column now contains smoothened values\cr
-new column ForceSlope, which contains the smoothened, momentaneous slope (delta Force / delta Distance) \cr
- new column pressureSlope, which contains the smoothened, momentaneous slope (delta pressure / delta distance) \cr
}

\author{
Thomas Braschler
}

\examples{
data(sampleGel)
plot(Force ~ Distance, sampleGel,type="l")
sampleGelSmooth=smooth_texture_analyzer_data(sampleGel, sd = 0.05,lm_region_upper_mm=0.05)
plot.xy(xy.coords(sampleGelSmooth$Distance,sampleGelSmooth$Force),type="l",col="red")
}

\keyword{ misc }

