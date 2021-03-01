\name{sampleGelSmooth}
\alias{sampleGelSmooth}
\docType{data}
\title{
Sample Gel compression data, smoothened
}
\description{
Compression curve for a 1.5 percent CMC (MW=90kDa) cryogel, crosslinked at every 10th residue by adipic dihydrazide (crosslinking pH is 6.7). Smoothened by \link{smooth_texture_analyzer_data}
}
\usage{data(sampleGelSmooth)}
\format{
  A data frame names \code{sampleGelSmooth} with the following variables
  \describe{
    \item{\code{Distance}}{Distance travalled by the chuck from its original height, in millimeters }
    \item{\code{direction}}{Direction of movement (i.e. down, then up)}
    \item{\code{Force}}{Force measured by the texture Analyzer. In grams.}
    \item{\code{Time}}{Time elapsed since the beginning of the compression experiment. In seconds.}
     \item{\code{pressure}}{Pressure (stress) as calculated form Force / gel cross section are. In Pascals}
      \item{\code{ForceSlope}}{Slope of the smoothened force curve, in gram/mm}
      \item{\code{pressureSlope}}{Slope of the smoothened pressure curve, in Pa/mm}
      
  }
 
  In addition a numerical value names \code{start_height_mm} that indicates the height of the chuck at the beginning of the experiment (where the \code{Distance} travelled is 0).
}

\examples{

# Generation of the data
data(sampleGel)
sampleGelSmooth=smooth_texture_analyzer_data(sampleGel, sd = 0.05,lm_region_upper_mm=0.05)
start_height_mm=2.5

# Loading of the data and elementary plotting
data(sampleGel)
data(sampleGelSmooth)
plot(sampleGel$Distance, sampleGel$pressure/1e3,type="l",xlab="Distance [mm]",ylab="Pressure [kPa]",ylim=c(0,2))
plot.xy(xy.coords(sampleGelSmooth$Distance,sampleGelSmooth$pressure/1e3),type="l",col="red")
}




\keyword{datasets}
