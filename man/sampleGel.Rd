\name{sampleGel}
\alias{sampleGel}
\alias{start_height_mm}
\docType{data}
\title{
Sample Gel compression data
}
\description{
Compression curve for a 1.5 percent CMC (MW=90kDa) cryogel, crosslinked at every 10th residue by adipic dihydrazide (crosslinking pH is 6.7).
}
\usage{data(sampleGel)}
\format{
  A data frame named \code{sampleGel} with the following variables
  \describe{
    \item{\code{Distance}}{Distance travalled by the chuck from its original height, in millimeters }
    \item{\code{direction}}{Direction of movement (i.e. down, then up)}
    \item{\code{Force}}{Force measured by the texture Analyzer. In grams.}
    \item{\code{Time}}{Time elapsed since the beginning of the compression experiment. In seconds.}
     \item{\code{pressure}}{Pressure (stress) as calculated form Force / gel cross section are. In Pascals}
  }
 
  In addition a numerical value names \code{start_height_mm} that indicates the height of the chuck at the beginning of the experiment (where the \code{Distance} travelled is 0).
}

\examples{

# Generation of the data
path = system.file('sampleData', package = 'textureAnalyzerGels');
sampleGel = read_texture_analyzer_tab(paste(path,"sampleGel.tab",sep="/"),chuck_diameter=4e-3)
start_height_mm=2.5

# Loading of the data and elementary plotting
data(sampleGel)
plot(sampleGel$Distance, sampleGel$pressure/1e3,type="l",xlab="Distance [mm]",ylab="Pressure [kPa]")


}
\keyword{datasets}

\author{
Patrick Burch \cr
Thomas Braschler
}

