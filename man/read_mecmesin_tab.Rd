\name{read_mecmesin_tab}
\alias{read_mecmesin_tab}

\title{
read_mecmesin_tab
}
\description{
Reads a Mecmesin text file from the disk. This is an adaptation of the \link{read_texture_analyzer_tab} function to the specific format of Mecmesin text export (tested with Mecmesin MultiTest 2.5 dV)}
\usage{
read_mecmesin_tab(file, sample_diameter = 0.022, downup = TRUE) 
}

\arguments{
  \item{file}{
File path for the tabulated text file.
}
  \item{sample_diameter}{
Diameter of the chuck (or sample, whichever is smaller) used to compress the gel. Indication in meters, the chuck is assumed to be circular. 
}
\item{aggregate_by}{Column to be used for aggregation. Provide FALSE if no aggregation is desired. If \code{aggregate_by="Time"} is indicated, the Time column will be rounded to integers prior to aggregation }
\item{downup}{Chuck displacement down (compression) followed by equal movement up (release)? }
}
\details{
Calls \link{read_texture_analyzer_tab} with specific parameters internally. 
}
\value{
Dataframe with the following columns:
\item{Distance }{Distance of downward movement of the chuck, as given by the texture analyzer; the units are expected to be in mm}
\item{Force }{Force as measured by the Texture Analyzer, in grams}
\item{direction}{Interpretation in terms of downward and upward movement, possible values are "down" and "up"}
\item{Time}{Time of acquisition, as given by the Texture Analyzer}
\item{pressure}{Calculated pressure (strain) exercised by the chuck; since pressure is given by force / area, the area is estimated from the \code{chuck_diameter} argument}
}

\author{
Thomas Braschler
}



\examples{
path = system.file('sampleData/mecmesin/raw-TensileStrenght(peellike)(V1)-Modified Version-sample-2.txt', package = 'textureAnalyzerGels');
sampleGel = read_mecmesin_tab(paste(path,"sampleGel.tab",sep="/"),chuck_diameter=4e-3)

}
\keyword{ misc }

