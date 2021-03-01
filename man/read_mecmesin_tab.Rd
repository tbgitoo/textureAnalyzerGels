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
\item{Distance }{Distance of movement of the chuck, as provided by the Mecmesin apparatus; the units are expected to be in mm}
\item{Force }{Force as measured by the Texture Analyzer, in grams}
\item{direction}{Interpretation in terms of downward and upward movement, possible values are "down" and "up". This is with respect to the phases of the program (assumed down first), not necessarily physical orientation. }
\item{Time (s)}{Time of acquisition, as given by the Mecmesin apparatus}
\item{pressure}{Calculated pressure (stress) exercised by the chuck; since pressure is given by force / area, the area is estimated from the \code{sample_diameter} argument}
}

\author{
Thomas Braschler
}



\examples{
# This example is taken from https://doi.org/10.5281/zenodo.4441090 (Filippova et al. 2021), Supplementary_Fig_S6.zip, sample2 for the traction test. From there, we copied the file "raw-TensileStrenght(peellike)(V1)-Modified Version-sample-2.txt" here to serve as an example.
path = system.file('sampleData/mecmesin/data/raw-TensileStrenght(peellike)(V1)-Modified Version-sample-2.txt', package = 'textureAnalyzerGels');
exampleTraction = read_mecmesin_tab(path,sample_diameter=4.76e-3,downup=FALSE)
plot(pressure ~ Distance, exampleTraction,type="b",xlab="Distance [mm]", ylab="Stress [Pa]")
}
\keyword{ misc }

\references{
 Filippova A, Bonini F, Efremov L, Locatelli M, Preynat-Seauve O, Beduer A, Krause KH, Braschler T, 2021: Neurothreads: development of supportive carriers for mature dopaminergic neuron differentiation and implantation, Raw data set: https://doi.org/10.5281/zenodo.4441090 accompanying the corresponding scientific publication at Biomaterials (https://doi.org/10.1016/j.biomaterials.2021)
}

