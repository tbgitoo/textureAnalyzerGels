\name{detect_compression_cycles}
\alias{detect_compression_cycles}

\title{
detect_compression_cycles
}
\description{
Detects cycle of back-and-forth movement in texture Analyzer data
}
\usage{
detect_compression_cycles(textureAnalyzerData)
}

\arguments{
  \item{textureAnalyzerData}{
Data as read by \link{read_texture_analyzer_tab}}
 \item{do_aggregation}{Whether or not to do aggregation. If TRUE, \link{aggregate} by the \code{Distance} column within each phase  ("up" or "down") in each of the cycles. Is set to TRUE by default}}
\details{
The routine looks for up and down movements by analyzing the "Distance" column in the \code{textureAnalyzerData} data frame.
}
\value{
The same dataframe that was passed to the function, with two supplementary (or overwritten) columns: "direction" indicates the current movement, "cycle" indicates the current compression cycle.
}

\author{
Thomas Braschler
}



\examples{
# This is destructive testing on a classical (nanoporous) CMC gel, for illustration of the multi-cycle analysis
path = system.file('sampleData', package = 'textureAnalyzerGels');
sampleGel=detect_compression_cycles(read_texture_analyzer_tab(paste(path,"sampleDataMultipleCycles.tab",sep="/"),chuck_diameter=4e-3,aggregate_by=FALSE))

sampleGelSmooth = smooth_texture_analyzer_data(sampleGel,boundary_extension_mm=0.2,sd=0.0025)

cycle = 1

col=palette()[cycle]

plot(pressure ~ Distance, sampleGelSmooth[sampleGelSmooth$cycle==cycle ,],type="l",col=col,ylim=c(-750,1250),xlim=c(0,8),ylab="Stress [Pa]",xlab="Compression [mm]")


for(cycle in 2:max(sampleGelSmooth$cycle))
{
col=palette()[cycle]

lines(pressure ~ Distance, sampleGelSmooth[sampleGelSmooth$cycle==cycle ,],type="l",col=col,ylim=c(-500,1000))


}

}
\keyword{ misc }

