\name{read_mecmesin_tab}
\alias{read_mecmesin_tab}

\title{
read_mecmesin_tab
}
\description{
Reads a Mecmesin text file from the disk. The units are assumed to mm for the distance, and g for the force
}
\usage{
read_mecmesin_tab(file, chuck_diameter = 0.004,aggregate_by="Distance",downup=TRUE,dec=".",skip_lines=1,unit_line=1,sep="",Force_column="Force",Distance_column="Distance",use_fill_for_read_table=FALSE)
}

\arguments{
  \item{file}{
File path for the tabulated text file.
}
  \item{chuck_diameter}{
Diameter of the chuck used to compress the gel. Indication in meters, the chuck is assumed to be circular. 
}
\item{aggregate_by}{Column to be used for aggregation. Provide FALSE if no aggregation is desired. If \code{aggregate_by="Time"} is indicated, the Time column will be rounded to integers prior to aggregation }
\item{downup}{Chuck displacement down (compression) followed by equal movement up (release)? }
\item{dec}{Decimal point when reading the text files}
\item{skip_lines}{Possibility to skip lines in front of the actual to data to read}
\item{unit_line}{Is there a line in the text file (after the variable names) that contains unit information? If yes pass 1, otherwise 0}
\item{sep}{Separator to be passed to \link{read.table}}
\item{Force_column}{Name of the column where the force is stored. The expected unit is grams for now}
\item{Distance_column}{Name of the column where the distance travelled by the chuck is stored. The expected unit is mm for now}
\item{use_fill_for_read_table}{Set the fill argument to \link{read.table}, used internally. Provide TRUE if there are incomplete lines in the .tab files that should be completed with empty columns}
}
\details{
The routine expects the file to contain force data from both the downward trajectory and the upward trajectory; it assigns the first half of the points to the down trajectory, and the second half to the up trajectory
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
path = system.file('sampleData', package = 'textureAnalyzerGels');
sampleGel = read_mecmesin_tab(paste(path,"sampleGel.tab",sep="/"),chuck_diameter=4e-3)

}
\keyword{ misc }

