\name{plot_stress_strain}
\alias{plot_stress_strain}

\title{
plot_stress_strain
}
\description{
Draws a stress strain plot from compression data as read by \link{read_texture_analyzer_tab} and typically smoothened by \link{smooth_texture_analyzer_data}
}
\usage{
plot_stress_strain(compression_data, analysis,start_height_mm, plot_evaluation=TRUE,pressure_column="pressure",distance_column="Distance",direction_column="direction",
title="stress-strain")
}

\arguments{
  \item{compression_data}{
Compression data as read by \link{read_texture_analyzer_tab}
}
  \item{analysis}{
Analyzed data as evaluated by \link{foam_mechanical_analysis}
}
\item{start_height_mm}{Starting height for the compression test, a priori in mm. Needs to be recorded when doing the test as the textureAnalyzer saves the data with starting Distance 0 at the beginning of the test, regardless of the absolute height
}
\item{plot_evaluation}{Indicates whether analysis data should be indicated directly in the plot
}
\item{pressure_column}{Possibility to indicate an alternative pressure column, for instance when correcting for buoyancy
}
\item{distance_column}{Possibility to indicate an alternative distance column
}
\item{direction_column}{Possibility to indicate an alternative direction column
}
\item{title}{Title of the graphic
}


}
\details{
Converts the distance values to strain values by using the \code{start_height_mm} information and the touchpoint information contained in the \code{analysis} argument. Together with the pressure column in \code{compression_data}, a stress-strain plot is drawn. In addition, if \code{plot_evaluation} is \code{TRUE}, detailed data from the \code{analysis} variable is included. In text form, the Young moduli at compression (direction "down") and relaxation (direction "up") are provided, both for the elastic and plateau segement. The touchpoint, central elastic point and central plateau points are marked with red circles, and the relevant slopes for the Young modulus calculation are indicated as red lines.
}
\value{
None
}

\author{
Thomas Braschler
}



\examples{
path = system.file('sampleData', package = 'textureAnalyzerGels');
sampleGel = smooth_texture_analyzer_data(read_texture_analyzer_tab(paste(path,"sampleGel.tab",sep="/"),chuck_diameter=4e-3),sd = 0.05,lm_region_upper_mm=0.05)
start_height_mm=2.5

analysis=foam_mechanical_analysis(
	sampleGel,start_height=start_height_mm,approximate_gel_touch_point=0.15)
	
plot_stress_strain(sampleGel, analysis,start_height_mm, title="Sample plot stress-strain")	

}
\keyword{ misc }

