\name{foam_mechanical_analysis_mecmesin}
\alias{foam_mechanical_analysis_mecmesin}

\title{
foam_mechanical_analysis_mecmesin
}
\description{
Specific adaption of \link{foam_mechanical_analysis} to Mecmesin file format.
}
\usage{
foam_mechanical_analysis_mecmesin(theData,offset_height=0,approximate_gel_height=NULL,stress_column="pressure",stress_slope_column="pressureSlope")

}

\arguments{
  \item{theData}{
Force-Distance data, typically from a "down"-"up" cycle of the chuck; the data needs to be smooth and contain already at the very least the columns \code{Distance} and \code{direction}, in addition to the colums described by the arguments \code{stress_column} and \code{stress_slope_column}. Such a variable is typically obtained by using \link{read_mecmesin_tab}. 
}
\item{offset_height}{If zeroing of the z-position ("Distance" column in \code{theData}) is performed at some point above the bench surface, the height offset needs to be added to obtain the true height above the bench surface. Provide this information in the \code{offset_height} argument if necessary.}
  \item{approximate_gel_height}{
Approximate (typically, caliper-based) estimation of the gel height in mm}
  \item{stress_column}{
Name of the column in \code{theData} that contains the stress information. By default, this is the "pressure" column as produced by \link{read_texture_analyzer_tab} and smoothened by  \link{smooth_texture_analyzer_data}. An alternative column can be used, for instance if chuck buoyancy is substracted with the creation of a new column (done manually). 
}
  \item{stress_slope_column}{
Name of the column in \code{theData} that contains the slope of the stress. The units are the units of the stress column devided by the units of the Distance column. By default, this is the "pressureSlope" column as produced by \link{read_texture_analyzer_tab} and smoothened by  \link{smooth_texture_analyzer_data}.  An alternative column can be used, for instance if chuck buoyancy is substracted with the creation of a new column (done manually). 
}
}

\value{
A list with different geometric and mechanical measures, as for \link{foam_mechanical_analysis} : \cr\cr
\describe{
\item{Distance}{Position of the analyzed points. The starting point of the compression is 0. This is a matrix with 2 rows (for the "down" and "up" part of the compression cycle"), and 5 columns. There is a column for each characteristic point of the curve: "touchpoint" for the point where the chuck touches the gel; "elastic" for the midpoint of the elastic compression part; "transition_E_P" for the transition point between elastic compression and plateau; "plateau" for the plateau midpoint; "transition_P_D" transition point from the plateau to densification. }
\item{strain}{Relative compression. This is distance relative to the touchpoint, divided by gel height. This variable has the same structure as the Distance element described above.}
\item{strain}{Relative compression. This is distance relative to the touchpoint, divided by gel height. This variable has the same structure as the Distance element described above.}
\item{stress}{Mechanical stress (force per area) at the different points. Again same matrix layout.}
\item{E}{Local Young moduli. The local Young moduli are the local slopes (derivative) in strain-stress diagrams. Same structure. The Young modulus on the elastic segment ("elastic") in the compression direction ("down" if using compression and relaxation) can be used to define the Young modulus as it corresponds to the zone of linear elasticity (Gibson&Ashby 1997).	}
\item{E_from_touchpoint}{Young moduli as measured from the touchpoint.The slope is taken from the straight line connecting the touchpoint to the desired second point on the curve in the strain-stress diagram. Same matrix structure as the variables before.}
\item{plateau_detected}{Whether or not the was a plateau detected. A plateau is detected if the smoothened slope has a minimum beyond the elastic compression, otherwise no plateau is detected. Logical vector of two elements, for the down and up part of the compression cycle. }
\item{plateau_width}{In terms of strain, the width of the plateau. Numerical vector of two values, for the up and down part of the compression cycle.}
\item{gel_height}{Height of the gel. Single numerical value, in mm}
}
}

\details{The usage of the Mecmesin MultiTest 2.5dV apparatus we used is a bit different from the TextureAnalyzer TA.XT. At least with our present settings, the TextureAnalyzer TA.XT files indicate distances relative to the starting point, so that the "Distance" values rise from 0 to some maximum positive value before descending to zero again in the retreat phase of the test. On the contrary, the Mecmesin apparatus is calibrated to absolute height above the bench surface and so the "Distance" values in compression commence at some positive value, decrease, and increase again. This function takes the "Mecmesin" distances and recalculates 0-based distance-values, and then uses \link{foam_mechanical_analysis} for the actual mechanical analysis. \cr\cr
Another particularity to describe in further detail is the offset height. Especially with the more sensitive strain gauges, it is in fact a bit risky to perform zeroing on a hard surface (the gauges easily break at even slight overloads, while the test bench is capable to literally crush them), and so we typically zero on softer surface or by manually measuring the position of the chuck above the bench surface. As a result, the distance is correct only to within some positive offset value, which has to be specified as the apparatus has no way of knowing it.\cr\cr
This function internally calculates key arguments to feed to \link{foam_mechanical_analysis}. First, the \code{start_height} is thus given by the initial distance value plus the \code{offset_height}. Second, the initial estimate of the gel touch point (i.e. \code{approximate_gel_touch_point} in \link{foam_mechanical_analysis}) is calculated from a macroscopic (caliper) measurement of the gel height provided as argument \code{approximate_gel_height} here, as \code{approximate_gel_touch_point=start_height-approximate_gel_height}.}

\author{
Thomas Braschler
}

\seealso{
\link{foam_mechanical_analysis}, used internally\cr
\link{read_mecmesin_tab} that can be used for reading Mecmesin text files to import the relevant data into R.
}
\examples{
# This example is to illustrate the Mecmesin and TextureAnalyzer formats with the aid of a single example (measured on a TextureAnalyzer machine)
data(sampleGelSmooth)
start_height_mm=2.5
plot(pressure~Distance, sampleGelSmooth,type="l",xlim=c(0,3),xlab="Distance [mm]",ylab="Stress [Pa]")
results_texture_analyzer=foam_mechanical_analysis(sampleGelSmooth,start_height=start_height_mm)
sampleGelSmooth$Distance=start_height_mm-sampleGelSmooth$Distance
# The slope columns would have opposite signs with an inverted distance variable
sampleGelSmooth$pressureSlope=-sampleGelSmooth$pressureSlope
sampleGelSmooth$ForceSlope=-sampleGelSmooth$ForceSlope
lines(pressure~Distance, sampleGelSmooth,type="l",col="red")
legend("topleft",legend=c("TextureAnalyzer format","Mecmesin format"),lty=c(1,1),col=c("black","red"))
results_mecmesin=foam_mechanical_analysis_mecmesin(sampleGelSmooth)

cat("Evaluation by foam_mechanical_analysis")
results_texture_analyzer
cat("Evaluation by foam_mechanical_analysis_mecmesin")
results_mecmesin 

}

\keyword{ misc }

\references{Gibson, L. J. & Ashby, M. F. Cellular Solids: Structure and properties. (Cambridge University Press, 1997).}

