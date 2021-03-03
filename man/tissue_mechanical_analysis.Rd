\name{tissue_mechanical_analysis}
\alias{tissue_mechanical_analysis}

\title{
tissue_mechanical_analysis
}
\description{
 Analysis of biological tissue compression curves. Stress-strain curves from biological tissues are usually non-linearly increasing. This function evaluates the Young modulus as the slope in the stress strain curves for small strain segments: strain 0.05-0.15, strain 0.15 to 0.25, then 0.25 to 0.35 and so forth. This gives a quick, discrete overview over how the Young modulus increases with increasing strain.
}
\usage{
tissue_mechanical_analysis(theData,start_height,approximate_touch_point=NULL,stress_column="pressure",stress_slope_column="pressureSlope")

}

\arguments{
  \item{theData}{
Force-Distance data, typically from a "down"-"up" cycle of the chuck; the data needs to be smooth and contain already at the very least the columns \code{Distance} and \code{direction}, in addition to the colums described by the arguments \code{stress_column} and \code{stress_slope_column}. Such a variable is typically obtained by using \link{read_texture_analyzer_tab} and then  \link{smooth_texture_analyzer_data} on the result. Care should be taken for the smoothing parameter \code{sd} in \link{smooth_texture_analyzer_data}, both the \code{pressure} and the \code{pressureSlope} should appear smooth in a diagram versus \code{distance}, while overall look of the curve with a flat part, a steeper elastic part, a less steep plateau and then again rising densitifcation in \code{pressure} vs. \code{Distance} should be conserved.
}
\item{start_height}{The texture Analyzer typically starts a file by \code{Distance=0}, so we loose the information on how high the chuck is above the substrate before starting compression. This needs to be noted separately and transmitted to this function in the \code{start_height} argument.}
  \item{approximate_touch_point}{
Initial guess for the gel touch point (will be refined by a call to \link{find_initial_touch_point})}
\item{stress_column}{
Name of the column in \code{theData} that contains the stress information. By default, this is the "pressure" column as produced by \link{read_texture_analyzer_tab} and smoothened by  \link{smooth_texture_analyzer_data}. An alternative column can be used, for instance if chuck buoyancy is substracted with the creation of a new column (done manually). 
}
  \item{stress_slope_column}{
Name of the column in \code{theData} that contains the slope of the stress. The units are the units of the stress column devided by the units of the Distance column. By default, this is the "pressureSlope" column as produced by \link{read_texture_analyzer_tab} and smoothened by  \link{smooth_texture_analyzer_data}.  An alternative column can be used, for instance if chuck buoyancy is substracted with the creation of a new column (done manually). 
}
}

\value{
A list with different geometric and mechanical measures: \cr\cr
\describe{
\item{touchpoint}{A numeric vector describing the touchpoint coordinates found for the different directions (usually, "down" and "up").}
\item{sample_thickness}{Estimation of the sample height}
\item{E}{A matrix with rows for the directions ("down", "up" in general) and columns for the different strains at which the Young modulus is estimated (i.e. 9 colums for Young moduli at strains 0.1, 0.2, 0.3, ... , 0.9}
\item{hysteresis}{An overall estimate of the hysteresis between compression ("down") and relaxation ("up"). This is the area between the "down" and "up" curve devided by the area under the "down" curve.}

}
}

\details{ The estimations of the Young modulus given in this function approximately corresponds to the local tangent modulus (ATSM E111); they are found by local linear regression (\link{lm}). }

\author{
Thomas Braschler
}

\seealso{
\link{find_initial_touch_point}, used internally
}
\examples{
# This is not necessarily the most appropriate use since with foam_mechanical_analysis, there is a more specific function for the foam-like cryogels. Here, one can see that the
# initial elastic Young modulus is high, followed by a plateau with lower Young moduli before a final increase

data(sampleGelSmooth)
results=tissue_mechanical_analysis(
	sampleGelSmooth,start_height=start_height_mm)
results



}

\keyword{ misc }

\references{
ATSM standard E111: \href{https://www.astm.org/Standards/E111.htm}{https://www.astm.org/Standards/E111.htm	}}

