\name{foam_mechanical_analysis}
\alias{foam_mechanical_analysis}

\title{
foam_mechanical_analysis
}
\description{
 Analysis of foam compression curves. This function separates the foam compression curve into different zones by indicating the transition points: [free zone (no contact between chuck and foam)] - touchpoint - elastic compression region - transition_E_P -  plateau region - transition_P_D - [densification region]. Localization and mechanical properties are analyzed at the points from touchpoint to transition_P_D.
}
\usage{
foam_mechanical_analysis(theData,start_height,approximate_gel_touch_point=NULL,landmarks=NULL,gel_thickness=NULL,stress_column="pressure",stress_slope_column="pressureSlope")

}

\arguments{
  \item{theData}{
Force-Distance data, typically from a "down"-"up" cycle of the chuck; the data needs to be smooth and contain already at the very least the columns \code{Distance} and \code{direction}, in addition to the colums described by the arguments \code{stress_column} and \code{stress_slope_column}. Such a variable is typically obtained by using \link{read_texture_analyzer_tab} and then  \link{smooth_texture_analyzer_data} on the result. Care should be taken for the smoothing parameter \code{sd} in \link{smooth_texture_analyzer_data}, both the \code{pressure} and the \code{pressureSlope} should appear smooth in a diagram versus \code{distance}, while overall look of the curve with a flat part, a steeper elastic part, a less steep plateau and then again rising densitifcation in \code{pressure} vs. \code{Distance} should be conserved.
}
\item{start_height}{The texture Analyzer typically starts a file by \code{Distance=0}, so we loose the information on how high the chuck is above the substrate before starting compression. This needs to be noted separately and transmitted to this function in the \code{start_height} argument.}
  \item{approximate_gel_touch_point}{
Initial guess for the gel touch point (will be refined by a call to \link{find_landmarks_gel_compression})}
 \item{landmarks}{Possibility to manually provide landmarks. Typically, there is no need to provide this as the function uses \link{find_landmarks_gel_compression} internally. If provided, numeric vector with elements named "touch_point", "elastic_limit", "densification_limit".  If an incomplete set of named elements is given, the function will try to find the missing values via \link{find_landmarks_gel_compression}}
  \item{gel_thickness}{Possibility to manually provide gel thicknessm in the same units as is the "Distance" column in \code{theData}. More typically, there is no need to provide this argument as \code{gel_thickness} will be estimated from \code{start_height} and the distance the chuck needs to travel down until contact with the gel is established (the touchpoint distance).
}
  \item{stress_column}{
Name of the column in \code{theData} that contains the stress information. By default, this is the "pressure" column as produced by \link{read_texture_analyzer_tab} and smoothened by  \link{smooth_texture_analyzer_data}. An alternative column can be used, for instance if chuck buoyancy is substracted with the creation of a new column (done manually). 
}
  \item{stress_slope_column}{
Name of the column in \code{theData} that contains the slope of the stress. The units are the units of the stress column devided by the units of the Distance column. By default, this is the "pressureSlope" column as produced by \link{read_texture_analyzer_tab} and smoothened by  \link{smooth_texture_analyzer_data}.  An alternative column can be used, for instance if chuck buoyancy is substracted with the creation of a new column (done manually). 
}
\item{do_plot}{If \code{TRUE}, plots the stress-Distance curve and indicates the landmarks found}
}

\value{
A list with different geometric and mechanical measures: \cr\cr
\describe{
\item{Distance}{Position of the analyzed points. The starting point of the compression is 0. This is a matrix with 2 rows (for the "down" and "up" part of the compression cycle"), and 5 columns. There is a column for each characteristic point of the curve: "touchpoint" for the point where the chuck touches the gel; "elastic" for the midpoint of the elastic compression part; "transition_E_P" for the transition point between elastic compression and plateau; "plateau" for the plateau midpoint; "transition_P_D" transition point from the plateau to densification. }
\item{strain}{Relative compression. This is distance relative to the touchpoint, divided by gel height. This variable has the same structure as the Distance element described above.}
\item{strain}{Relative compression. This is distance relative to the touchpoint, divided by gel height. This variable has the same structure as the Distance element described above.}
\item{stress}{Mechanical stress (force per area) at the different points. Again same matrix layout.}
\item{E}{Local Young moduli. The local Young moduli are the local slopes (derivative) in strain-stress diagrams. Same structure.}
\item{E_from_touchpoint}{Young moduli as measured from the touchpoint.The slope is taken from the straight line connecting the touchpoint to the desired second point on the curve in the strain-stress diagram. Same matrix structure as the variables before.}
\item{plateau_detected}{Whether or not the was a plateau detected. A plateau is detected if the smoothened slope has a minimum beyond the elastic compression, otherwise no plateau is detected. Logical vector of two elements, for the down and up part of the compression cycle. }
\item{plateau_width}{In terms of strain, the width of the plateau. Numerical vector of two values, for the up and down part of the compression cycle.}
\item{gel_height}{Height of the gel. Single numerical value, in mm}
}
}

\details{The algorithm sequentially evaluates the positions of the analysis points (touchpoint, mid elastic, transition to plateau, plateau, transition to densification), and also evaluates the different geometric and mechanical measures at these points. First, the function needs the landmarks available either manually or more typically through \link{find_landmarks_gel_compression}, called internally if the \code{landmarks} argument is not provided.\cr\cr

 Within the elastic region delimited by the landmarks, the algorithm then looks for a local maximum of the slope (in the column designated by the argument \code{stress_slope_column} in \code{theData}. For this it is important that the curve is appropriately smoothened beforehand, in general by using \link{smooth_texture_analyzer_data} before using this function. Appropriately means here that noise should be removed so that the slope appears smooth, but the smoothing should still conserve the overall features of the curve and namely the existence of a local maximum in the elastic compression region. \cr\cr
 The touchpoint is then sought using the data from the elastic midpoint. For this, the algorithm considers the tangent at the elastic midpoint in the Force vs. Distance diagram, and looks for its intersection with x-axis. This defines the touchpoint where the chuck is assumed to substantially enter into interaction with the gel. 
 \cr\cr On the other side of the midpoint of the elastic region defined by the local maximum slope, the algorithm then looks for a minimum slope at higher compression. For this it is again important to have the curve appropriately smoothened, otherwise the minimum will be defined by noise rather than the global shape of the curve. The plateau is a feature of foams with sufficiently high pore fraction, it may not exist in all experimental configurations. If no local minimum is found, the plateau midpoint is set identical to the elastic compression midpoint, and \code{plateau_detected=FALSE} will be returned for the corresponding direction ("down" or "up", see value section). In this case, the analysis is not appropriate and the caution should be applied in the interpretation of the values. \cr\cr
 The transition point between elastic compression and elastic compression region and the plateau region is then sought as the intersection between the tangent at the plateau midpoint and the tangent at the elastic midpoint, in the Force vs. Distance diagram. The transition point is noted "transition_E_P" for transition from elastic to plateau. \cr\cr
 Finally, the function determines the transition point from plateau to densification ("transition_P_D"). For this, it first determines the slope at the transition point from elastic compression to plateau, and then looks for the first point beyond the plateau midpoint where this slope is again exceeded. This first point is the transition point from plateau to densification. \cr\cr
 The above algorithm is carried out on both the descending "down" part of the compression cycle and the ascending ("up") part of the compression cycle. Usually, the positions and values for the individual points differ slightly. For the determination of the gel height, the average of the touchpoint Distance is evaluated. This is then substracted from the starting height, which needs to be supplied (\code{start_height}).\cr\cr
  With the positions of the different points known, stress, strain, and Young moduli are then evaluated for the points for both the "down" and "up" part of the compression cycle and tabulated as described in the value section.    }

\author{
Thomas Braschler
}

\seealso{
\link{find_landmarks_gel_compression}, used internally
}
\examples{
data(sampleGelSmooth)
results=foam_mechanical_analysis(
	sampleGelSmooth,start_height=start_height_mm,approximate_gel_touch_point=0.15)
results
Distance = results[["Distance"]]
stress = results[["stress"]]	
plot(pressure ~ Distance, sampleGelSmooth,type="l",xlab="Distance [mm]", ylab="Stress [Pa]",ylim=c(0,2500))
lines(Distance["down",],stress["down",],type="p",col="red")
lines(Distance["up",],stress["up",],type="p",col="green")
text(x=Distance["up","touchpoint"]+0.22,y=stress["up","touchpoint"],labels="Touchpoint")
text(x=Distance["up","elastic"]+0.3,y=stress["up","elastic"],labels="Linear elasticity")
text(x=Distance["up","transition_E_P"]+0.2,y=stress["up","transition_E_P"]-30,labels="Transition")
text(x=Distance["up","plateau"]+0.2,y=stress["up","plateau"]-30,labels="Plateau")
text(x=Distance["up","transition_P_D"]+0.45,y=stress["up","transition_P_D"]-30,labels="Transition to densification")

legend("topleft",pch=c(1,1),col=c("red","green"),legend=c("Compression","Relaxation"))



}

\keyword{ misc }

