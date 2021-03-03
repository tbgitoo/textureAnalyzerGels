\name{get_frame_compensation_function}
\alias{get_frame_compensation_function}
\title{get_frame_compensation_function
}
\description{Provides a compensation function that allows to estimate how much distance is lost at a given force due to TextureAnalyzer compliance. Relevant only for very hard subtrates, for hydrogels, this is typically negligeable.
}
\usage{
get_frame_compensation_function(filename,min_force=5, max_force = 20)
}
\arguments{
  \item{filename}{ Texture analyzer tab file containing compression data without sample, solely against the bench or other holder surface}
  \item{min_force}{ Minimum force required for inclusion into slope regression region}
  \item{max_force}{ Maximum force for inclusion into slope regression region}
}
\value{
Distance compensation function. Takes the measured force as an argument, and indicates the equivalent chuck movement distance arising from frame compliance}

\details{The intended use of this function is to adjust the distance measurements to compensate for frame compliance. Indeed, frame compliance means that the set (=reported) travelling distance (motor steps) is slightly larger than the distance actually travelled by the chuck surface, a small part being lost due to frame compliance. This small part can be obtained from the Force if passed to the output function provided by the \code{get_frame_compensation_function}.\cr\cr
For soft samples, the frame compensation makes only a very minor difference and is not generally necessary; for very soft samples, buoyancy and capillary effects are generally more important.}

\author{Thomas+Marina Braschler}
\examples{

# Get the frame compensation function
path=system.file("sampleData",package="textureAnalyzerGels")
frame_compensation_function=get_frame_compensation_function(paste(path,"frame_stiffness.tab",sep="/"))

# Comparative plotting
data(sampleGel)
frame_stiffness = read_texture_analyzer_tab(paste(path,"frame_stiffness.tab",sep="/"),chuck_diameter=4e-3)
frame_stiffness_smooth=smooth_texture_analyzer_data(frame_stiffness, sd = 0.005,lm_region_upper_mm=0.05)
touch_point_frame_compensation=find_initial_touch_point(frame_stiffness_smooth)

plot(pressure ~ Distance, sampleGel,type="l",xlim=c(0,start_height_mm+0.5),main="Frame stiffness compensation",xlab="Distance [mm]", ylab="Stress [Pa]")
# This is the corresponding plot without sample
lines(frame_stiffness$Distance-touch_point_frame_compensation+start_height_mm, frame_stiffness$pressure,type="l",lty=2)
# Frame compensation
sampleGel$Distance_compensated = sampleGel$Distance-frame_compensation_function(sampleGel$Force)
lines(pressure ~ Distance_compensated, sampleGel,type="l",col="red")
# Frame compensation on the frame compensation measurement
frame_stiffness$Distance_compensated = frame_stiffness$Distance-frame_compensation_function(frame_stiffness$Force)
lines(frame_stiffness$Distance_compensated-touch_point_frame_compensation+start_height_mm, frame_stiffness$pressure,type="l",lty=2, col="red")

legend("topleft",legend=c("sample Gel","sample Gel compensated", "no sample control","no sample control compensated"), lty=c(1,1,2,2), col=c("black","red","black","red"))

}
\keyword{ misc }

\references{Filtering in the Time and Frequency Domains, Herman J. Blinchikoff, Anatol I. Zverev, Wiley, 1976}





