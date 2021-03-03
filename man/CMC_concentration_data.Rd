\name{CMC_concentration_data}
\alias{CMC_concentration_data}
\alias{CMC_concentration_blank}
\alias{CMC_concentration_file_info}
\docType{data}
\title{
Sample set of cryogel compression data, with different concentration of carboxymethylcellulose
}
\description{
Sample compression curves for CMC/AAD cryogels ranging from 0.65 percent to 2.6 percent of CMC, with always a nominal crosslinking degree of every tenth residue crosslinked by AAD.
}
\usage{data(CMC_concentration_data)}
\format{
  Contains three variables:
  \describe{
    \item{\code{CMC_concentration_data}}{List of 6 elements, containing the compression curves as read by \link{read_texture_analyzer_tab} and smoothened by \link{smooth_texture_analyzer_data} }
    \item{\code{CMC_concentration_file_info}}{Dataframe with 6 lines, desribing the conditions associated with the 6 compression curves. Here, it is primarily the CMC concentration that varies}
     \item{\code{CMC_concentration_blank}}{Dataframe as produced by by \link{read_texture_analyzer_tab} and smoothened by \link{smooth_texture_analyzer_data}. This the blank without gel that should be used to subtract the Archimedes force (this is particularly important for the softer gels) }
    }
}

\examples{

# Generation of the data
path = system.file('sampleData/2016_08_30_Patrick_sample', package = 'textureAnalyzerGels');
CMC_concentration_file_info = read.xls(file.path(path,"file_listing.xlsx") )
CMC_concentration_data=read_texture_analyzer_tab_list(file_info=CMC_concentration_file_info,root_folder=path,sd=0.05,boundary_extension_mm=0.2,lm_region_upper_mm=0.05)

# The blank, for buyuoancy with gel, is common to all samples
CMC_concentration_blank=read_texture_analyzer_tab_list(file_info=CMC_concentration_file_info[1,],folder_column="control_folder",file_column="control_file",root_folder=path,sd=0.2,boundary_extension_mm=0.5,lm_region_upper_mm=0.1)[[1]]


for(ind in 1:length(CMC_concentration_data))
{
theFile = CMC_concentration_file_info[ind,]
theData=CMC_concentration_data[[ind]]

CMC_concentration_data[[ind]]$pressure_reduced = theData$pressure - CMC_concentration_blank$pressure[match(paste(theData$Distance,theData$direction),paste(CMC_concentration_blank$Distance,CMC_concentration_blank$direction))]


CMC_concentration_data[[ind]]$pressureSlope_reduced = theData$pressureSlope - CMC_concentration_blank$pressureSlope[match(paste(theData$Distance,theData$direction),paste(CMC_concentration_blank$Distance,CMC_concentration_blank$direction))]

CMC_concentration_data[[ind]]$height = theFile$start_height_mm-theData$Distance



}

# ===========================================================
# Loading of the data and elementary plotting
data(CMC_concentration_data)
plot(CMC_concentration_data[[1]]$Distance, CMC_concentration_data[[1]]$pressure/1e3,type="l",xlab="Distance [mm]",ylab="Pressure [kPa]",xlim=c(0,3),ylim=c(0,10))

for(ind in 2:length(CMC_concentration_data))
{
lines(CMC_concentration_data[[ind]]$Distance, CMC_concentration_data[[ind]]$pressure/1e3,type="l",col=palette()[ind])
}

lines(CMC_concentration_blank$Distance,CMC_concentration_blank$pressure/1e3,type="l",col=palette()[length(CMC_concentration_data)+1])

legend("topleft",legend=c(paste("c(CMC) = ",round(CMC_concentration_file_info$CMC_conc*100)/10, " mg/mL",sep=""),"blank (no gel)"),lty=rep(1,length(CMC_concentration_data)+1),col=palette()[1:(length(CMC_concentration_data)+1)])

}

\author{
Patrick Burch, Thomas Braschler
}

\keyword{datasets}
