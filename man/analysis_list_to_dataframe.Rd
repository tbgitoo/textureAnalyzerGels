\name{analysis_list_to_dataframe}
\alias{analysis_list_to_dataframe}
\alias{process_foam_mechanical_analysis_list_to_dataframe}

\title{
analysis_list_to_dataframe
}
\description{
 Converts the output of \code{foam_mechanical_analysis_list}, which is a nested list (list of list), into a flat dataframe for use in plotting and statistical analysis.
}
\usage{
analysis_list_to_dataframe(file_info,output_foam_mechanical_analysis_list)

}

\arguments{
  \item{file_info}{
Description of the compression curves. This is a dataframe, which needs to contain as many lines as there are elements in \code{data_list} 
}
\item{output_foam_mechanical_analysis_list}{Output by  \link{foam_mechanical_analysis_list}. This is a list of lists}
 
}

\value{
A dataframe containing one line per estimated value. The values are described by the columns \code{measurement}, \code{direction} and \code{point}. The \code{measurement} indicates what is measured (stress, strain, distance, and so forth), the \code{direction} indicates whether this was obtained in the "down" or "up" cycle (compression or release), while the \code{point} column indicates the relevant segment on the foam compression curve (touchpoint, linear elastic segment, transition to plateau, plateau, transition to densification). As there are many measurements and points, as well as the "down" and "up" cycle, there are many lines in the return value for each line in \code{file_info}. The   }


\author{
Thomas Braschler
}

\seealso{
\link{read_texture_analyzer_tab}, used internally
}
\examples{
data(CMC_concentration_data)
foam_mechanical_analysis_list_result=foam_mechanical_analysis_list(file_info=CMC_concentration_file_info, data_list=CMC_concentration_data,stress_column="pressure_reduced",stress_slope_column="pressureSlope_reduced")	

analyzed_data = analysis_list_to_dataframe(file_info=CMC_concentration_file_info,output_foam_mechanical_analysis_list=foam_mechanical_analysis_list_result)



# Usage of the dataframe: one has to select specific measurements at specific points and a specific direction

E_elastic_down = analyzed_data[analyzed_data$measurement=="E" & analyzed_data$point=="elastic" & analyzed_data$direction=="down",]

plot(value ~ CMC_conc, E_elastic_down[order(E_elastic_down$CMC_conc),],type="b",log="xy")


}

\keyword{ misc }

