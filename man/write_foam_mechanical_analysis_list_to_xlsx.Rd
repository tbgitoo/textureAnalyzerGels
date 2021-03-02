\name{write_foam_mechanical_analysis_list_to_xlsx}
\alias{write_foam_mechanical_analysis_list_to_xlsx}

\title{
write_foam_mechanical_analysis_list_to_xlsx
}
\description{
 Writes the output of \code{foam_mechanical_analysis_list}, which is a nested list (list of list), into an Excel file for further standalone processing.
}
\usage{
write_foam_mechanical_analysis_list_to_xlsx(file_info,output_foam_mechanical_analysis_list,file_location)

}

\arguments{
  \item{file_info}{
Description of the compression curves. This is a dataframe, which needs to contain as many lines as there are elements in \code{data_list} 
}
\item{output_foam_mechanical_analysis_list}{Output by  \link{foam_mechanical_analysis_list}. This is a list of lists}
\item{file_location}{Path to the Excel file to be written. If it already exists, will be overwritten without further notice.}
 
}



\author{
Thomas Braschler
}

\details{This function uses the various spreadsheet creation, manipulation and saving functions provided by the \link{xlsx} package for generating and writing of the output Excel file.}

\examples{
data(CMC_concentration_data)
foam_mechanical_analysis_list_result=foam_mechanical_analysis_list(file_info=CMC_concentration_file_info, data_list=CMC_concentration_data,stress_column="pressure_reduced",stress_slope_column="pressureSlope_reduced")	

write_foam_mechanical_analysis_list_to_xlsx(file_info=CMC_concentration_file_info,output_foam_mechanical_analysis_list=foam_mechanical_analysis_list_result,file_location="sample.xlsx")





}

\keyword{ misc }

