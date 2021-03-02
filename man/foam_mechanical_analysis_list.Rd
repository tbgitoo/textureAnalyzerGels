\name{foam_mechanical_analysis_list}
\alias{foam_mechanical_analysis_list}

\title{
foam_mechanical_analysis_list
}
\description{
 Analysis of list of foam compression curves. This function applies \link{foam_mechanical_analysis} to each compression curve and collects the results
}
\usage{
foam_mechanical_analysis_list(file_info,data_list,start_height_column="start_height_mm",approximate_gel_touch_point_column="approximate_gel_touch_point",...)

}

\arguments{
  \item{file_info}{
Description of the compression curves. This is a dataframe, which needs to contain at least the columns with names given by the arguments \code{start_height_column} and \code{approximate_gel_touch_point_column}, but usually contains more information such as the fabrication conditions. \code{file_info} needs to contain as many lines as there are elements in \code{data_list} 
}
\item{data_list}{List of compression curves, typically produced by \link{read_texture_analyzer_tab_list}}
  \item{start_height_column}{
Name of the starting height column in \code{file_info}. This column describes the starting height of the chuck at the beginning of compression, in millimeters}
 \item{approximate_gel_touch_point_column}{Name of the column in \code{file_info} that contains a manual estimation of the approximate gel touchpoint, in mm. }
   \item{...}{
Possibility to pass down further arguments to \link{foam_mechanical_analysis}, used internally
}
 
}

\value{
A list with as many elements as there are lines \code{file_info}. Each of these elements is the output of \link{foam_mechanical_analysis}. As the output of \link{foam_mechanical_analysis} is a list itself, the return value is a list of lists.   }


\author{
Thomas Braschler
}

\seealso{
\link{foam_mechanical_analysis}, used internally
}
\examples{
data(CMC_concentration_data)
results=foam_mechanical_analysis_list(file_info=CMC_concentration_file_info, data_list=CMC_concentration_data,stress_column="pressure_reduced",stress_slope_column="pressureSlope_reduced")
	



}

\keyword{ misc }

