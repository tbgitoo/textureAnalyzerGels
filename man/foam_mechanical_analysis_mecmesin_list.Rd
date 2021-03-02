\name{foam_mechanical_analysis_mecmesin_list}
\alias{foam_mechanical_analysis_mecmesin_list}

\title{
foam_mechanical_analysis_mecmesin_list
}
\description{
 Analysis of list of foam compression curves. This function applies \link{foam_mechanical_analysis_mecmesin} to each compression curve and collects the results
}
\usage{
foam_mechanical_analysis_mecmesin_list(file_info,data_list,offset_height_column="offset_height_mm",approximate_gel_height_column="approximate_gel_height",...)

}

\arguments{
  \item{file_info}{
Description of the compression curves. This is a dataframe, which needs to contain at least the columns with names given by the arguments \code{start_height_column} and \code{approximate_gel_touch_point_column}, but usually contains more information such as the fabrication conditions. \code{file_info} needs to contain as many lines as there are elements in \code{data_list} 
}
\item{data_list}{List of compression curves, typically produced by \link{read_mecmesin_tab_list}}
  \item{offset_height_column}{
Name of the off set height column in \code{file_info}. This column describes the offset between indicated and absolute height in millimeters.}
 \item{approximate_gel_height_column}{Name of the column in \code{file_info} that contains a manual estimation of the approximate gel height, in mm. }
   \item{...}{
Possibility to pass down further arguments to \link{foam_mechanical_analysis_mecmesin}, used internally
}
 
}

\value{
A list with as many elements as there are lines \code{file_info}. Each of these elements is the output of \link{foam_mechanical_analysis_mecmesin}. As the output of \link{foam_mechanical_analysis_mecmesin} is a list itself, the return value is a list of lists.   }


\author{
Thomas Braschler
}

\seealso{
\link{foam_mechanical_analysis_mecmesin}, used internally
}

\keyword{ misc }

