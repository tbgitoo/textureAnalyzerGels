\name{tissue_mechanical_analysis_list}
\alias{tissue_mechanical_analysis_list}

\title{
tissue_mechanical_analysis_list
}
\description{
 Analysis of list of compression curves, for example from biological tissues. This function applies \link{tissue_mechanical_analysis} to each compression curve and collects the results
}
\usage{
tissue_mechanical_analysis_list(file_info,data_list,start_height_column="start_height_mm",approximate_gel_touch_point_column="approximate_gel_touch_point",...)

}

\arguments{
  \item{file_info}{
Description of the compression curves. This is a dataframe, which needs to contain at least the columns with names given by the arguments \code{start_height_column} and \code{approximate_gel_touch_point_column}, but usually contains more information such as the fabrication conditions. \code{file_info} needs to contain as many lines as there are elements in \code{data_list} 
}
\item{data_list}{List of compression curves, typically produced by \link{read_mecmesin_tab_list}}
   \item{start_height_column}{
Name of the starting height column in \code{file_info}. This column describes the starting height of the chuck at the beginning of compression, in millimeters}
 \item{approximate_gel_touch_point_column}{Name of the column in \code{file_info} that contains a manual estimation of the approximate gel touchpoint, in mm. }
   \item{...}{
Possibility to pass down further arguments to \link{tissue_mechanical_analysis}, used internally
}
 
}

\value{
A list with as many elements as there are lines \code{file_info}. Each of these elements is the output of \link{tissue_mechanical_analysis}. As the output of \link{tissue_mechanical_analysis} is a list itself, the return value is a list of lists.   }


\author{
Thomas Braschler
}

\seealso{
\link{tissue_mechanical_analysis}, used internally
}

\keyword{ misc }

