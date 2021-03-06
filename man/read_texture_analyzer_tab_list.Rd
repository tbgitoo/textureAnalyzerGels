\name{read_texture_analyzer_tab_list}
\alias{read_texture_analyzer_tab_list}

\title{
read_texture_analyzer_tab_list
}
\description{
Reads a series of texture analyzer tab files
}
\usage{
read_texture_analyzer_tab_list(file_info,root_folder,folder_column="Folder",file_column="File",
         chuck_diameter_column="diameter_mm",do_plot=TRUE,do_smoothing=TRUE,lines_to_read=NULL,
         aggregate_by="Distance",downup=TRUE,dec=".",skip_lines=1,unit_line=1,sep="",
         Force_column="Force",Distance_column="Distance",use_fill_for_read_table=FALSE,...)
}

\arguments{
  \item{file_info}{
Dataframe describing the files to be read. Needs to contain at least the following columns: 1. A folder column, describing the sub-folder where the individual files are located. The name of this column is given by the argument \code{folder_column}. 2. A file column indicating the filename of the file to be read. The name of this column is given by \code{file_column}. 3. A chuck or gel diameter column, where the diameter of the chuck or gel is indicated (whichever is smaller, basically). The name of this column is given by the \code{chuck_diameter_column} argument.
}
\item{root_folder}{Common root folder where all the tab-files to be read are located.}
  \item{folder_column}{
Name of the column in \code{file_info} that contains the folder location.
}
\item{file_column}{Name of the column in \code{file_info} that contains the filename.}
\item{chuck_diameter_column}{Name of the column in \code{file_info} that contains the relevant chuck/gel diameters. The chuck or gel diameters should be in millimeters.}
\item{do_plot}{Indicates whether or not plots should be drawn while reading the files}
\item{do_smoothing}{Indicates whether or not smoothing by \link{smooth_texture_analyzer_data} should be performed when reading the files}
\item{lines_to_read}{Possibility to restrict the lines to be read, a numerical vector indicating the lines to be read.}
\item{aggregate_by}{Aggregation argument passed to \link{read_texture_analyzer_tab}, used internally}
\item{downup}{Chuck displacement down (compression) followed by equal movement up (release)?}
\item{dec}{Decimal point when reading the text files}
\item{skip_lines}{Possibility to skip lines in front of the actual to data to read}
\item{unit_line}{Is there a line in the text file (after the variable names) that contains unit information? If yes pass 1, otherwise 0}
\item{Force_column}{Name of the column where the force is stored. The expected unit is grams for now}
\item{Distance_column}{Name of the column where the distance travelled by the chuck is stored. The expected unit is mm for now}
\item{sep}{Separator to be passed to \link{read.table}}
\item{use_fill_for_read_table}{Set the fill argument to \link{read.table}, used internally. Provide TRUE if there are incomplete lines in the .tab files that should be completed with empty columns}
\item{...}{Additional arguments to be passed to \link{smooth_texture_analyzer_data}. These arguments should be named according to the arguments given in \link{smooth_texture_analyzer_data} } 
}
\details{
The function will run through all the lines of \code{file_info} and read the file specified by the file and folder columns (which need to be located within folder given by the \code{root_folder} argument). To do so, it uses \link{read_texture_analyzer_tab}, and, if \code{do_smoothing} is \code{TRUE}, \link{smooth_texture_analyzer_data} as well.  If \code{do_plot} is true, it will plot the data for each line as well. The \code{lines_to_read} argument can be used to restrict the file reading to certain lines; this is primarily for debugging purposes, as the function may take quite a long time if there are many entries in \code{file_info}.
}
\value{
A list with as many elements as there are rows in \code{file_info}. Each element in the list is the output of \link{read_texture_analyzer_tab},  optionally processed by \link{smooth_texture_analyzer_data}
}

\author{
Thomas Braschler, Patrick Burch
}



\examples{
path = system.file('sampleData/2016_08_30_Patrick_sample', package = 'textureAnalyzerGels');
CMC_concentration_file_info = read.xls(file.path(path,"file_listing.xlsx") )
CMC_concentration_data=read_texture_analyzer_tab_list(file_info=CMC_concentration_file_info,root_folder=path,sd=0.05,boundary_extension_mm=0.2,lm_region_upper_mm=0.05)


}
\keyword{ misc }

