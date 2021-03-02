\name{read_mecmesin_tab_list}
\alias{read_mecmesin_tab_list}

\title{
read_mecmesin_tab_list
}
\description{
Reads a series of texture analyzer tab files
}
\usage{
read_mecmesin_tab_lis(file_info,root_folder=getwd(),folder_column="Folder",file_column="File",sample_diameter_column="diameter_mm",do_plot=TRUE,do_smoothing=TRUE,lines_to_read=NULL,downup=TRUE,...)
}

\arguments{
  \item{file_info}{
Dataframe describing the files to be read. Needs to contain at least the following columns: 1. A folder column, describing the sub-folder where the individual files are located. The name of this column is given by the argument \code{folder_column}. 2. A file column indicating the filename of the file to be read. The name of this column is given by \code{file_column}. 3. A sample or gel diameter column, where the diameter of the sample or gel is indicated (whichever is smaller, basically). The name of this column is given by the \code{sample_diameter_column} argument.
}
\item{root_folder}{Common root folder where all the tab-files to be read are located, defaults to the current working directory as given by \link{getwd}.}
  \item{folder_column}{
Name of the column in \code{file_info} that contains the folder location.
}
\item{file_column}{Name of the column in \code{file_info} that contains the filename.}
\item{sample_diameter_column}{Name of the column in \code{file_info} that contains the relevant sample/gel diameters. The sample or gel diameters should be in millimeters.}
\item{do_plot}{Indicates whether or not plots should be drawn while reading the files}
\item{do_smoothing}{Indicates whether or not smoothing by \link{smooth_texture_analyzer_data} should be performed when reading the files}
\item{lines_to_read}{Possibility to restrict the lines to be read, a numerical vector indicating the lines to be read.}
\item{downup}{sample displacement down (compression) followed by equal movement up (release)?}
\item{...}{Additional arguments to be passed to \link{smooth_texture_analyzer_data}. These arguments should be named according to the arguments given in \link{smooth_texture_analyzer_data} } 
}
\details{
The function will run through all the lines of \code{file_info} and read the file specified by the file and folder columns (which need to be located within folder given by the \code{root_folder} argument). To do so, it uses \link{read_mecmesin_tab}, and, if \code{do_smoothing} is \code{TRUE}, \link{smooth_texture_analyzer_data} as well.  If \code{do_plot} is true, it will plot the data for each line as well. The \code{lines_to_read} argument can be used to restrict the file reading to certain lines; this is primarily for debugging purposes, as the function may take quite a long time if there are many entries in \code{file_info}.
}
\value{
A list with as many elements as there are rows in \code{file_info}. Each element in the list is the output of \link{read_mecmesin_tab},  optionally processed by \link{smooth_texture_analyzer_data}
}

\author{
Thomas Braschler, Patrick Burch
}



\examples{
# The data for this example is taken from https://doi.org/10.5281/zenodo.4441090 (Filippova et al. 2021), Supplementary_Fig_S6.zip, sample2 and sample3 for the traction test. From there, we copied the file "raw-TensileStrenght(peellike)(V1)-Modified Version-sample-2.txt" and "raw-TensileStrenght(peellike)(V1)-Modified Version-sample-3.txt" data files, as well as the file listing "files_stretch.xlsx" for these two data files here to serve as an example.
path = system.file('sampleData/mecmesin', package = 'textureAnalyzerGels');
tensile_test_file_info = read.xls(file.path(path,"files_stretch.xlsx") )
tensile_test_data=read_mecmesin_tab_list(tensile_test_file_info,root_folder=path,do_smoothing=TRUE,do_plot=TRUE,downup=FALSE,sd=0.1)


}
\keyword{ misc }

\references{
 Filippova A, Bonini F, Efremov L, Locatelli M, Preynat-Seauve O, Beduer A, Krause KH, Braschler T, 2021: Neurothreads: development of supportive carriers for mature dopaminergic neuron differentiation and implantation, Raw data set: https://doi.org/10.5281/zenodo.4441090 accompanying the corresponding scientific publication at Biomaterials (https://doi.org/10.1016/j.biomaterials.2021)
}

