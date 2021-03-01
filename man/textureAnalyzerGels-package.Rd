\name{textureAnalyzerGels-package}
\alias{textureAnalyzerGels-package}
\alias{textureAnalyzerGels}
\docType{package}
\title{
textureAnalyzerGels
}
\description{
Reading and analyzing files written by the Exponent software of the Texture Analyzer. The Texture Analyzer compresses materials, and records Force - Distance curves. This package reads these force distance curves from tabulated text files exported from the Exponent software, and also contains routines for basic analysis of the data, namely in terms of the Young modulus. \cr\cr



}
\details{
A typical workflow for reading mechanical compression data into R is: \cr
1. Carry out mechanical measurement. Typically, a measurement is a single uniaxial compression/release cycle, with a maximum compression to some 20 percent of the original height to avoid mechanical damage. To reduce the influence of capillary pressure and fluid retention, the measurement is carried out with the sample immersed. The piston displacement speed should be as low as possible to reduce the influence of viscous drag of the liquid in the pore space. We use a TextureAnalyzer machine from Stable Microsystems for this purpose \cr
2. Export data in tab format. Keep the maximum number of decimal places and time points. \cr
3. Read the data into R. For reading a single file, \link{read_texture_analyzer_tab} can be used, but more generally, one will tabulate file information and metadata in a spreadsheet, and use the spreadsheet in R (see \link{read.xls} to direct the lecture of a multitude of .tab file with \link{read_texture_analyzer_tab_list}.\cr
4. For soft gels in particular, the mechanical noise can be relatively high. It is therefore advisable to carry out low-pass filtering to reduce noise. This can be done explicitly with \link{smooth_texture_analyzer_data}. If \link{read_texture_analyzer_tab_list} is used, this is more typically done directly by \link{read_texture_analyzer_tab_list}. \cr
In this workflow, the smoothing steps are especially slow, and for a larger collection of files, it is clearly advisable to write the smoothened curves into R datafiles. For this, the \link{save} command can be used.\cr\cr

Analysis depends on the type of gel from which one wants to extract information.\cr\cr
 For cryogels, foam mechanical analysis is appropriate. The workflow can be continued as follows: \cr
1. \link{foam_mechanical_analysis} respectively \link{foam_mechanical_analysis_list} for an set of measurements can be used to extract characteristic points and data from the compression / relaxation curves\cr
2. \link{write_foam_mechanical_analysis_list_to_xlsx} can be used to write the extracted data to an Excel file for external analysis. \cr
3. \link{process_foam_mechanical_analysis_list_to_dataframe} processes the nested list output given by \link{foam_mechanical_analysis_list} into a more standard R dataframe from which the desired values can be aggregated and plotted in more standard R manner.\cr\cr

For classical hydrogels


}
\author{
Thomas Braschler

Maintainer: Thomas Braschler <thomas.braschler@gmail.com>

}
\keyword{ package }
\examples{
\dontrun{
path = system.file('sampleData/2016_08_30_Patrick_sample', package = 'textureAnalyzerGels');
CMC_concentration_file_info = read.xls(file.path(path,"file_listing.xlsx") )
CMC_concentration_data=read_texture_analyzer_tab_list(file_info=CMC_concentration_file_info,root_folder=path,sd=0.05,boundary_extension_mm=0.2,lm_region_upper_mm=0.05)
}
}
