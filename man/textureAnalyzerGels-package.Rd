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
 For cryogels of sufficient interconnectivity and porosity, foam mechanical analysis may be appropriate. Under certain circumstances (among them, sufficient porosity and interconnectivity), these gels have a mechanical behaviour akin to classical foam-like materials in compression (stress-strain diagram): there is an initial elastic compression segment, followed by a wall-buckling plateau, followed by steeply increasing stress in densification (Gibson 1997). For such gels, mechanical analysis in terms of foam-mechanics is most suitable. In this case, the workflow can be continued as follows: \cr
1. \link{foam_mechanical_analysis} respectively \link{foam_mechanical_analysis_list} for an set of measurements can be used to extract characteristic points and data from the compression / relaxation curves\cr
2. \link{write_foam_mechanical_analysis_list_to_xlsx} can be used to write the extracted data to an Excel file for external analysis. \cr
3. \link{process_foam_mechanical_analysis_list_to_dataframe} processes the nested list output given by \link{foam_mechanical_analysis_list} into a more standard R dataframe from which the desired values can be aggregated and plotted in more standard R manner.\cr\cr

For classical hydrogels without or with limited macroporosity, no plateau exists, and other analysis is appropriate (for example, simple linear fitting (\link{lm}), or analysis in more sophisticated frameworks such as Ogden model (Ogden 1972, \link{ogden_model}).


}
\author{
Patrick Burch\cr
Marina Braschler\cr
Thomas Braschler\cr\cr

Maintainer: Thomas Braschler <thomas.braschler@gmail.com>

}
\keyword{ package }
\examples{
\dontrun{
path = system.file('sampleData/2016_08_30_Patrick_sample', package = 'textureAnalyzerGels');
CMC_concentration_file_info = read.xls(file.path(path,"file_listing.xlsx") )
CMC_concentration_data=read_texture_analyzer_tab_list(file_info=CMC_concentration_file_info,root_folder=path,sd=0.05,boundary_extension_mm=0.2,lm_region_upper_mm=0.05,do_plot=FALSE)
analysis_CMC_concentration_list=foam_mechanical_analysis_list(CMC_concentration_file_info,CMC_concentration_data)
analysis_CMC_concentration_dataframe=analysis_list_to_dataframe(CMC_concentration_file_info,analysis_CMC_concentration_list)


# Plotting the initial Young moduli on the elastic segment before the plateau. The Young moduli depend strongly on the exact cryo-synthesis conditions (here, in 1mL syringes); and also,
# the initial Young moduli are higher than linear fits that include plateau effects (c.f. Beduer 2015)
plot(value ~ CMC_conc, analysis_CMC_concentration_dataframe[analysis_CMC_concentration_dataframe$direction=="down" & analysis_CMC_concentration_dataframe$measurement=="E" & analysis_CMC_concentration_dataframe$point=="elastic" ,],xlim=c(0,3),ylim=c(0,2e4),cex=2,xlab="CMC concentration [%]", ylab="Young modulus [Pa]")
}
}


\references{Gibson, L. J. & Ashby, M. F. Cellular Solids: Structure and properties. (Cambridge University Press, 1997). \cr\cr
Ogden, R. W. (1972), Large Deformation Isotropic Elasticity - On the Correlation of Theory and Experiment for Incompressible Rubberlike Solids,Proceedings of theRoyal Society A: Mathematical, Physical and Engineering Sciences, 326(1567), 565-584.\cr\cr
Beduer A, Braschler T, Peric O, Fantner GE, Mosser S, Fraering PC , Bencherif SA, Mooney DJ, Renaud P: A compressible scaffold for minimally invasive delivery of large intact neuronal networks,  Adv Healthc Mater, 2015, 4(2):301-12, https://doi.org/10.1002/adhm.201400250

 
}
