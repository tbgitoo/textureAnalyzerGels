\name{pore_size_from_hydrodynamic_pressure}
\alias{pore_size_from_hydrodynamic_pressure}

\title{
pore_size_from_hydrodynamic_pressure
}
\description{
Order of magnitude estimation of the pore size from stress hysteresis. The idea is to estimate approximately an effective pore size that could explain higher stress by the pressure build-up due to lag in pore fluid evacuation as opposed to underpressure when relaxing the same gel. In general, such differences are large with micro- or nanoporous gels, but very small with macroporous gels and thus difficult to measure at all but very high compression rates in macroporous gels.
}
\usage{
pore_size_from_hydrodynamic_pressure(P_up=500,P_down=1000,gel_thickness=1e-3,v_compression=1e-5,viscosity=1e-3,disk_radius=2e-3)
}
\arguments{
  \item{P_up}{
 Pressure (stress) observed at a given strain during relaxation ("up" movement of the chuck)
}
\item{P_down}{Pressure (stress) observed during compression ("down" movement of the chuck), this is generally larger than \code{P_up} }
  \item{gel_thickness}{
Height of the gel, in meters
}
\item{v_compression}{Speed of the chuck, identical except for direction in down and up movement. Units: m/s}
\item{viscosity}{Viscosity of the pore fluid, in Pa*s}
\item{disk_radius}{Radius of the disk sample, in meters}
}
\details{
This function is based on an order of magnitude estimation. For a single pore, Poiseuille's law of laminar flow resistance in a cylindrical tube gives: \cr
\code{Qsingle_pore = deltaP*pi*r^4/8/viscosity/disk_radius}\cr
where \code{deltaP=P_down-P_up}, and \code{r} is the characteristic pore radius we are looking for. There are many more or less warranted assumptions, such as all pores having the same length of \code{disk_radius}, all pores being cylindrical and of identical radius \code{r} and so forth, but one has to keep in mind that this is an order-of-magnitude estimation only.\cr\cr
For \code{N} pores in parallel, the flow rate is \code{N} times higher; it must at the same time match the pore fluid evacuation rate given by the volume displacement of the chuck:\cr
\code{Qtotal = v_compression*pi*disk_radius^2 = N*Qsingle_pore} \cr
The number of parallel pores can roughly be obtained from the cross section surface, roughly at half the disk radius:\cr	
\code{N = disk_radius*pi*gel_thickness/pi/r^2} \cr
Assembling, we get:
\code{Qtotal = disk_radius*pi*gel_thickness/pi/r^2 * deltaP*pi*r^4/8/viscosity/disk_radius = disk_radius/disk_radius * pi/pi*pi * gel_thickness * r^4/r^2 * deltaP/8/viscosity =}\cr
\code{Qtotal = pi*gel_thickness*r^2*deltaP/8/viscosity = v_compression*pi*disk_radius^2} \cr\cr
The final expression, used in this function, for the pore radius estimation is thus:\cr	
\code{=> r = disk_radius*sqrt(v_compression *8 *viscosity /gel_thickness/deltaP)}
}
\value{
Numerical estimation of the pore radius
}


\author{
Thomas Braschler
}
\examples{
pore_size_from_hydrodynamic_pressure(P_up=500,P_down=1000,gel_thickness=1e-3,v_compression=1e-3,viscosity=1e-3,disk_radius=2e-3)
}
\keyword{ misc }

