\name{pore_radius_from_water_diffusion}
\alias{pore_radius_from_water_diffusion}

\title{
pore_radius_from_water_diffusion
}
\description{
Estimation of the pore size in a hydrogel from the apparent water diffusion coefficient
}
\usage{
pore_radius_from_water_diffusion(D=1e-9,nu=1e-3,E=1e4)
}
\arguments{
  \item{D}{
 Apparent diffusion coefficient (can be obtained from stress relaxation experiments in covalently crosslinked gels)
}
\item{nu}{Viscosity of the pore fluid}
  \item{E}{
Young modulus after stress relaxation (typically, half of the Young modulus before stress relaxation
}
}
\details{
This function is based on a direct comparison of water diffusion under compression in a hydrogel and Poiseuille's law to obtain an order-of magnitude estimate for pore sizes in highly porous media.\cr\cr
Covalently crosslinked hydrogel present viscoelastic behaviour which is almost exclusively due to water displacement in the pores. Indeed, under reasonable deformation, one does not expect plastic deformation of the covalently crosslinked gel skeleton itself, only displacement of water pore volume and local stretching or folding of the polymer chains. This gives rise to a characteristic stress relaxation behavior, where the gel is incompressible on short time scales, but compressible by loss of pore water at long time scales. On a fundamental level, the pore pressure is due to volume load with pore medium  :\cr\cr
\code{P_pore = E*phi} \cr\cr
where \code{E} is the bulk elastic modulus (technically, given as the drained bulk modulus by \code{E/3/(1-2*poisson_ratio)}, but which is about the same as the Young modulus \code{E} for typical materials with a Poisson ratio of about 0.3) , whereas \code{phi=delta(V)/V} is the relative volume load with pore medium. In a relaxation experiment, \code{phi} is initially given by the rapid compression, but then drops to \code{phi=0} as the gel equilibrates by expulsion of pore solution. Darcy's law gives the flow velocity \code{v} due to the ensuing pressure gradient:\cr\cr
\code{v = K/nu * (dP_pore/dx) = K*E/nu *(dphi/dx)} \cr\cr
where \code{K} is the Darcy permeability (in m^2), and nu the viscosity of the pore medium (in Pa*s). Since \code{v} corresponds to the transport of excessive pore fluid, we can write \code{v= dphi/dt * 1/A}, such that the equivalence with a water diffusion law becomes evident:\cr\cr
\code{dphi/dt = A*(K*E/nu)*(dphi/dx)} \cr\cr
and therefore\cr\cr
D=K*E/nu\cr\cr
We can approximately relative the Darcy-permeability coefficient \code{K} to the pore size, by considering \code{N} parallel channels. The volume flow rate of \code{N} parallel channels, occupying a total cross-sectional area \code{A}, is given by Poiseuille's law:\cr\cr
\code{dphi/dt = N*pi*r^4/8/nu*(dP_pore/dx)}\cr\cr
If we set N=A/(pi*r^2), this becomes: \cr\cr
\code{dphi/dt = A*r^2/8/nu*(dP_pore/dx)   => }\cr
\code{v=r^2/8/nu*(dP_pore/dx) }\cr\cr
which, by comparison with Darcy's law, relates the Darcy permeability coefficient to the pore size by:\cr\cr
\code{K=r^2/8}\cr\cr
Finally, if we now \code{D},\code{E} and \code{nu}, we can estimate the pore size by:\cr\cr
\code{r=sqrt(8*K)=sqrt(8*D*nu/E)}\cr\cr
This is the formula used by the function to provide an estimate of the pore radius from the apparent water diffusion coefficient \code{D}.\cr\cr

Experimentally, a rapid applicaton of a uniaxial strain (\code{epsilon_z}) in a sample with homogeneous height and shape in the z-direction and with free lateral boundaries  can be used to introduce an initially homogeneous volume loading \code{phi}. Typically, such a sample is a disk, placed between a substrate and a piston for loading on its flat faces. On a short time scale, hydrogels are typically nearly incompressible, such that the total volumetric strain is 0: \code{epsilon_z+epsilon_x+epsilon_y=0} and therefore \code{epsilon_x + epsilon_y = -epsilon_z}. The resulting stress in the z-direction is balanced by the loading piston, but the planar stress \code{sigma_xy=E*(epsilon_x+epsilon_y)=-E*epsilon_z} must be balanced by the pore pressure, such that we have: \code{P_pore = -E*epsilon_z} everywhere in the sample. In terms of volume loading, this can be interpreted as having a homogeneous, and controlled excess of pore medium \code{phi=-epsilon_z} everywhere. The apparent Young modulus under these conditions is \code{2*E}, since the piston must sustain both the stress due to \code{epsilon_z} and the pore pressure.\cr\cr
During the hydrogel relaxation, the excess pore water diffuses out of the gel, and ultimately, \code{P_pore=0}, with a drop of the apparent Young modulus by a factor of 2. Precise knowledge of the gel geometry can be used to estimate a diffusion coefficient for the disappearance of the excess pore water from the stress relaxation characteristics (for instance, by using \link{diffusion_from_disk}) for a disk-shaped sample. The present function can then be used to estimate the hydrogel pore size.
}
\value{
Estimation of the pore radius
}


\author{
Thomas Braschler
}
\examples{
pore_radius_from_water_diffusion(D=1e-9,nu=1e-3,E=1e4)
}
\keyword{ misc }

