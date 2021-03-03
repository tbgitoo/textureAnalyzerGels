\name{young_modulus_cryogel_theory}
\alias{young_modulus_cryogel_theory}

\title{
young_modulus_cryogel_theory
}
\description{
Theoretical calculation of the Young modulus of a cryogel
}
\usage{
young_modulus_cryogel_theory(polymer_volume_fraction=0.01,E_polymer=10e6,structural_factor=1)
}

\arguments{
  \item{polymer_volume_fraction}{
Volume fraction occupied by the polymer phase (rather than the pore space)
}
  \item{E_polymer}{
Intrinsic Young modulus of the polymer phase
}

\item{structural_factor}{
Factor relating the actual mechanical response to the idealized cantilever situation. On the order of 1 for structurally fully intact gels, and 0.01-0.1 for gels where some of the bridges are not formed due to too low viscosity of the monomer
}

}

\value{
Macroscopic Young modulus of the cryogel
}

\details{
To obtain an order-of-magnitude estimation of the cryogel macroscopic modulus, a following cantilever-based model is used, as described in Beduer 2015 \cr\cr
Each pore structure is represented by a single cantilever of the following dimensions:\cr
- Thickness given by \code{polymer_volume_fraction*pore_size}\cr
- Length of \code{pore_size/2}\cr
- Width given by \code{pore_size/2}\cr\cr

Such a cantilever has a spring constant given by:\cr
\code{k=E_polymer*beam_thickness^3*beam_width/4/beam_length^3}\cr
\code{ =E_polymer*polymer_volume_fraction^3*pore_size^3*pore_size/2/4/(pore_size^3/8)=E_polymer*polymer_volume_fraction^3*pore_size}\cr\cr

The Young modulus can be calculated by considering full compression (e.g., compression by \code{pore_size}), and then reporting it to the area of a pore (e.g., \code{pore_size^2}):\cr
\code{E_cryogel = k*pore_size/pore_size^2=E_polymer*polymer_volume_fraction^3}\cr\cr

Maybe a bit surprisingly the global modulus of the cryogel is independent of the pore size. \cr\cr

In reality, the gels tend to be weaker than the idealized calculation because some of the "bridges" are actually interrupted, and maybe also because the sheet-cantilever model simply supposes a too high connectivity. To account for this, it is possible to supply \code{structural_factor}, which is used to multiply the cryogel Young modulus (\code{E_cryogel = E_polymer*polymer_volume_fraction^3*structural_factor}).

}

\author{
Thomas Braschler
}


\examples{

young_modulus_cryogel_theory()

}

\keyword{ misc }

\references{
Beduer A, Braschler T, Peric O, Fantner GE, Mosser S, Fraering PC , Bencherif SA, Mooney DJ, Renaud P: A compressible scaffold for minimally invasive delivery of large intact neuronal networks,  Adv Healthc Mater, 2015, 4(2):301-12, https://doi.org/10.1002/adhm.201400250
}

