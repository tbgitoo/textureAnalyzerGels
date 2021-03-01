\name{fit_young_modulus_polymer_from_bulk}
\alias{fit_young_modulus_polymer_from_bulk}

\title{
fit_young_modulus_polymer_from_bulk
}
\description{
Estimation of the intrinsic Young modulus of the polymer phase in a cryogel from the bulk Young modulus
}
\usage{
fit_young_modulus_polymer_from_bulk(polymer_volume_fraction,E_bulk,return_model=FALSE)
}

\arguments{
  \item{polymer_volume_fraction}{
Volume fraction occupied by the polymer phase
}
  \item{E_bulk}{
Apparent Young modulus of the bulk structure as measured by a mechanical test equipment with a chuch size much larger than the pore size
}
\item{return_model}{If TRUE, return the \link{nls} model used for fitting as an attribute to the result (e.g. \link{attr}(result,"model"))}

}

\value{
Estimation of the microscopic Young modulus of the polymer phase
}

\details{
The model used for the estimation is explained in more detail in \link{young_modulus_cryogel_theory}; it is given by \code{E_bulk = E_polymer*polymer_volume_fraction^3}, as described in Beduer et al., 2015. The fitting is carried out by taking the log of both the \code{polymer_volume_fraction} and \code{E_bulk}, and the by linear fitting of a straight line of slope 3. \cr\cr
If both \code{polymer_volume_fraction} and \code{E_bulk} are numeric (length=1), the calculation is carried out exactly.

}

\author{
Thomas Braschler
}


\examples{

fit_young_modulus_polymer_from_bulk(0.01,1e3)

fit_young_modulus_polymer_from_bulk(c(0.01,0.02,0.03),c(0.2e3,0.7e3,2e3))


}

\keyword{ misc }

\references{
Beduer A, Braschler T, Peric O, Fantner E, Mosser S, Fraering PC, Bencherif SA, Mooney DJ, Renaud P: A Compressible Scaffold for Minimally Invasive Delivery of Large Intact Neuronal Networks, Advanced Healthcare Materials, 2015, 4(2), 301-312
}

