\name{ogden_model_energy}
\alias{ogden_model_energy}

\title{
ogden_model_energy
}
\description{
Elastic stress energy as a function of the material stretch, according to the Ogden model (Ogden 1972). This is the integral of the Ogden stress (as given by \link{ogden_model}).
}
\usage{
ogden_model_energy(mu,alpha,stretch)
}

\arguments{
  \item{mu}{
Vector of Young moduli at zero stretch (see details)
}
  \item{alpha}{
Vector of coefficients
}

\item{stretch}{
Stretch at which the Young modulus should be evaluated
}

}

\value{
Young modulus at the given stretch values
}

\details{
The integration constant for the stress energy to be 0 at the undeformed state (stretch=1 or equivalently stress=0).
}

\author{
Thomas Braschler
}


\examples{

stretch=seq(from=1,to=2,by=0.01)
stress=ogden_model_energy(mu=c(1e3,1e3),alpha=c(6,-3),stretch=stretch)

E=stress/(stretch-1)

plot(E ~ stretch)

}

\keyword{ misc }

\references{Ogden, R. W. (1972), Large Deformation Isotropic Elasticity - On the Correlationof Theory and Experiment for Incompressible Rubberlike Solids,Proceedings of theRoyal Society A: Mathematical, Physical and Engineering Sciences326(1567), 565?584.}

