\name{ogden_model_symmetric_energy}
\alias{ogden_model_symmetric_energy}

\title{
ogden_model_symmetric_energy
}
\description{
Convenience function, giving the elastic stress energy as a function of the material stretch, according to the integrated Ogden model (Ogden 1972), by using symmetric pairs of \code{mu} and \code{alpha}. Uses \link{ogden_model_energy}
}
\usage{
ogden_model_symmetric_energy(mu,alpha,stretch)
}

\arguments{
  \item{mu}{
Vector of Young moduli at zero stretch (see details)
}
  \item{alpha}{
Vector of coefficients
}

\item{stretch}{
Stretch at which the stress energy should be evaluated
}

}

\value{
Stress energy at the given stretch values, obtained by calling \link{ogden_model_energy} via:\cr\cr
\code{ogden_model_energy(c(mu,mu),c(alpha,-alpha),stretch)}
}


\author{
Thomas Braschler
}


\examples{

stretch=seq(from=0.5,to=2,by=0.01)
stress_energy=ogden_model_symmetric_energy(mu=c(1e3),alpha=c(3),stretch=stretch)



plot(stress_energy ~ stretch)

}

\keyword{ misc }

\references{Ogden, R. W. (1972), 'Large Deformation Isotropic Elasticity - On the Correlationof Theory and Experiment for Incompressible Rubberlike Solids',Proceedings of theRoyal Society A: Mathematical, Physical and Engineering Sciences326(1567), 565?584.}


