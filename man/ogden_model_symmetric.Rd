\name{ogden_model_symmetric}
\alias{ogden_model_symmetric}

\title{
ogden_model_symmetric
}
\description{
Convenience function, giving the elastic stress as a function of the material stretch, according to the Ogden model (Ogden 1972), by using symmetric pairs of \code{mu} and \code{alpha}. Uses \link{ogden_model}
}
\usage{
ogden_model_symmetric(mu,alpha,stretch)
}

\arguments{
  \item{mu}{
Vector of Young moduli at zero stretch (see details)
}
  \item{alpha}{
Vector of coefficients
}

\item{stretch}{
Stretch at which the stress values should be evaluated
}

}

\value{
Stress at the given stretch values, obtained by calling \link{ogden_model} via:\cr\cr
\code{ogden_model(c(mu,mu),c(alpha,-alpha),stretch)}
}


\author{
Thomas Braschler
}


\examples{

stretch=seq(from=0.5,to=2,by=0.01)
stress=ogden_model_symmetric(mu=c(1e3),alpha=c(3),stretch=stretch)
# Sekant Young model
E=stress/(stretch-1)

plot(E ~ stretch)

}

\keyword{ misc }

\references{Ogden, R. W. (1972), ?Large Deformation Isotropic Elasticity - On the Correlationof Theory and Experiment for Incompressible Rubberlike Solids?,Proceedings of theRoyal Society A: Mathematical, Physical and Engineering Sciences326(1567), 565?584.}


