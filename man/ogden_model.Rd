\name{ogden_model}
\alias{ogden_model}

\title{
ogden_model
}
\description{
Elastic stress as a function of the material stretch, according to the Ogden model
}
\usage{
ogden_model(mu,alpha,stretch)
}

\arguments{
  \item{mu}{
Vector of Young moduli at zero stretch (see details)
}
  \item{alpha}{
Vector of coefficients
}

\item{stretch}{
Stretch at which the stress should be evaluated
}

}

\value{
Stress at the given stretch values
}

\details{
The Ogden model (Ogden 1972) is a power model for fitting Young moduli as a function of the material stretch (stretch = delta(L) / L = 1+strain). The formula can be expressed variously, we use here :\cr\cr
\code{Stress=SUM mu*((stretch^alpha)-1)/alpha}\cr\cr
The reason for this particular choice is that this causes the contribution of the different powers (\code{alpha}) to the Young modulus (d(stress)/d(strain)) for the undeformed material (\code{stretch=1}) to depend solely on the \code{mu} but not \code{alpha} values :\cr
\code{lim(E,stretch=1)=SUM mu} \cr
Further, \code{stress=0} for \code{stretch=0}.
}

\author{
Thomas Braschler
}


\examples{

stretch=seq(from=1,to=2,by=0.01)
stress=ogden_model(mu=c(1e3,1e3),alpha=c(6,-3),stretch=stretch)
# Secant Young modulus
E=stress/(stretch-1)

plot(E ~ stretch)

}

\keyword{ misc }

\references{Ogden, R. W. (1972), Large Deformation Isotropic Elasticity - On the Correlation of Theory and Experiment for Incompressible Rubberlike Solids,Proceedings of theRoyal Society A: Mathematical, Physical and Engineering Sciences, 326(1567), 565-584.}

