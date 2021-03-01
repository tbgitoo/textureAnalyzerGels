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
Stretch at which the Young modulus should be evaluated
}

}

\value{
Young modulus at the given stretch values
}

\details{
The Ogden model is a polynomial model for fitting Young moduli as a function of the material stretch (stretch = delta(L) / L = 1+strain). The formula can be expressed variously, for uniaxial compression it is essentially a polynomial. The formula used here is :\cr\cr
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

E=stress/(stretch-1)

plot(E ~ stretch)

}

\keyword{ misc }

