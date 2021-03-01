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
The Ogden model is a polynomial model for fitting Young moduli as a function of the material stretch (stretch is delta(L) / L). The formula is:\cr\cr
\code{Stress=SUM mu*(stretch^alpha-1)/alpha}\cr\cr
By considering the Bernoulli linearization, one finds that the Young modulus for the undeformed material is: (\code{stretch=1}) is:\cr\cr
\code{lim(E,stretch=1)=SUM mu}
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

