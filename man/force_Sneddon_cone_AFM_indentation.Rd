\name{force_Sneddon_cone_AFM_indentation}
\alias{force_Sneddon_cone_AFM_indentation}

\title{
force_Sneddon_cone_AFM_indentation
}
\description{
Calculation of the force necessary to indent a conical tip into a homogeneous material (Sneddon 1965). This typically used in AFM, but can also be applied to a macroscopic indentor.}
\usage{
force_Sneddon_cone_AFM_indentation(E_substrate=1e8,nu_substrate=0.5,h_indentation=20e-9,alpha_cone=20/180*pi)
}

\arguments{
  \item{E_substrate}{
Young modulus of the substrate to be compressed
}
  \item{nu_substrate}{
Poisson ratio of the substrate to be compressed
}

\item{h_indentation}{
Depth of the cone indentation
}

\item{alpha_cone}{Half-opening angle of the cone}

}

\value{
Force necessary to produce the desired indentation.
}

\details{

The formula used here applies to an indentation by a conical indentor into a substrate of isotropic, homogeneous composition occupying the lower half-plane (Sneddon 1965w).

}

\author{
Thomas Braschler
}


\examples{

force_Sneddon_cone_AFM_indentation()

}

\references{
I. N. Sneddon, International Journal of Engineering Science 1965, 3, 47.
}

\keyword{ misc }

