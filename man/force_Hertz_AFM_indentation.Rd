\name{force_Hertz_AFM_indentation}
\alias{force_Hertz_AFM_indentation}

\title{
force_Hertz_AFM_indentation
}
\description{
Calculation of the force necessary to indent an AFM tip of radius \code{R_tip} into a flat substrate to a depth of \code{h_indentation}. \cr
This is the classical Hertzian sphere indentation model (Popov 2017). 
}
\usage{
force_Hertz_AFM_indentation(E_substrate=1e8,nu_substrate=0.5,h_indentation=20e-9,R_tip=8e-9)
}

\arguments{
  \item{E_substrate}{
Young modulus of the substrate to be compressed
}
  \item{nu_substrate}{
Poisson ratio of the substrate to be compressed
}

\item{h_indentation}{
Total depth of indentation. This is the indentation due to the physical contact plus the free space due to the deformation of the neighbouring substrate surface. In the Hertz approximation, the two contributions are exactly equal.
}

\item{R_tip}{Tip radius}

}

\value{
Force necessary to produce the desired indentation.
}

\details{

The formula used here applies to an indentation by a spherical indentor into a substrate of isotropic, homogeneous composition occupying the lower half-plane (Popov 2017).

}

\author{
Thomas Braschler
}


\examples{

force_Hertz_AFM_indentation()

}

\references{
Popov, L. V. Contact Mechanics and Friction, Springer, Berlin-Heidelberg, 2017
}

\keyword{ misc }

