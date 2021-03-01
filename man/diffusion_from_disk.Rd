\name{diffusion_from_disk}
\alias{diffusion_from_disk}

\title{
diffusion_from_disk
}
\description{
Remaing total fraction of solute in a originally homogeneously loaded disk, with the boundary condition of c=0 on the portour 
}
\usage{
diffusion_from_disk(r,D,t)
}
\arguments{
  \item{r}{
 Radius of the disk
}
\item{D}{Diffusion coefficient}
  \item{t}{
Time since the beginning of the diffusion
}
}
\details{
The general solution of the diffusion equation in cylindrical coordinates can be expressed in terms of Bessel functions. It can for instance be found in \link{http://cds.cern.ch/record/1129859/files/CM-P00065889.pdf}. For a disk with initially homogenous concentrations, the coefficients take a slightly particular form (e.g. \link{http://mathworld.wolfram.com/HeatConductionEquationDisk.html}) such that the integral giving the remaining solute fraction becomes:\cr\cr

\code{B(tau)=4*SUM_n 1/jon^2 * exp(-jon^2*tau)} \cr\cr

where \code{tau=D*t/r^2} and where the \code{jon} terms indicate the n-th zero of the Bessel function of the first kind, order 0 (e.g. \code{J0}). The 4 arises due to the particular value of the infinite series for \code{tau=0}, where the exponential terms all evaluate to 1 (c.f. \link{http://latt.if.usp.br/cgi-bin-delyra/cntsnd?scientific-pages/otsoiepozorbf/Text.pdf})

}
\value{
Fraction of solute still remaining in the disk
}
\seealso{
The function makes use of the \link{bessel_zero_J0} function
}

\author{
Thomas Braschler
}
\examples{
t=seq(from=0,to=1,by=0.01)
remaining=diffusion_from_disk(1,1,t)
plot(remaining~t)
}
\keyword{ misc }

