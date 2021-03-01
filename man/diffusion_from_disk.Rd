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

where \code{tau=D*t/r^2} and where the \code{jon} terms indicate the n-th zero of the Bessel function of the first kind, order 0 (e.g. \code{J0}). The 4 arises due to the particular value of the infinite series for \code{tau=0}, where the exponential terms all evaluate to 1 (c.f. \link{http://latt.if.usp.br/cgi-bin-delyra/cntsnd?scientific-pages/otsoiepozorbf/Text.pdf}).

The time-dependency of the remaining solute in a disk was used to evaluate hydrogel pore size from water loss under compressive load Braschler et al. (2015).

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

\references{E. Keil, 13.12.1971, Solution of the diffusion equation in cylindrical coordinates and comparison with experiments, http://cds.cern.ch/record/1129859/files/CM-P00065889.pdf \cr\cr
Wolfram web ressources: http://mathworld.wolfram.com/HeatConductionEquationDisk.html \cr\cr
Jorge L. deLyra, On the Sums of Inverse Even Powers ofZeros of Regular Bessel Functions, http://latt.if.usp.br/cgi-bin-delyra/cntsnd?scientific-pages/otsoiepozorbf/Text.pdf, https://arxiv.org/abs/1305.0228 \cr\cr
Braschler T, Songmei W, Wildhaber F, Bencherif SA, Mooney DJ, "Soft nanofluidics governing minority ion exclusion in charged hydrogels", Soft Matter, Issue 20, 2015, Supplementary 2.}
	 

