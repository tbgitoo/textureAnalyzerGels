\name{young_modulus_thin_film_correction}
\alias{young_modulus_thin_film_correction}

\title{
young_modulus_thin_film_correction
}
\description{
Correction factor for thin films in the Hertz model
}
\usage{
young_modulus_thin_film_correction(R_indentor=20e-9,h_gel=100e-9,indentation_depth=40e-9)
}

\arguments{
  \item{R_indentor}{
Radius of the spherical indentor
}
  \item{h_gel}{
Height of the gel
}

\item{indentation_depth}{
Indentation depth
}

}

\value{
The correction for the apparent Young modulus as calculated by the Hertz model. NA for inappropriate values of the argument
}

\details{

This function calculates the no-slip case as proposed by Long 2011.


}

\author{
Thomas Braschler
}


\examples{

young_modulus_thin_film_correction()

}

\keyword{ misc }


\references{
Rong Long,Matthew S. Hall,Mingming Wu, and Chung-Yuen Hui: Effects of Gel Thickness on Microscopic Indentation Measurements
of Gel Modulus, Biophysical Journal Volume 101, 2011, 643-650.
}


