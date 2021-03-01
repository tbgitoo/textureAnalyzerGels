young_modulus_thin_film_correction <- function(R_indentor=20e-9,h_gel=100e-9,indentation_depth=40e-9)
{
	w = (R_indentor*indentation_depth/h_gel^2)^(3/2)
	
	alpha = 10.05-0.63*sqrt(h_gel/R_indentor)*(3.1+h_gel^2/R_indentor^2)
	
	beta = 4.8 - 4.23*h_gel^2/R_indentor^2
	
	phi = (1+2.3*w)/(1+1.5*w^(1/3)+alpha*w+beta*w^2)
	
	phi[phi<=0]=NA
	
	phi[phi<1]=1
	
	
	
	return(phi)
	
				   
				   
	
	
	
	
}

