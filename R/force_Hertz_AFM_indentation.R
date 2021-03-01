force_Hertz_AFM_indentation <-
function(E_substrate=1e8,nu_substrate=0.5,h_indentation=20e-9,R_tip=8e-9)
{
	a=sqrt(R_tip*h_indentation)
	
	K=4/3/(1-nu_substrate^2)*E_substrate
	
	F=K*a^3/R_tip
	
	return(F)
	
	
	
	
}

