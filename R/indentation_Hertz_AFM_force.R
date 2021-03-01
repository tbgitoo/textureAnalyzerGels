indentation_Hertz_AFM_force <-
function(E_substrate=1e8,nu_substrate=0.5,force=20e-9,R_tip=8e-9)
{
	
	
	K=4/3/(1-nu_substrate^2)*E_substrate
	
	a=(force*R_tip/K)^(1/3)
	
	h_indentation=a^2/R_tip
	
	return(h_indentation)
	
	
	
	
}

