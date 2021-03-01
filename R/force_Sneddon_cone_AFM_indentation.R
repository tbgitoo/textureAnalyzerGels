force_Sneddon_cone_AFM_indentation <-
function(E_substrate=1e8,nu_substrate=0.5,h_indentation=20e-9,alpha_cone=20/180*pi)
{
	
	K=E_substrate/(1-nu_substrate^2)
	
	F=K*2*tan(alpha_cone)/pi*h_indentation^2
	
	return(F)
	
	
	
	
}

