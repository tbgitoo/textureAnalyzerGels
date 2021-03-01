diffusion_from_disk <-
function(r,D,t)
{
	tau = D*t/r^2
	
	B = 0
	
	for(n in 1:1000)
	{
		alpha_n = bessel_zero_J0(n)
		B=B+4/alpha_n^2*exp(-alpha_n^2*tau)
	}
	
	return(B)
}

