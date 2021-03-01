ogden_model_symmetric_energy<-function(mu,alpha,stretch)
{
	return(ogden_model_energy(c(mu,mu),c(alpha,-alpha),stretch))
}
