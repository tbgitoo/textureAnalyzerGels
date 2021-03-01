ogden_model_symmetric<-function(mu,alpha,stretch)
{
	return(ogden_model(c(mu,mu),c(alpha,-alpha),stretch))
}
