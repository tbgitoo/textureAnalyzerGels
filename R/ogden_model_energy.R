ogden_model_energy<-function(mu,alpha,stretch)
{
	stress = 0
	
	for(ind in 1:max(length(alpha),length(mu)))
	{
		current_mu = mu[min(ind,length(mu))]
		current_alpha = alpha[min(ind,length(alpha))]
		stress = stress + (stretch^(current_alpha+1)/(current_alpha+1)-stretch-1/(current_alpha+1)+1)*current_mu/current_alpha
	}
	
	return(stress)
}