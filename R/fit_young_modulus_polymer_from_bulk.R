fit_young_modulus_polymer_from_bulk <-
function(polymer_volume_fraction,E_bulk,return_model=FALSE)
{
	if(length(E_bulk)==1 & length(polymer_volume_fraction)==1)
	{
		return(E_bulk/polymer_volume_fraction^3)
	}
	E_bulk_log = log(E_bulk)
	polymer_volume_fraction_log = log(polymer_volume_fraction)
	
	nls_model = nls(E_bulk_log ~ 3*polymer_volume_fraction_log+offset,start=list(offset=1))
	
	E_polymer=exp(coefficients(nls_model)[[1]])
	
	if(return_model)
	{
		attr(E_polymer,"model")=nls_model
	}
	
	return(E_polymer)
	
}

