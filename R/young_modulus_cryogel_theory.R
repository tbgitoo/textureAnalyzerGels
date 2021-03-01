young_modulus_cryogel_theory <-
function(polymer_volume_fraction=0.01,E_polymer=10e6,structural_factor=1)
{
	return(E_polymer*polymer_volume_fraction^3*structural_factor)
	
	
	
	
}

