pore_radius_from_water_diffusion <- function(D=1e-9,nu=1e-3,E=1e4)
{
	return(sqrt(D*8*nu/E))

}