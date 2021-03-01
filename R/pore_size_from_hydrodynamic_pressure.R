pore_size_from_hydrodynamic_pressure <-
function(P_up=500,P_down=1000,gel_thickness=1e-3,v_compression=1e-5,viscosity=1e-3,disk_radius=2e-3)
{
	
# Poiseuille flow gives:
	
# Qsingle_pore = deltaP*pi*r^4/8/viscosity/disk_radius
	
# Since the pores are in parallel, we have
	
# Qtotal = v_compression*pi*disk_radius^2 = N*Qsingle_pore
	
# but N can also approximated geometrically
	
# N = disk_radius*pi*gel_thickness/pi/r^2
	
# Plugging in, we get
	
# Qtotal = disk_radius*pi*gel_thickness/pi/r^2 * deltaP*pi*r^4/8/viscosity/disk_radius = disk_radius/disk_radius * pi/pi*pi * gel_thickness * r^4/r^2 * deltaP/8/viscosity =
	
# Qtotal = pi*gel_thickness*r^2*deltaP/8/viscosity = v_compression*pi*disk_radius^2
	
# => r^2/disk_radius^2 = v_compression *8 *viscosity /gel_thickness/deltaP
	
	
# Loduvics formula
	
# Eq = @(x) [(x(1)*10^(-6))^4-((8*visq*Lmoy*Qtot)/(x(2)*pi*max(P))), x(2)-((0.99*h1*pi*2*rd)/((x(1)*10^(-6))^2*pi))];
	
	
# => r^4 = ((8*viscosity*disk_radius*Qtotal)/(N*pi*deltaP))
# => N = gel_thickness*pi*2*disk_radius/(r^2*pi)
# => r^2 = ((8*viscosity*Qtotal)/(pi*deltaP*gel_thickness*2)

	
	deltaP_flow = (P_down-P_up)/2;
	
	pore_radius = disk_radius*sqrt(v_compression*8*viscosity/deltaP_flow/gel_thickness);
	
	return(pore_radius)
	
}

