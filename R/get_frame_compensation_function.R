get_frame_compensation_function<-function(filename)
{
	
	
	frame_compliance_data = read_texture_analyzer_tab(filename)
	
	for_frame_compliance_compensation = frame_compliance_data[frame_compliance_data$direction=="down",c("Distance","Force")]
	
	for_origin=coefficients(lm(Force ~ Distance, for_frame_compliance_compensation[for_frame_compliance_compensation$Force>5 & for_frame_compliance_compensation$Force<20,]))
	
	touch_point = -for_origin["(Intercept)"]/for_origin["Distance"]
	
	x_points = c(touch_point,for_frame_compliance_compensation$Distance[for_frame_compliance_compensation$Distance>touch_point])
	
	y_points = c(0,for_frame_compliance_compensation$Force[for_frame_compliance_compensation$Distance>touch_point])
	
	y_points[y_points<5]=for_origin["(Intercept)"]+x_points[y_points<5]*for_origin["Distance"]
	
	x_points = x_points-touch_point
	
	force_compensation_function<-approxfun(y_points,x_points,rule=2)
	
	return(force_compensation_function)
	
}
