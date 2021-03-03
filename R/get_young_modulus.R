get_young_modulus <-
function(theData,approximate_gel_touch_point=NULL,gel_thickness=1,strain=0.1,doplot=FALSE,do_touch_point_estimation=TRUE,subtract_chuck_buoyancy=FALSE)
{
	do_tp=FALSE
	touch_points = vector(mode="numeric",length=length(unique(theData$direction)))
	names(touch_points)=unique(theData$direction)
	
	if(do_touch_point_estimation | is.null(approximate_gel_touch_point))
	{
	  
		do_tp=TRUE
		for(direction in names(touch_points))
		{
		  touch_points[direction]=find_initial_touch_point(theData[theData$direction==direction,],approximate_touch_point=approximate_gel_touch_point) 
		}

	} 	else
	{
		
		touch_points[]=approximate_gel_touch_point
	}
	
	young_modulus = vector(mode="numeric",length=length(touch_points))
	
	names(young_modulus)=names(touch_points)
	
	for(direction in names(touch_points))
	{
	
		upper_limit = touch_points[direction]+gel_thickness*strain
		
		dataToUse = theData[theData$Distance>touch_points[direction] & theData$Distance < upper_limit & theData$direction==direction,]
		
		
		if(subtract_chuck_buoyancy)
		{
			dataToUse$pressure=dataToUse$pressure-(dataToUse$Distance-min(dataToUse$Distance))*9.81
		}
		
		
		if(do_tp)
		{
		
		coeffs<-coefficients(
							lm(pressure ~ Distance,data=dataToUse)
							)
		} else {
			
			dataToUse$Distance = dataToUse$Distance-touch_points[direction]
			initial_pressure=dataToUse$pressure[1]
			dataToUse$pressure = dataToUse$pressure
			
			coeffs<-coefficients(
								 lm(pressure ~ Distance+0,data=dataToUse)
								 )
			
			
			
			coeffs["(Intercept)"]=-touch_points[direction]*coeffs["Distance"]
			
			
		}
		
		
		
			
		young_modulus[direction]=coeffs["Distance"]*gel_thickness 
		
		if(doplot)
		{
			
			xlim<-c(min(theData$Distance),max(theData$Distance))
			
			plot.xy(xy.coords(x=xlim,y=coeffs["(Intercept)"]+coeffs["Distance"]*xlim),type="l",col="blue")
		}
		
		
	}
	
	attr(young_modulus,"touch_points")<-touch_points
	
	return(young_modulus)
	
}

