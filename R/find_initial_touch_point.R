find_initial_touch_point <-
function(theData,approximate_touch_point=NULL,free_region=NULL)
{
	if(is.null(approximate_touch_point))
	{
		approximate_touch_point=0.1*max(theData$Distance)	
	}
	if(is.null(free_region))
	{
		free_region = approximate_touch_point/2
	}
	
    # Use the free region as a baseline region
    
    selector_down_first_cycle = rep(TRUE,length(theData$direction))
    
    if(any(theData$direction=="down"))
    {
        selector_down_first_cycle=theData$direction=="down"
    }
    
    if("cycle" %in% colnames(theData))
    {
        selector_down_first_cycle = selector_down_first_cycle & (theData$cycle==1)
        
        
    }
    
    selector_free = (theData$Distance<free_region) & selector_down_first_cycle
    
    selector_touch = theData$Distance>free_region & selector_down_first_cycle
    
    
    
    free_lm=lm(Force ~ Distance, theData[selector_free,])
	
    # Departure from the behavior in the free region should be positive, and at least 5x the maximum error in the free region
    
    max_error_free = max(theData$Force[selector_free]-predict(free_lm))
    
    # Now, find the first point 10x above the max error
    
    certainly_touching=min((theData$Distance[selector_touch])[theData$Force[selector_touch]>predict(free_lm,newdata=theData[selector_touch,])+10*max_error_free])
    if(certainly_touching==Inf) {certainly_touching = approximate_touch_point}
    
    # Now, going back, find the point where we go below the max_error again
    
    limiting_point = max((theData$Distance[selector_down_first_cycle])[theData$Distance[selector_down_first_cycle] < certainly_touching & theData$Force[selector_down_first_cycle]<=predict(free_lm,newdata=theData[selector_down_first_cycle,])+5*max_error_free])
    
    touching_lm = lm(Force ~ Distance, theData[selector_down_first_cycle & theData$Distance>=limiting_point & theData$Distance <= 2*certainly_touching-limiting_point,])
    
    A_touch = coefficients(touching_lm)[["Distance"]]
    B_touch = coefficients(touching_lm)[["(Intercept)"]]
    
    A_free = coefficients(free_lm)[["Distance"]]
    B_free = coefficients(free_lm)[["(Intercept)"]]
	
	intersection_distance=(B_free-B_touch)/(A_touch-A_free)
	
	return(intersection_distance)
	
}

