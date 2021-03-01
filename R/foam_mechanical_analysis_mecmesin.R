foam_mechanical_analysis_mecmesin <-
function(theData,offset_height=0,approximate_gel_height=NULL,stress_column="pressure",stress_slope_column="pressureSlope")
{

    start_height=theData[1,"Distance"]+offset_height 
    theData[,"Distance"]= theData[1,"Distance"]-theData[,"Distance"]	#modifies distance column to be used with old texture analyzer script (distance effectively traveled by the chuck)
    theData[,stress_slope_column]=-theData[,stress_slope_column] # We reverse the direction of the distance, so we need to reverse the slope as well
	if(!is.null(approximate_gel_height))
	{ approximate_gel_touch_point = start_height-offset_height-approximate_gel_height } else { approximate_gel_touch_point=NULL}


    
    return(foam_mechanical_analysis(theData=theData,start_height=start_height,approximate_gel_touch_point=approximate_gel_touch_point,
stress_column=stress_column,stress_slope_column=stress_slope_column))
    
    
    
	
}





