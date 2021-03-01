find_landmarks_gel_compression <-
function(theData,approximate_gel_touch_point=NULL,gel_thickness=NULL)
{
	touch_point=find_initial_touch_point(theData,approximate_touch_point=approximate_gel_touch_point)
	
	elastic_limit=find_elastic_limit(theData,touch_point=touch_point)
	
	densification_limit=find_densification_limit(theData,touch_point,elastic_limit,gel_thickness=gel_thickness)
	
	ret = vector(mode="numeric",length=3)
	
	names(ret)=c("touch_point","elastic_limit","densification_limit")
	
	ret["touch_point"]=touch_point
	ret["elastic_limit"]=elastic_limit
	ret["densification_limit"]=densification_limit
	
	return(ret)
	
		
}

