find_densification_limit <-
function(theData,touch_point,elastic_limit,gel_thickness=NULL)
{
	if(is.null(gel_thickness))
	{
		gel_thickness = max(theData$Distance-touch_point)
		
	}
	
	direction_to_use = theData$direction[1]
	
	if(any(theData$direction=="down"))
	{
		direction_to_use="down"
	}
	
	# The idea here is to use plateau characteristics to find the densification limit
	# First, get approximatley the plateau segment, from the elastic limit to max. 50% of the gel thickness
	
	dataPlateauSegment = theData[theData$Distance > elastic_limit & theData$Distance < touch_point + gel_thickness/2 & theData$direction==direction_to_use,]
	
	# Second, get the minimum positive slope on this plateau
	
	pressureSlopePlateau = min(abs(dataPlateauSegment$pressureSlope))
	
	# Also get the position where this slope occurs
	minPosPlateau = dataPlateauSegment$Distance[which.min(abs(dataPlateauSegment$pressureSlope))]
	
	# And the level of stress (aka pressure at the minimum)
	minPosPlateauPressure = dataPlateauSegment$pressure[which.min(abs(dataPlateauSegment$pressureSlope))]
	
	
	# The densification limit must be somewhere above the the minimum position
	dataDensificationSegment = theData[theData$Distance > minPosPlateau& theData$direction==direction_to_use,]
	
	# "Reduced" pressure: Measured pressure minus the pressure expected from the plateau minimum and its slope
	dataDensificationSegment$pressure = dataDensificationSegment$pressure-minPosPlateauPressure-(dataDensificationSegment$Distance-minPosPlateau)*pressureSlopePlateau
	
	# Densification criterion: The "reduced" pressure is at least as big as the minimum pressure on the plateau,
	# that is, we are as much above the extrapolated plateau as the plateau pressure itself.
	densification_limit = min(dataDensificationSegment$Distance[dataDensificationSegment$pressure>minPosPlateauPressure])
	
	return(densification_limit)
	
}

