find_densification_limit <-
function(theData,touch_point,elastic_limit,gel_thickness=NULL)
{
	if(is.null(gel_thickness))
	{
		gel_thickness = max(theData$Distance-touch_point)
		
	}
	
	direction_to_use = theData$direction
	
	if(any(theData$direction=="down"))
	{
		direction_to_use="down"
	}
	
	dataPlateauSegment = theData[theData$Distance > elastic_limit & theData$Distance < touch_point + gel_thickness/2 & theData$direction==direction_to_use,]
	
	pressureSlopePlateau = min(abs(dataPlateauSegment$pressureSlope))
	
	minPosPlateau = dataPlateauSegment$Distance[which.min(abs(dataPlateauSegment$pressureSlope))]
	
	minPosPlateauPressure = dataPlateauSegment$pressure[which.min(abs(dataPlateauSegment$pressureSlope))]
	
	# The corner between plateau and elastic regime is given by the solution of:
	# (x-touch_point)*slopeElastic = (x-minPosPlateau)*pressureSlopePlateau + minPosPlateauPressure
	# => (x-touch_point)=minPosPlateauPressure/(slopeElastic-pressureSlopePlateau)
	# =>
	
	dataDensificationSegment = theData[theData$Distance > minPosPlateau& theData$direction==direction_to_use,]
	
	dataDensificationSegment$pressure = dataDensificationSegment$pressure-minPosPlateauPressure-(dataDensificationSegment$Distance-minPosPlateau)*pressureSlopePlateau
	
	densification_limit = min(dataDensificationSegment$Distance[dataDensificationSegment$pressure>minPosPlateauPressure])
	
	return(densification_limit)
	
}

