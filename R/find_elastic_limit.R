find_elastic_limit <-
function(theData,touch_point,gel_thickness=NULL)
{
	if(is.null(gel_thickness))
	{
		gel_thickness = max(theData$Distance-touch_point)
		
	}
	
		
	elastic_limit_initial_guess = touch_point+0.2*gel_thickness
	
	slope_max = max(theData$pressureSlope[theData$Distance > touch_point & theData$Distance < elastic_limit_initial_guess])
	
	position_max = which.max(theData$pressureSlope[theData$Distance > touch_point & theData$Distance < elastic_limit_initial_guess])
	
	distance_max = (theData$Distance[theData$Distance > touch_point & theData$Distance < elastic_limit_initial_guess])[position_max]
	
	elastic_limit_refined_guess=touch_point+2*(distance_max-touch_point)
	
	direction_to_use = theData$direction[1]
	
	if(any(theData$direction=="down"))
	{
		direction_to_use = "down"
	}
	
	dataElasticSegment = theData[theData$Distance > touch_point & theData$Distance < elastic_limit_refined_guess & theData$direction==direction_to_use,]
	
	dataElasticSegment$Distance = dataElasticSegment$Distance-touch_point
	
	weightsElasticSegment = abs(dataElasticSegment$pressureSlope+max(abs(dataElasticSegment$pressureSlope))/20)^2
	
	weightsElasticSegment = weightsElasticSegment/max(weightsElasticSegment)
	
	linmodElasticSegment = lm(pressure ~ Distance+0,dataElasticSegment,weights=weightsElasticSegment)
	
	slopeElastic = coefficients(linmodElasticSegment)[[1]]
	
	maxElasticPoint = dataElasticSegment$Distance[which.max(dataElasticSegment$pressureSlope)]
	
	dataPlateauSegment = theData[theData$Distance > elastic_limit_refined_guess & theData$Distance < touch_point + gel_thickness/2 & theData$direction==direction_to_use,]
	
	pressureSlopePlateau = min(abs(dataPlateauSegment$pressureSlope))
	
	minPosPlateau = dataPlateauSegment$Distance[which.min(abs(dataPlateauSegment$pressureSlope))]
	
	minPosPlateauPressure = dataPlateauSegment$pressure[which.min(abs(dataPlateauSegment$pressureSlope))]
	
	# The corner between plateau and elastic regime is given by the solution of:
	# (x-touch_point)*slopeElastic = (x-minPosPlateau)*pressureSlopePlateau + minPosPlateauPressure
	# => (x-touch_point)=minPosPlateauPressure/(slopeElastic-pressureSlopePlateau)
	# =>
	
	elastic_limit_refined_guess = touch_point + minPosPlateauPressure/(slopeElastic-pressureSlopePlateau)
	
	if((elastic_limit_refined_guess>touch_point+(minPosPlateau-touch_point)*0.8) | (elastic_limit_refined_guess<touch_point+(minPosPlateau-touch_point)*0.1))
	{
		elastic_limit_refined_guess=min(touch_point+(minPosPlateau-touch_point)*0.8,(maxElasticPoint+touch_point+minPosPlateau)/2)
	}
	
	
 	
	return(elastic_limit_refined_guess)
	
}

