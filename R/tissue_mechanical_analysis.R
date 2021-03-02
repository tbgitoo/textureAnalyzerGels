tissue_mechanical_analysis <-
function(theData,start_height,approximate_touch_point=NULL,stress_column="pressure",stress_slope_column="pressureSlope")
{
	
    
    # First thing, find touchpoint
    
    directions = unique(theData$direction)
    
    template_vector = vector(mode="numeric",length=length(directions))
    
    names(template_vector)=directions
    
    touchpoint=template_vector
    
    for(theDirection in directions )
    {
    
    
        data_for_touchpoint = theData[theData$direction==theDirection,]
        
        data_for_touchpoint$pressure = data_for_touchpoint[,stress_column]
        
        data_for_touchpoint$pressureSlope = data_for_touchpoint[,stress_slope_column]
        
        touchpoint[theDirection] = find_initial_touch_point(data_for_touchpoint,approximate_touch_point=approximate_touch_point)
        
        
        
        
        
	
    }
    
    
    sample_thickness=start_height-touchpoint[["down"]]
    
    theData$strain = (theData$Distance - touchpoint[["down"]])/sample_thickness
        
    E = matrix(nrow=length(directions),ncol=9)
    rownames(E)=directions
        
        
    colnames(E)=(1:9)/10
	
    for(E_ind in 1:9)
    {
        for(theDirection in directions )
        {

            selData = theData[theData$direction==theDirection & theData$strain>0.1*E_ind-0.05 & theData$strain<0.1*E_ind+0.05, ]
            
            if(dim(selData)[1]>1)
            {
                thePressure = selData[,stress_column]
                theStrain = selData$strain
                E[theDirection,as.character((E_ind)/10)] = coefficients(lm(thePressure ~ theStrain))[["theStrain"]]
                
            }

        }
        
    }
    
    # Hysteresis
    
    
    
   
        
        pressureDown <- theData[theData$direction=="down" & theData$strain>0, stress_column ]
        strainDown <- theData[theData$direction=="down" & theData$strain>0, "strain" ]
        
        pressureUp <-theData[theData$direction=="up" & theData$strain>0, stress_column ]
        strainUp <- theData[theData$direction=="up" & theData$strain>0, "strain" ]
        
        pressureUpmatched <<- pressureUp[match(strainDown,strainUp)]
        
        
        deltas=diff(strainDown)
        
        deltas=c(deltas,deltas[length(deltas)])
        
        hysteresis = sum((pressureDown-pressureUpmatched)*deltas)/sum(pressureDown*deltas)
    
    
    return(list(touchpoint=touchpoint, sample_thickness=sample_thickness,E=E,hysteresis=hysteresis))
	
}

