smooth_texture_analyzer_data <-
function(theData,sd=0.04,cycle_col="cycle",boundary_extension_mm=0.2,lm_region_lower_mm=boundary_extension_mm,lm_region_upper_mm=boundary_extension_mm,block_size=5)
{

    boundary_extension=0
    lm_region_lower=0
    lm_region_upper=0
    
    # If a boundary extension by linear interpolation for the smoothing is desired, we need to
    # convert the mm data in number of data points
    # We look how many points are needed to move the probe from the starting position beyond
    # the boundary_extension_mm variable
    if(boundary_extension_mm>0)
    {
        boundary_extension = min(which(abs(theData$Distance-theData$Distance[1])>boundary_extension_mm))
        lm_region_lower = lm_region_lower_mm*boundary_extension/boundary_extension_mm
        lm_region_upper = lm_region_upper_mm*boundary_extension/boundary_extension_mm
    }
    theData$ForceSlope = NA
    theData$pressureSlope = NA
    
    
    do_smoothing<-function(dataToSmooth,selector)
    {
        
        d=dataToSmooth$Distance[selector]
        
        isInverted=FALSE
        
        if(d[1]>d[length(d)]) {isInverted=TRUE}
        
        lmr_upper = lm_region_upper
        lmr_lower = lm_region_lower
        
        if(isInverted)
        {
            lmr_upper = lm_region_lower
            lmr_lower = lm_region_upper
        }
        
        dataToSmooth$Force[selector] =
            gaussianSmoothingBlock(dataToSmooth$Distance[selector],
                dataToSmooth$Force[selector],sd=sd,boundary_extension=boundary_extension,
                lm_region_upper=lmr_upper,lm_region_lower=lmr_lower,block_size=block_size)
        dataToSmooth$ForceSlope[selector] =
            gaussianSmoothingSlopeBlock(dataToSmooth$Distance[selector],
                dataToSmooth$Force[selector],sd=sd,boundary_extension=boundary_extension,
                lm_region_upper=lmr_upper,lm_region_lower=lmr_lower,block_size=block_size)
        
        dataToSmooth$pressure[selector] =
            gaussianSmoothingBlock(dataToSmooth$Distance[selector],
                dataToSmooth$pressure[selector],sd=sd,boundary_extension=boundary_extension,
                lm_region_upper=lmr_upper,lm_region_lower=lmr_lower,block_size=block_size)
        dataToSmooth$pressureSlope[selector] =
            gaussianSmoothingSlopeBlock(dataToSmooth$Distance[selector],
                dataToSmooth$pressure[selector],sd=sd,boundary_extension=boundary_extension,
                lm_region_upper=lmr_upper,lm_region_lower=lmr_lower,block_size=block_size)
        
        return(dataToSmooth)
        
    }
    
    
    for(theDirection in unique(theData$direction))
    {
        if(!(cycle_col %in% colnames(theData)))
        {
            
            selector = (theData$direction==theDirection)
            
            theData=do_smoothing(theData,selector)
            
        } else
        {
            
            for(theCycle in unique(theData[,cycle_col]))
            {
                selector = (theData$direction==theDirection) & (theData[,cycle_col]==theCycle)
                
                theData=do_smoothing(theData,selector)
                
            }
            
            
        }
        
    }
    
    return(theData)
    
    
}

