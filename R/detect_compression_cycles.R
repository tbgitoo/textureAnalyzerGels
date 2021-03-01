detect_compression_cycles <-
function(textureAnalyzerData,do_aggregation=TRUE)
{
	
    textureAnalyzerData$cycle = NA
    
    direction_char = c("up","down")
    
    diffs = sign(diff(textureAnalyzerData$Distance))
    
    n=length(diffs)
    
    # Get the sign of the first movement (1 is down, -1 is up)
    
    current_direction = diffs[which(diffs!=0)[1]]
    
    current_pos = 1
    current_cycle = 1
    
    # Find the next turning point
    
    turning_point=which(diffs[current_pos:n]!=0 & diffs[current_pos:n]!=current_direction)[1]
    
    while(!is.na(turning_point))
    {
    
        textureAnalyzerData$direction[current_pos:turning_point] = direction_char[(current_direction+3)/2]
        textureAnalyzerData$cycle[current_pos:turning_point] = current_cycle
        current_pos=turning_point
        current_direction=diffs[turning_point]
        current_cycle = current_cycle+1
        turning_point=current_pos+which(diffs[current_pos:n]!=0 & diffs[current_pos:n]!=current_direction)[1]-1
        
        # Last cycle
        if(is.na(turning_point))
        {
            textureAnalyzerData$direction[current_pos:(n+1)] = direction_char[(current_direction+3)/2]
            textureAnalyzerData$cycle[current_pos:(n+1)] = current_cycle
            
            
            
        }
    }
    
    
    
    # A cycle is once back and forth
    
    textureAnalyzerData$cycle=floor((textureAnalyzerData$cycle+1)/2)
    
    if(do_aggregation)
    {
        
        textureAnalyzerData_agg = aggregate(textureAnalyzerData,by=list("group"=paste(textureAnalyzerData$cycle,textureAnalyzerData$direction,textureAnalyzerData$Distance,sep="_")),FUN=function(x){if(is.numeric(x)) {return(mean(x))} else {return(x[1])}})
        
        # Aggregation will sometime invert the order, need to correct this
        
        grouping_order= aggregate(data.frame(ascending=textureAnalyzerData_agg$Time),by=list("group"=paste(textureAnalyzerData_agg$cycle,textureAnalyzerData_agg$direction,sep="_")),FUN=function(x){x[length(x)]>x[1]})
        
        for(theDirection in unique(textureAnalyzerData_agg$direction))
        {
            for(theCycle in unique(textureAnalyzerData_agg$cycle))
            {
                reverse = !(grouping_order$ascending[match(paste(theCycle,theDirection,sep="_"),grouping_order$group)])
                
                
                
                if(reverse)
                {
                 to_reverse=textureAnalyzerData_agg[textureAnalyzerData_agg$direction==theDirection & textureAnalyzerData_agg$cycle==theCycle,]
                 textureAnalyzerData_agg[textureAnalyzerData_agg$direction==theDirection & textureAnalyzerData_agg$cycle==theCycle,]<-to_reverse[seq(from=dim(to_reverse)[1],to=1,by=-1),]
                
                }
            }
        }
        
        
        
        textureAnalyzerData_to_return <- textureAnalyzerData_agg[,-1] # We don't need the grouping column
        
    } else { textureAnalyzerData_to_return<-textureAnalyzerData }
    
   
     
	return(textureAnalyzerData_to_return)
	
}

