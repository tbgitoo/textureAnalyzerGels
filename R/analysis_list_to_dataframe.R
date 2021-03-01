analysis_list_to_dataframe<-function(file_info,output_foam_mechanical_analysis_list)
{






for(index_file in 1:(dim(file_info)[1]))
{
    
    analyzed_data = output_foam_mechanical_analysis_list[[index_file]]
    
    theFile = file_info[index_file,]
    
    
    
    datapoints=0
    
    for(theName in names(analyzed_data))
    {
        if(!is.null(dim(analyzed_data[[theName]])))
        {
            datapoints = datapoints+(dim(analyzed_data[[theName]])[1])*(dim(analyzed_data[[theName]])[2])
        } else {
            datapoints = datapoints+length(analyzed_data[[theName]])
        }
    }
    
    current_analyzed_data_frame = theFile
    
    for(ind in 2:datapoints)
    {
        current_analyzed_data_frame = rbind(current_analyzed_data_frame,theFile)
    }
    
    current_analyzed_data_frame$measurement = vector(mode="character",length=datapoints)
    current_analyzed_data_frame$direction = vector(mode="character",length=datapoints)
    current_analyzed_data_frame$point = vector(mode="character",length=datapoints)
    current_analyzed_data_frame$value = vector(mode="numeric",length=datapoints)
    
    current_analyzed_data_frame$file_info_id = index_file
    
    current_row_analyzed_data = 1
    
    for(theName in names(analyzed_data))
    {
        to_put=analyzed_data[[theName]]
        if(!is.null(dim(to_put)))
        {
            
            for(theDirection in rownames(to_put))
            {
                for(thePlace in colnames(to_put))
                {
                    current_analyzed_data_frame$measurement[current_row_analyzed_data]=theName
                    current_analyzed_data_frame$direction[current_row_analyzed_data]=theDirection
                    current_analyzed_data_frame$point[current_row_analyzed_data]=thePlace
                    current_analyzed_data_frame$value[current_row_analyzed_data]=to_put[theDirection,thePlace]
                    current_row_analyzed_data = current_row_analyzed_data+1
                }
            }
        } else {
            
            if(is.null(names(to_put)))
            {
                for(ind_to_put in 1:length(to_put))
                {
                    current_analyzed_data_frame$measurement[current_row_analyzed_data]=theName
                    current_analyzed_data_frame$direction[current_row_analyzed_data]=NA
                    current_analyzed_data_frame$point[current_row_analyzed_data]="overall"
                    current_analyzed_data_frame$value[current_row_analyzed_data]=to_put[ind_to_put]
                    current_row_analyzed_data = current_row_analyzed_data+1
                }
            
            } else {
            
            for(theDirection in names(to_put))
            {
                current_analyzed_data_frame$measurement[current_row_analyzed_data]=theName
                current_analyzed_data_frame$direction[current_row_analyzed_data]=theDirection
                current_analyzed_data_frame$point[current_row_analyzed_data]="overall"
                current_analyzed_data_frame$value[current_row_analyzed_data]=to_put[theDirection]
                current_row_analyzed_data = current_row_analyzed_data+1
            }
            }
            
        }
        
    }
    
    
    if(index_file==1)
    {
        all_analyzed_data = current_analyzed_data_frame
    } else
    {
        
        all_analyzed_data = rbind(all_analyzed_data,current_analyzed_data_frame)
    }
    
    
    
    
    
}

return(all_analyzed_data)


}




