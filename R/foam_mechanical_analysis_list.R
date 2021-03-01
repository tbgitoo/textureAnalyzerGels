foam_mechanical_analysis_list <- function(file_info,data_list,start_height_column="start_height_mm",approximate_gel_touch_point_column="approximate_gel_touch_point",...){


analyzed_data = vector(mode="list",length=length(data_list))

for(index_file in 1:(dim(file_info)[1]))
{
    
    theData = data_list[[index_file]]
    
    theFile = file_info[index_file,]


analyzed_data[[index_file]] = foam_mechanical_analysis(theData,start_height=theFile[,start_height_column],approximate_gel_touch_point=theFile[,approximate_gel_touch_point_column],...)


}

return(analyzed_data)

}