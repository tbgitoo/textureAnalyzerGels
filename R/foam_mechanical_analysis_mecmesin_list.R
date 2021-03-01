foam_mechanical_analysis_mecmesin_list <- 
function(file_info,data_list,offset_height_column="offset_height_mm",approximate_gel_height_column="approximate_gel_height",...){


analyzed_data = vector(mode="list",length=length(data_list))

for(index_file in 1:(dim(file_info)[1]))
{
    
    theData = data_list[[index_file]]
    
    theFile = file_info[index_file,]

approximate_gel_height=NULL

if(!is.na(theFile[,approximate_gel_height_column]) & !is.null(theFile[,approximate_gel_height_column])){
   approximate_gel_height=theFile[,approximate_gel_height_column]
}


analyzed_data[[index_file]] = foam_mechanical_analysis_mecmesin(theData,offset_height=theFile[,offset_height_column], approximate_gel_height=approximate_gel_height,...)


}

return(analyzed_data)

}