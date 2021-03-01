write_foam_mechanical_analysis_list_to_xlsx<-function(file_info,output_foam_mechanical_analysis_list,file_location)
{



wb <- createWorkbook()

theSheet<-createSheet(wb, sheetName="Extracted values from R")

headerRow<-createRow(theSheet, rowIndex=2)

theCells<-createCell(headerRow, colIndex=1:length(names(file_info)))

mapply(setCellValue, theCells, names(file_info))

firstRow<-createRow(theSheet, rowIndex=1)

theCells<-createCell(firstRow, colIndex=1)

mapply(setCellValue, theCells, "File listing variables")


index_file = 1

theFile = file_info[index_file,]

sample = output_foam_mechanical_analysis_list[[index_file]]

current_col_index=length(names(file_info))+1

theCells<-createCell(firstRow, colIndex=current_col_index)
mapply(setCellValue, theCells, "direction")

current_col_index = current_col_index + 1

column_index_data = current_col_index

for(theName in names(sample))
{
    
    
    
    theCells<-createCell(firstRow, colIndex=current_col_index)
    mapply(setCellValue, theCells, theName)
    
    
    if(!is.null(dim(sample[[theName]])))
    {
        theCells<-createCell(headerRow, colIndex=current_col_index:(dim(sample[[theName]])[2]-1+current_col_index))
        mapply(setCellValue, theCells, colnames(sample[[theName]]))
        
    } else {
        theCells<-createCell(headerRow, colIndex=current_col_index)
        mapply(setCellValue, theCells, theName)
    }
    
    
    
    
    if(!is.null(dim(sample[[theName]])))
    {
        current_col_index = current_col_index + dim(sample[[theName]])[2]
    } else {
        current_col_index = current_col_index+1
    }
    
    
    
}
current_row_index = 1

for(index_file in 1:(dim(file_info)[1]))
{
    
    analyzed_data = output_foam_mechanical_analysis_list[[index_file]]
    
    theFile = file_info[index_file,]
    
    
    
    
    
    
    current_row_index = current_row_index+2
    
    theRowUpper<-createRow(theSheet, rowIndex=current_row_index)
    
    theCells<-createCell(theRowUpper, colIndex=1:length(names(file_info)))
    
    mapply(setCellValue, theCells, theFile)
    
    
    
    theRowLower<-createRow(theSheet, rowIndex=current_row_index+1)
    
    theCells<-createCell(theRowLower, colIndex=1:length(names(file_info)))
    
    mapply(setCellValue, theCells, theFile)
    
    
    
    
    
    theCells<-createCell(theRowUpper, colIndex=column_index_data-1)
    mapply(setCellValue, theCells, rownames(analyzed_data[[1]])[1])
    
    theCells<-createCell(theRowLower, colIndex=column_index_data-1)
    mapply(setCellValue, theCells, rownames(analyzed_data[[1]])[2])
    
    current_col_index=column_index_data
    
    for(theName in names(analyzed_data))
    {
        
        
        
        
        if(!is.null(dim(analyzed_data[[theName]])))
        {
            
            theCells<-createCell(theRowUpper, colIndex=current_col_index:(dim(analyzed_data[[theName]])[2]-1+current_col_index))
            mapply(setCellValue, theCells, analyzed_data[[theName]][1,])
            
            theCells<-createCell(theRowLower, colIndex=current_col_index:(dim(analyzed_data[[theName]])[2]-1+current_col_index))
            mapply(setCellValue, theCells, analyzed_data[[theName]][2,])
            
        } else
        {
            theCells<-createCell(theRowUpper, colIndex=current_col_index)
            mapply(setCellValue, theCells, analyzed_data[[theName]][1])
            
            
            
            theCells<-createCell(theRowLower, colIndex=current_col_index)
            mapply(setCellValue, theCells, analyzed_data[[theName]][min(c(2,length(analyzed_data[[theName]])))])
            
        }
        
        if(!is.null(dim(analyzed_data[[theName]])))
        {
            current_col_index = current_col_index + dim(analyzed_data[[theName]])[2]
        }    else
        {
            current_col_index = current_col_index + 1
        }
        
        
        
    }
    
    
    
    
    
    
    
}


saveWorkbook(wb, file_location)


}