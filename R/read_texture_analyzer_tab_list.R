read_texture_analyzer_tab_list <-
function(file_info,root_folder,folder_column="Folder",file_column="File",
         chuck_diameter_column="diameter_mm",do_plot=TRUE,do_smoothing=TRUE,lines_to_read=NULL,
         aggregate_by="Distance",downup=TRUE,dec=".",skip_lines=1,unit_line=1,sep="",
         Force_column="Force",Distance_column="Distance",use_fill_for_read_table=FALSE,...)
{
    
    all_data = vector(mode="list",length=dim(file_info)[1])
    
    root_folder_parts = strsplit(root_folder,c("\\\\","/"))[[1]]

    root_folder = do.call(file.path,as.list(root_folder_parts))
    
    # If no particular lines are indicated, read all
    if(is.null(lines_to_read))
    {
        lines_to_read=1:(dim(file_info)[1])
    }
    
    # This is handy for debugging
     index_file = lines_to_read[1]
    
    for(index_file in lines_to_read )
    {
        
        theFile = file_info[index_file,]
        
        theFolder = theFile[,folder_column]
        
        
        
        folder_parts = strsplit(as.character(theFolder),c("\\\\","/"))[[1]]
        
        theFolder = do.call(file.path,as.list(folder_parts))
        
        
    theData=read_texture_analyzer_tab(file.path(root_folder,theFolder,theFile[,file_column]),chuck_diameter=theFile[,chuck_diameter_column]/1e3,aggregate_by=aggregate_by,downup=downup,dec=dec,skip_lines=skip_lines,unit_line=unit_line,sep=sep,
        Force_column=Force_column,Distance_column=Distance_column,use_fill_for_read_table=use_fill_for_read_table)
        
        
        if(do_plot)
        {
            dev.new()
            plot(theData$Distance, theData$pressure, xlab="Distance [mm]", ylab="Stress [Pa]", type="l",
            main=file.path(theFolder,theFile[,file_column]))
        }
        
        
        
        if(do_smoothing)
        {
            theData=smooth_texture_analyzer_data(theData,...)
            if(do_plot)
            {
                    lines(theData$Distance,theData$pressure,type="l",col="red")
            }
        }
        
        
        
        
        all_data[[index_file]]=theData
        
        
        cat(paste(index_file, " of ",length(lines_to_read),sep=""))
        cat("\n")
        
        
    }
    
    
    
    return(all_data)
    

	
	
}

