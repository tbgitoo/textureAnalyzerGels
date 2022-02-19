read_texture_analyzer_tab <-
function(file,chuck_diameter=4e-3,aggregate_by="Distance",downup=TRUE,dec=".",skip_lines=1,unit_line=1,sep="", Force_column="Force",Distance_column="Distance",use_fill_for_read_table=FALSE,split_down_up_using_distance=FALSE)
{
    
    
    file_parts = strsplit(file,c("\\\\","/"))[[1]]
    
    file = do.call(file.path,as.list(file_parts))
    
    
    ret_data=read.table(file=file,header=FALSE,dec=dec,sep=sep,fill=use_fill_for_read_table)
    cols = c(as.matrix(ret_data[1+skip_lines,]))
    if(unit_line)
    {
    units = c(as.matrix(ret_data[2+skip_lines,]))
    }
    ret_data=ret_data[(2+skip_lines+unit_line):(dim(ret_data)[1]),]
    
    cols=sub(Force_column,"Force",cols,fixed=TRUE)
    cols=sub(Distance_column,"Distance",cols,fixed=TRUE)
    
    colnames(ret_data)=cols
    
    for(colind in 1:(dim(ret_data)[2]))
    {
        if(dec==".")
        {
            ret_data[,colind]=as.numeric(as.vector(ret_data[,colind]))
        } else
        {
            ret_data[,colind]=as.numeric(sub(dec,".",as.vector(ret_data[,colind])))
        }
    }
    ret_data$direction = "down"
    if(downup)
    {
      if(split_down_up_using_distance)
      {
        split_index<-nrow(ret_data)
          for(i in 1:nrow(ret_data))
        {
          if(i<nrow(ret_data) & ret_data$Distance[i]<ret_data$Distance[i+1])
          {
            split_index<-i
            break
          }
        }
        ret_data$direction[split_index:nrow(ret_data)]="up"
      }
      else
      {
        ret_data$direction[ceiling(dim(ret_data)[1]/2+1):(dim(ret_data)[1])]="up"
      }
    }
    ret_data$pressure = ret_data$Force*1e-3*9.81/(chuck_diameter/2)^2/pi
    if(aggregate_by=="Time")
    {
        ret_data$Time = round(ret_data$Time)
    }
    if(aggregate_by != FALSE)
    {
        
        
        data_aggregated_distance = aggregate(ret_data[,!(colnames(ret_data) %in% c(aggregate_by,"direction"))],by=ret_data[,c(aggregate_by,"direction")],FUN=mean)
        
        grouping_order= aggregate(data.frame(ascending=data_aggregated_distance$Time),by=list("group"=data_aggregated_distance$direction),FUN=function(x){x[length(x)]>x[1]})
        
        
        
        for(theDirection in unique(data_aggregated_distance$direction))
        {
                reverse = !(grouping_order$ascending[match(theDirection,grouping_order$group)])
                
                
                
                if(reverse)
                {
                    to_reverse=data_aggregated_distance[data_aggregated_distance$direction==theDirection ,]
                    
                    data_aggregated_distance[data_aggregated_distance$direction==theDirection ,]<-to_reverse[seq(from=dim(to_reverse)[1],to=1,by=-1),]
                    
                }
            
        }
        
        
    }
    else
    {
        return(ret_data)
    }
    return(data_aggregated_distance)
    
}
