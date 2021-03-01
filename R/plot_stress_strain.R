plot_stress_strain <-
function(compression_data, analysis,start_height_mm, plot_evaluation=TRUE,pressure_column="pressure",distance_column="Distance",direction_column="direction",
title="stress-strain")
{
	
    directions=unique(compression_data[,direction_column])
    
    compression_data$touchpoint = analysis$Distance[compression_data[,"direction"],"touchpoint"]
    
    compression_data$gel_height = start_height_mm-compression_data$touchpoint
    
    compression_data$strain = (compression_data$Distance-compression_data$touchpoint)/compression_data$gel_height
    
    started=FALSE
    
    for(theDirection in directions)
    {
        
        sel=compression_data[,direction_column]==theDirection
        
        if(!started)
        {
           
           
           plot(compression_data$strain[sel], compression_data[sel,pressure_column],
           xlab="strain",ylab="stress",type="l",main=title)
           
           started=TRUE
           
           old_direction=theDirection
            
            
        } else
        
        {
            old=compression_data[compression_data[,direction_column]==old_direction,]
            
            last_old=old[dim(old)[1],]
            
            first_new = (compression_data[sel,])[1,]
            
            lines(c(last_old$strain,first_new$strain),c(last_old[,pressure_column],first_new[,pressure_column]),type="l")
            
            lines(compression_data$strain[sel], compression_data[sel,pressure_column],type="l")
            
            
        }
        
        
    }
    
    if(plot_evaluation)
    {
        
        points_to_plot=c("touchpoint","elastic","plateau")
        for(theDirection in directions)
        {
            
            lines(analysis$strain[theDirection,points_to_plot],analysis$stress[theDirection,points_to_plot],type="p",col="red")
            
            E_elastic = analysis$E[theDirection,"elastic"]
            
            support_strain = c(analysis$strain[theDirection,"touchpoint"],
            analysis$strain[theDirection,"plateau"])
            
            stress_line_E_elastic = E_elastic*(support_strain-analysis$strain[theDirection,"elastic"])+analysis$stress[theDirection,"elastic"]
            
            lines(support_strain,stress_line_E_elastic,type="l",col="red")
            
            
            E_plateau = analysis$E[theDirection,"plateau"]
            
            support_strain = c(analysis$strain[theDirection,"elastic"],
            analysis$strain[theDirection,"transition_P_D"])
            
            stress_line_E_plateau = E_plateau*(support_strain-analysis$strain[theDirection,"plateau"])+analysis$stress[theDirection,"plateau"]
            
            lines(support_strain,stress_line_E_plateau,type="l",col="red")
            
        
        }
        
        legend("topleft",legend=paste("E ",directions, "=", round(analysis$E[directions,"elastic"]),"Pa (elastic) , ", round(analysis$E[directions,"plateau"]),"Pa (plateau) "))
        
    }
    
    
    
    
	
}

