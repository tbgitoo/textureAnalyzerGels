foam_mechanical_analysis <-
function(theData,start_height,approximate_gel_touch_point=NULL,landmarks=NULL,gel_thickness=NULL,stress_column="pressure",stress_slope_column="pressureSlope",do_plot=FALSE)
{
    
    landmark_is_imposed = rep(FALSE,3)
    names(landmark_is_imposed) = c("touch_point", "elastic_limit", "densification_limit")
    
    
    if(is.null(landmarks))
    {
        landmarks=find_landmarks_gel_compression(theData,approximate_gel_touch_point=approximate_gel_touch_point)

    } else {
        # Merge the user landmarks with landmarks to find otherwise
        if("touch_point" %in% names(landmarks))
        {
            touch_point = landmarks["touch_point"]
            landmark_is_imposed["touch_point"]=TRUE
        } else
        {
            touch_point = find_initial_touch_point(theData, approximate_touch_point = approximate_gel_touch_point)
        }
        if("elastic_limit" %in% names(landmarks))
        {
            elastic_limit = landmarks["elastic_limit"]
            landmark_is_imposed["elastic_limit"]=TRUE
        } else
        {
            elastic_limit = find_elastic_limit(theData, touch_point = touch_point)
        }
        if("densification_limit" %in% names(landmarks))
        {
            densification_limit = landmarks["densification_limit"]
            landmark_is_imposed["densification_limit"]=TRUE
        } else
        {
            densification_limit = find_densification_limit(theData, touch_point,
            elastic_limit, gel_thickness = start_height-touch_point)
        }
        landmarks = vector(mode = "numeric", length = 3)
        names(landmarks) = c("touch_point", "elastic_limit", "densification_limit")
        landmarks["touch_point"] = touch_point
        landmarks["elastic_limit"] = elastic_limit
        landmarks["densification_limit"] = densification_limit
        

        
        
    }
    
    
    
	
    
    ret = data.frame(direction=unique(theData$direction),slope_elastic=NA,Distance_elastic=NA,stress_elastic=NA,touchpoint_elastic=NA,
        elastic_plateau_transition_distance=NA,elastic_plateau_transition_slope=NA,
        slope_plateau=NA,Distance_plateau=NA,stress_plateau=NA)
        
    rownames(ret)=ret$direction
    
    for(theDirection in unique(theData$direction))
    {
    
    
        selector_elastic = theData$direction==theDirection & theData$Distance >= landmarks["touch_point"] & theData$Distance <= landmarks["elastic_limit"]
        
        # First the elastic segment; the characteristic point is the one with maximal slope
        slope_elastic=max(theData[selector_elastic,stress_slope_column])
        
        elastic_index = which.max(theData[selector_elastic,stress_slope_column])
        
        Distance_elastic =(theData$Distance[selector_elastic]) [elastic_index]
        
        stress_elastic = (theData[selector_elastic,stress_column]) [elastic_index]
       
        touchpoint = Distance_elastic-stress_elastic/slope_elastic
        
        
        
        ret[ret$direction==theDirection,c("slope_elastic","Distance_elastic","stress_elastic","touchpoint_elastic")]=c(slope_elastic,Distance_elastic,stress_elastic,touchpoint)
        
        
        # Second, the plateau segment. Here, on the contrary, we look for minimal slope
        
        selector_plateau = theData$direction==theDirection & theData$Distance >= Distance_elastic & theData$Distance <= landmarks["densification_limit"]
        
        mid_plateau_index = which.min(theData[selector_plateau,stress_slope_column])
        
        mid_plateau_Distance = (theData$Distance[selector_plateau])[mid_plateau_index]
        
        mid_plateau_pressure = (theData[selector_plateau,stress_column])[mid_plateau_index]
        
        mid_plateau_pressureSlope = (theData[selector_plateau,stress_slope_column])[mid_plateau_index]
        
        ret[ret$direction==theDirection,c("slope_plateau","Distance_plateau","stress_plateau")]=
            c(mid_plateau_pressureSlope,mid_plateau_Distance,mid_plateau_pressure)
    
    # Calculate the cross-over from elastic to plateau by the crossing slopes
    
    #stress_elastic+(x-Distance_elastic)*slope_elastic = stress_plateau+(x-Distance_plateau)*slope_plateau
    
    # stress_elastic-stress_plateau = (x-Distance_plateau)*slope_plateau - (x-Distance_elastic)*slope_elastic
    
    # stress_elastic-stress_plateau = x*(slope_plateau-slope_elastic)+Distance_elastic*slope_elastic-Distance_plateau*slope_plateau
    
    # x*(slope_plateau-slope_elastic)=stress_elastic-stress_plateau + Distance_plateau*slope_plateau-Distance_elastic*slope_elastic
    
    # x=(stress_elastic-stress_plateau + Distance_plateau*slope_plateau-Distance_elastic*slope_elastic)/(slope_plateau-slope_elastic)
    
    
    
        elastic_plateau_transition_distance=with(ret[ret$direction==theDirection,],(stress_elastic-stress_plateau + Distance_plateau*slope_plateau-Distance_elastic*slope_elastic)/(slope_plateau-slope_elastic))
        
        plateau_found = TRUE
        
        if(is.nan(elastic_plateau_transition_distance)) {
                elastic_plateau_transition_distance = Distance_elastic
                plateau_found = FALSE}
        
        if(elastic_plateau_transition_distance < Distance_elastic) {
            elastic_plateau_transition_distance = Distance_elastic
            plateau_found = FALSE}
        if(elastic_plateau_transition_distance > mid_plateau_Distance) {
            # A problem with the maximum, try again with update mid_plateau distance for the max
            
            elastic_new = theData[theData$direction==theDirection & theData$Distance>=touchpoint &
            theData$Distance<=mid_plateau_Distance,]
            
            ind_max=which.max(elastic_new$pressureSlope)
            
            stress_elastic_new = elastic_new$pressure[ind_max]
            Distance_elastic_new = elastic_new$Distance[ind_max]
            slope_elastic_new = elastic_new$pressureSlope[ind_max]
            
            found_better_values=FALSE
            
            if(slope_elastic_new > ret[ret$direction==theDirection,"slope_plateau"] & Distance_elastic_new>touchpoint)
            {
            
            elastic_plateau_transition_distance_new=with(ret[ret$direction==theDirection,],(stress_elastic_new-stress_plateau + Distance_plateau*slope_plateau-Distance_elastic_new*slope_elastic_new)/(slope_plateau-slope_elastic_new))
            
            if(elastic_plateau_transition_distance_new<mid_plateau_Distance)
            {
               found_better_values=TRUE
               
               stress_elastic=stress_elastic_new
               Distance_elastic=Distance_elastic_new
               slope_elastic=slope_elastic_new
               elastic_plateau_transition_distance=elastic_plateau_transition_distance_new
               
               ret[ret$direction==theDirection,"slope_elastic"]=slope_elastic
               ret[ret$direction==theDirection,"Distance_elastic"]=Distance_elastic
               ret[ret$direction==theDirection,"stress_elastic"]=stress_elastic
               ret[ret$direction==theDirection,"elastic_plateau_transition_distance"]=elastic_plateau_transition_distance
               
               selector_plateau = theData$direction==theDirection & theData$Distance >= Distance_elastic & theData$Distance <= landmarks["densification_limit"]
               
               
            }
            
            }
            
            if(!found_better_values)
            {
            elastic_plateau_transition_distance = mid_plateau_Distance
            plateau_found = FALSE
            }
            }
        
        
        
        
        elastic_plateau_transition_index = which.min(abs(theData$Distance[theData$direction==theDirection]-elastic_plateau_transition_distance))
        
        elastic_plateau_transition_distance=theData[theData$direction==theDirection,"Distance"][elastic_plateau_transition_index]
        
        ret$elastic_plateau_transition_distance[ret$direction==theDirection] = elastic_plateau_transition_distance
        
        
        ret$elastic_plateau_transition_slope[ret$direction==theDirection] = theData[theData$direction==theDirection,stress_slope_column][elastic_plateau_transition_index]
        
        ret$elastic_plateau_transition_stress[ret$direction==theDirection] = theData[theData$direction==theDirection,stress_column][elastic_plateau_transition_index]
        
        # End of the plateau: when we reach again the slope at the beginning of the plateau
        
        slope_to_reach=ret$elastic_plateau_transition_slope[ret$direction==theDirection]
        
        selector_end_plateau = theData$direction==theDirection & theData$Distance > mid_plateau_Distance
        
        distance_end_plateau=min((theData$Distance[selector_end_plateau])[theData[selector_end_plateau,stress_slope_column]>slope_to_reach])
        
        ret$distance_transition_to_densification[ret$direction==theDirection]=distance_end_plateau
        
        index_end_plateau = which(theData$Distance[theData$direction==theDirection]==distance_end_plateau)
       
        ret$slope_transition_to_densification[ret$direction==theDirection]=theData[theData$direction==theDirection,stress_slope_column][index_end_plateau]
        
        ret$pressure_transition_to_densification[ret$direction==theDirection]=theData[theData$direction==theDirection,stress_column][index_end_plateau]
        
        ret$plateau_detected =plateau_found
        
    }
    
    touchpoint = mean(ret$touchpoint_elastic)
    
    if(is.null(gel_thickness))
    {
        
        gel_thickness = start_height-touchpoint
        
    }
    
    ret$touchpoint = touchpoint
    
    # Do another round at the common touchpoint
    
    for(theDirection in unique(theData$direction))
    {
    
        touchpoint_index = which.min(abs(theData$Distance[theData$direction==theDirection]-ret[theDirection,]$touchpoint))
        
        ret$stress_touchpoint[ret$direction==theDirection]=theData[theData$direction==theDirection,stress_column][touchpoint_index]
        
        ret$strain_touchpoint = 0
        
        
        
    }
    
    
    
    
    ret$E_elastic = ret$slope_elastic*gel_thickness
    
    ret$strain_elastic = (ret$Distance_elastic-touchpoint)/gel_thickness
    
    ret$strain_transition_elastic_plateau = (ret$elastic_plateau_transition_distance-touchpoint)/gel_thickness
    
    ret$strain_plateau = (ret$Distance_plateau-touchpoint)/gel_thickness
    
    ret$strain_P_D =(ret$distance_transition_to_densificatio-touchpoint)/gel_thickness
    
    ret$E_differential_elastic_plateau_transition = ret$elastic_plateau_transition_slope*gel_thickness
    
    ret$E_differential_touchpoint = ret$stress_touchpoint*gel_thickness
    
    ret$E_differential_P_D=ret$slope_transition_to_densification*gel_thickness
    
    ret$E_abs_elastic_plateau_transition = ret$elastic_plateau_transition_stress/ret$strain_transition_elastic_plateau
    
    
    ret$E_differential_plateau = ret$slope_plateau*gel_thickness
    
    ret$E_abs_plateau = ret$stress_plateau/ret$strain_plateau
    
	
    template_for_return = data.frame(touchpoint=rep(NA,length=dim(ret)[1]),
        elastic=NA,transition_E_P=NA,plateau=NA,transition_P_D=NA)
    
    rownames(template_for_return)=ret$direction
    
    E_differential=template_for_return
    
    E_differential[,c("touchpoint","elastic","transition_E_P","plateau","transition_P_D")] =
        ret[,c("E_differential_touchpoint","E_elastic","E_differential_elastic_plateau_transition","E_differential_plateau","E_differential_P_D")]
    
    stress = template_for_return

    stress[,c("touchpoint","elastic","transition_E_P","plateau","transition_P_D")]=ret[,c("stress_touchpoint","stress_elastic","elastic_plateau_transition_stress","stress_plateau","pressure_transition_to_densification")]
    
    Distance = template_for_return
    
    Distance[,c("touchpoint","elastic","transition_E_P","plateau","transition_P_D")]=ret[,c("touchpoint","Distance_elastic","elastic_plateau_transition_distance","Distance_plateau","distance_transition_to_densification")]
    
    strain = template_for_return
    
    strain[,c("touchpoint","elastic","transition_E_P","plateau","transition_P_D")]=ret[,c("strain_touchpoint","strain_elastic","strain_transition_elastic_plateau","strain_plateau","strain_P_D")]
    
    
    
    
    
    E_absolute = stress/strain
    
    E_absolute[strain==0]=NA
    
    
    plateau_detected = ret[,"plateau_detected"]
    names(plateau_detected) = ret$direction
    
    
    # For evaluation of the plateau width
    
    plateau_width = vector(mode="numeric",length=length(plateau_detected))
    names(plateau_width)=names(plateau_detected)
    
    for(theDirection in names(plateau_width))
    {
        if(plateau_detected[theDirection])
        {
            plateau_width[theDirection]=strain[theDirection,"transition_P_D"]-strain[theDirection,"transition_E_P"]
        } else {
            plateau_width[theDirection]=0
        }
    }
    ret=list(Distance=Distance,strain=strain,stress=stress,E=E_differential,E_from_touchpoint=E_absolute, plateau_detected = plateau_detected,plateau_width=plateau_width,gel_height=gel_thickness)
    
    
    if(do_plot) {
        
        plot(theData[theData$direction=="down","Distance"],theData[theData$direction=="down",stress_column],type="l")
        lines(theData[theData$direction=="up","Distance"],theData[theData$direction=="up",stress_column],type="l")
        lines(ret[["Distance"]]["down",],ret[["stress"]]["down",],type="p",col="red")
        lines(ret[["Distance"]]["up",],ret[["stress"]]["up",],type="p",col="green")
    }
    
    
    
    return(ret)
    
    
	
}

