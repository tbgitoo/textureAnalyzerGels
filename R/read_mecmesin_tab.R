read_mecmesin_tab <-
function (file, sample_diameter = 0.022, downup = TRUE) 
{
    # In the Mecmesin, the sample is smaller than the chuck, and since for pressure calculation the smaller of the two areas counts, we feed this into the textureAnylzer routines 
	# as teh chuck diameter
    chuck_diameter = sample_diameter

    dec="," # Mecmesin is a very french company and likes commaes
    
    skip_lines=0 # In Mecmesin files there is just the title and then the data, no lines to skip
    unit_lines=0 # No specific unit lines, Mecmesin files have the units in brackets behind the column title

    sep="\t" # Standard separator in the Volumina VectorPRO export files

   Force_column = "Load (gf)" # That's the title in the Mecmesin files for the force column

   Distance_column = "Displacement (mm)" # That's the title in the Mecmesin files for the distance column



    return (read_texture_analyzer_tab(file=file,chuck_diameter=chuck_diameter,aggregate_by=FALSE, downup=downup, dec=dec,skip_lines=skip_lines,unit_line=unit_lines,
sep=sep, Force_column=Force_column,Distance_column=Distance_column))
	
}
