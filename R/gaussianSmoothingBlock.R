gaussianSmoothingBlock<-function(x,y,sd=1,xout=NULL, boundary_extension=0,lm_region_lower=boundary_extension,lm_region_upper=boundary_extension,block_size=3)
{
	
    # First of all, if no block size is provided, call the underlying gaussianSmoothin function directly

if(block_size==0 | is.null(block_size)) {
    return (gaussianSmoothing(x=x,y=y,sd=sd,xout=xout, boundary_extension=boundary_extension,lm_region_lower=lm_region_lower,lm_region_upper=lm_region_upper))
}

# OK, so a block size is defined

# First of all, remove NA =====================

	
	notna=!is.na(x) & !is.na(y)
	
	x=x[notna]
	y=y[notna]
    
    if(boundary_extension<0) { boundary_extension=0}
    
    if(is.null(xout)) { xout = x}
    
    # Extend the variables by linear extrapolation to avoid edge effects (optional, depends on the value of boundary_extension)
    
    if(boundary_extension>0)
    {
        
        extend_z<-function(z,boundary_extension)
        {
            n<-length(z)
            b_index_lower<- 1:lm_region_lower
            b_index_extension_lower <- (-(1:boundary_extension)+1)[boundary_extension:1]
            zlm_lower<-z[b_index_lower]
            
            
            b_index_upper <- (n-lm_region_upper+1):n
            b_index_extension_upper <- (n+1):(n+boundary_extension)
            zlm_upper<-z[b_index_upper]
            
            
            return (c(
                predict(lm(zlm_lower~b_index_lower),newdata=data.frame(b_index_lower=b_index_extension_lower)),
                z,
                predict(lm(zlm_upper~b_index_upper),newdata=data.frame(b_index_upper=b_index_extension_upper))
            ))
            
        }
        
        
        x=extend_z(x,boundary_extension)
        y=extend_z(y,boundary_extension)
        
        
        
        
        
    }
    
    
    
    
# Now, since the boundary extension is done, only use +/- block_size

yout=xout

yout[]=NA

xnorm = x/sd

xoutnorm = xout/sd

for(ind in 1:length(xout))
{
    
    sel = (xnorm-xoutnorm[ind])>=-block_size & (xnorm-xoutnorm[ind])<=block_size
    
    yout[ind] = gaussianSmoothing(xnorm[sel],y[sel],sd=1,xout=xoutnorm[ind], boundary_extension=0,lm_region_lower=0,lm_region_upper=0)
}

	

    
    
	
	return(yout)
    
    
}