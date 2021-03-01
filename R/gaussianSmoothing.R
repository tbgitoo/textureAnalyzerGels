gaussianSmoothing<-function(x,y,sd=1,xout=NULL, boundary_extension=0,lm_region_lower=boundary_extension,lm_region_upper=boundary_extension)
{
	
# First of all, remove NA =====================
	
	notna=!is.na(x) & !is.na(y)
	
	x=x[notna]
	y=y[notna]
    
    if(boundary_extension<0) { boundary_extension=0}
    
    if(is.null(xout)) { xout = x}
    
    # Extend the variables by linear extrapolation to avoid edge effects (optional)
    
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
        xout=extend_z(xout,boundary_extension)
        y=extend_z(y,boundary_extension)
        
        
        
        
        
    }
	
# Now, prepare the data in the form of matrices =======================
	
# Repeat the x values as columns
	x_matrix = matrix(data=rep(x,length(xout)),ncol=length(xout),byrow=FALSE)
	
# Repeat the xout values as row
	x_out_matrix = matrix(rep(xout,length(x)),ncol=length(xout),byrow=TRUE)



	
# Get the difference and normalize to the standard deviation
	normalized_x = (x_matrix-x_out_matrix)/sd
	
	
	
# Gaussian Kernel, e.g. coefficients following the gaussian distribution taking into account the distance from x to xout; also, normalize such that every row has sum 1
	
	coeffs = t(apply(X=dnorm(normalized_x),MARGIN=2,FUN=function(z){return(z/sum(z))}))
	
	
	
# We need y as a single column matrix (column vector, but with class matrix)
	y_matrix = matrix(data=y,ncol=1,nrow=length(y))
    
    

# The filtering itself boils down to a matrix multiplication================

    ret=as.vector(coeffs %*%y_matrix)
    
    
	
	return(ret[(boundary_extension+1):(length(ret)-boundary_extension)])
    
    
}