gaussianSmoothingSlope<-function(x,y,sd=1,xout=x,...)
{
    
    
    
	if(length(xout)>1)
	{
		deltas = abs(diff(xout))
	}
	else
	{
		if(length(x)>1)
		{
				deltas = abs(diff(x))
		}
		else
		{
			return(0)
		}
	}
	delta_xout = min(deltas[deltas>0])/100
	
	yout = gaussianSmoothing(x=x,y=y,sd=sd,xout=xout,...)
	yout_delta = gaussianSmoothing(x=x,y=y,sd=sd,xout=xout+delta_xout,...)
	
	return((yout_delta-yout)/delta_xout)
	
}