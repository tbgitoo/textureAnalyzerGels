gaussianSmoothingSlopeBlock<-function(x,y,sd=1,xout=x,block_size=6,...)
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
    
    yout = gaussianSmoothingBlock(x=x,y=y,sd=sd,xout=xout,block_size=block_size,...)
    yout_delta = gaussianSmoothingBlock(x=x,y=y,sd=sd,xout=xout+delta_xout,block_size=block_size,...)
    
    return((yout_delta-yout)/delta_xout)
    
}