getgroup<-function(g.first, n=2)
{
    ## set.seed(as.integer(Sys.Date()))

    N = length(g.first)
    
    s =  sample(g.first )

###  break up the s into groups of n each
    k = ceiling(N/n)
    
    k2 = rep(1:n, times=k)

    k2 = k2[1:N]

    cbind(s, k2)

  groops =   split(s, factor(k2) )

    cols = length(groops)

    DEX = 1/(cols)
    ex.plan = seq(from=DEX/2, by=DEX, length=cols )

    max.n = max( unlist( sapply(groops, 'length') ) )
     DEY = 1/(max.n)
    why.plan = seq(from=DEY/2, by=DEY, length=max.n)
    
   ##   par(mai=c(.1,.1,.1,.1) )
    plot(range(ex.plan) , range(why.plan) ,type='n',  ann=FALSE, axes=FALSE)
    abline(v=ex.plan+DEX/2)
    
    
    for(i in 1:cols)
        {
            gr = groops[[i]]
            ex0 = ex.plan[i]
            ng = length(gr) 
            
            for(j in 1:length(why.plan) )
                {
                    text(ex0,  why.plan[j], labels=gr[j]  , xpd=TRUE)
                    
                }
        }
    
    mtext(1:cols, side = 3, line = 0, at = ex.plan )

    return(groops)
    
}
