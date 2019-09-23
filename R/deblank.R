deblank<-function(a)
    {
        b = a
        for(i in 1:length(a) )
            {
                h = unlist( strsplit(a[i], split='') )
                b[i] = paste(h[h!=' '], collapse='')
            }
        return( b)
    }

