GetStudentNames<-function(c1, dup.lets=1)
    {
####   c1 is a vector of names or a list with
###  an element "Name" that includes that vector

        if(is.list(c1) )
            {
                c1.names = names(c1)
                wn = which( tolower(c1.names) == 'name' )

                c1 = c1[[wn]]
            }
        g = strsplit(c1, ",")
        g.last = sapply(g, '[[', 1)
        g.first = sapply(g, '[[', 2)
        g.F = deblank(g.first)
        u.name = unique(g.F)
        
        if(length(u.name) != length(g.F) )
            {
###  might need to add last name initial to first name

                w.dup = which(  duplicated(g.F) )
                
                m =  match(g.F,  g.F[w.dup] )
                
               w1 = which(!is.na(m) )
               LastInit = substr( g.last[w1], 1,dup.lets)
               g.F[w1] = paste(g.F[w1], LastInit, sep='.')
                }
                return(g.F)

    }

