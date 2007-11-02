`jist` <-
function(h, Z, L, col=2)
  {
    if(missing(col)) { col = 1 }
    ###  h = histogram structure
    ###  Z = scores
    #### L = letter grades or labels

    J = rep(1, length(Z))

    Kounts = c(0, h$counts)
    
    for(i in 1:length(Kounts))
      {
        if(Kounts[i]==0) next
        x1 = h$breaks[i]
        x2 = h$breaks[i+1]
        w = which(Z>=x1&Z<x2)
        k = Kounts[i]

        v = seq(1, length(w))
        J[w] = v
        
      }
    i = length(Kounts)
    x2 = h$breaks[i]
    w = which(Z>=x2)
    if(length(w)>0)
      {
        v = seq(1, length(w))
        J[w] = v
      }
    
    text(Z, J, labels=L, col=col, xpd=TRUE, cex=.8, font=2)
    invisible(J)
  }

