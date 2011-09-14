SELbank<-function(QB, N)
  {
    ###   make random selection of  question bank
    ch = 1:length(QB)
    ran1 = sample(ch, N )

    KBnew = vector(mode='list')

    for(i in 1:N)
      {
        KBnew[[i]] = QB[[ran1[i]]]
      }
    return(KBnew)
    
  }

