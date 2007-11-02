`ran.exam` <-
function(Qbank)
  {
    ran1 = sample(1:length(Qbank) )
    Q1 = list()
    for(i in 1:length(ran1))
      {
        Q1[[i]] = Qbank[[ran1[i]]]
        
      }
    return(Q1)
  }

