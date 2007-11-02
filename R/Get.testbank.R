`Get.testbank` <-
function(fn)
  {

    ALLQ = scan(file=fn, what="", sep="\n")

    q1 = grep("QUESTION:", ALLQ)


    Qbank = list()

    for(i in 1:length(q1))
      {
        i1 = q1[i]
	if(i<length(q1))
          { i2 = q1[i+1]-1 }	else { i2 = length(ALLQ)	}

        
        ## print(paste(sep=" ", "#####", i, i1, i2))
        ## print(ALLQ[i1:i2])
        
        quest = substring(ALLQ[i1], 11, nchar(ALLQ[i1]))


        ans1 = ALLQ[(i1+1):i2]
        grfig = grep("FIG:", ans1)
        if(length(grfig)>=1)
          {
            fig1 = ans1[grfig]
            ufig = unlist(strsplit(fig1, split=" "))
            
            figname = ufig[2]
            figtag = ufig[3]
            fignewt = list(fn=figname, tag =figtag) 
            ans1 = ans1[-grfig]
            
          }
        else
          {
            fignewt = NULL
          }
        
        a1 = grep("ANSWER:", ans1)
        fa1 = ans1[a1]
        a2 = substring(fa1, 9, nchar(fa1))
        ans1[a1] = a2

        Qbank[[i]] = list(Q=quest, A=ans1, a=fa1, numANS=a1, FIG=fignewt)

      }

    return(Qbank)



  }

