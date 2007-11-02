DUMPbank<-function(ofile, QB, sep="\n")
{
  if(missing(sep)) sep=NULL
#########  dump out an ascii version of an exam question bank
  for(i in 1:length(QB))
    {
      Q1 = QB[[i]]
      cat(file=ofile,"QUESTION: ", append=TRUE, sep="")
      cat(file=ofile,Q1$Q, append=TRUE, sep="\n")
      for(j in 1:length(  Q1$A))
        {
          if(j==Q1$numANS) { cat(file=ofile,"ANSWER: ", append=TRUE, sep="") }
          cat(file=ofile,Q1$A[j], append=TRUE, sep="\n")


        }
      if(!is.null(sep)) cat(file=ofile,"", append=TRUE, sep=sep)

    }
#######  example: DUMPbank("dafinal", QBFINAL)

}

