CHECKbank<-function(QB)
{

  for(i in 1:length(QB))
    {
      Q1 = QB[[i]]
      if( nchar(Q1$Q)<2) { print(paste("PROBLEM: Question error", i, sep=" ") ) }
      if( length(Q1$A)<1) { print(paste("PROBLEM: Answer error", i, sep=" ") ) }
      if( length(Q1$numANS)<1) { print(paste("PROBLEM: NO Correct Answer error", i, sep=" ") ) }
      if( length(Q1$a)<1) { print(paste("PROBLEM: NO Correct Answer error", i, sep=" ") ) }

    }

}


