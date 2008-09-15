repair.id<-function(sisroster, scrfn)
{

####   sisroster = list(ID=number,  lastname='last name of student',  fullname='full name of student')
 ####   scrfn = list(ID=number, nam="name on scantron")

  
  ###########   fix ID = repair.id
  
#########   fix ID/namews that do not match the roster list
######### sisroster -> roster from BLACK Board
#########  scrfn    -> list of scantron

###  scrfn = list(ID, nam, 
###  sisroster  list( lastname  , fullname
  
  newid = scrfn$ID
  m1 = match(sisroster$ID, scrfn$ID)

  m2 = match(scrfn$ID, sisroster$ID )

  w1 = which(is.na( sisroster$ID[m2]))

  

  if(length(w1)<1) { print("no problems"); return() }
  print("list of mismatches:")
  print( cbind( scrfn$ID[w1] , scrfn$nam[w1]) )

  for(i in 1:length(w1))
    {
      Lname = tolower(unlist(strsplit(scrfn$nam[w1[i]], split=" ")))
      
      g1 = grep(Lname[1], sisroster$lastname)

      if(length(g1)<1)
        {
          lameo = Lname[1]
          name15 = substr(lameo, 1,5)
          namend = substr(lameo, nchar(lameo)-5, nchar(lameo))
          
          g1 = grep(name15, sisroster$lastname)
          g2 = grep(namend, sisroster$lastname)
          g1 = c(g1, g2)
          if(length(g1)<1)
            {
              name15 = substr(lameo, 1,1)
              g1 = grep(name15, sisroster$lastname)
            }
        }
      if(length(g1)==1)
	{
          cat("---FOUND MATCH - FIXING---------", sep="\n")
          print(data.frame(cbind(scrfn$nam[w1[i]], scrfn$ID[w1[i]], sisroster$lastname[g1], sisroster$fullname[g1]	, sisroster$ID[g1])))
          newid[w1[i]]=sisroster$ID[g1]
        }
      else
        {
          cat("---------------------------", sep="\n")

          cat("working on this dude:", sep="\n")

          cat( cbind( scrfn$ID[w1[i]] , scrfn$nam[w1[i]]) , sep="\n" )
          cat("--------LIST--------", sep="\n")
          cat("possible matches:", sep="\n")

          pname = paste(sep=":", 1:length(g1), sisroster$fullname[g1])

          cat(pname, sep="\n")

          cho = readline("possible matches: ")

          if(cho=="") { next }
          else
            {
              icho = as.numeric(cho)
              
              newid[w1[i]]=sisroster$ID[g1[icho]]
            }

        }
    }

  return(newid)

}
