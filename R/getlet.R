`getlet` <-
function(ggrades, divs)
{
M = length(divs)
  N = M
  AP =  ggrades>=divs[N]-(divs[N]-divs[N-1])/3
  A =   ggrades>= (divs[N]-2*(divs[N]-divs[N-1])/3)& ggrades<divs[N]-(divs[N]-divs[N-1])/3
  AM =   ggrades>= (divs[N-1])& ggrades<divs[N]-2*(divs[N]-divs[N-1])/3

  N = M-1
  BP =  ggrades>=divs[N]-(divs[N]-divs[N-1])/3 & ggrades<divs[N]
  B =   ggrades>= (divs[N]-2*(divs[N]-divs[N-1])/3) & ggrades<divs[N]-(divs[N]-divs[N-1])/3
  BM =   ggrades>= (divs[N-1])& ggrades<divs[N]-2*(divs[N]-divs[N-1])/3

  N = M-2
  CP =  ggrades>=divs[N]-(divs[N]-divs[N-1])/3 & ggrades<divs[N]
  C =   ggrades>= (divs[N]-2*(divs[N]-divs[N-1])/3)& ggrades<divs[N]-(divs[N]-divs[N-1])/3
  CM =   ggrades>= (divs[N-1])& ggrades<divs[N]-2*(divs[N]-divs[N-1])/3

  N = M-3
  DP =  ggrades>=divs[N]-(divs[N]-divs[N-1])/3 & ggrades<divs[N]
  D =   ggrades>= (divs[N]-2*(divs[N]-divs[N-1])/3)& ggrades<divs[N]-(divs[N]-divs[N-1])/3
  DM =   ggrades>= (divs[N-1])& ggrades<divs[N]-2*(divs[N]-divs[N-1])/3

  N = M-4
  E = ggrades>=divs[N-1]&ggrades<divs[N]

  letts = rep("E", length(ggrades))
  scores  = rep(0, length(ggrades))

  SCRS = seq(from=100, by=(-4), length=13)
  LETS = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "E")

  scores[AP] = SCRS[1]
  scores[A] = SCRS[2]
  scores[AM] = SCRS[3]

  scores[BP] = SCRS[4]
  scores[B] = SCRS[5]
  scores[BM] = SCRS[6]

  scores[CP] = SCRS[7]
  scores[C] = SCRS[8]
  scores[CM] = SCRS[9]

  scores[DP] = SCRS[10]
  scores[D] = SCRS[11]
  scores[DM] = SCRS[12]

 scores[E] = SCRS[13]-SCRS[13]*(divs[2]-ggrades[E])/divs[2]
  
  letts[AP] = LETS[1]
  letts[A] = LETS[2]
  letts[AM] = LETS[3]

  letts[BP] = LETS[4]
  letts[B] = LETS[5]
  letts[BM] = LETS[6]

  letts[CP] = LETS[7]
  letts[C] = LETS[8]
  letts[CM] = LETS[9]

  letts[DP] = LETS[10]
  letts[D] = LETS[11]
  letts[DM] = LETS[12]

  letts[E] = LETS[13]

 
 return(list(grades=ggrades, lett=letts, scor=scores, divs=divs, LETS=LETS, SCRS=SCRS))
}

