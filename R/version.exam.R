`version.exam` <-
function(Qbank, V, exnumber="Exam 1", seqnum="2", examdate='', instructor="", course="", instructions="" )
{
  if(missing(exnumber)) {  exnumber="Exam 1" }
  if(missing(seqnum)) { seqnum="1" }
  if(missing(examdate)) {   examdate=''}
    if(missing(instructor)) instructor=""
   
    if(missing(course)) course=""
    
    if(missing(instructions)) instructions=""

  
  QTEMP = ran.exam(Qbank)
  outtex = paste(sep=".",V, "tex" )
  outMAST  = paste(sep="", V, "MAST" )


  examname=paste(sep=" ", exnumber, "Seq", seqnum)



  
  MASTtex  = paste(sep=".", outMAST , "tex" )


    outsolut  = paste(sep="", V, "solutions.tex" )

  MASTdvi  = paste(sep=".", outMAST , "dvi" )
  MASTps  = paste(sep=".", outMAST , "ps" )
  MASTpdf  = paste(sep=".", outMAST , "pdf" )
  make.exam(QTEMP, ofile=outtex )

  
  make.solution(QTEMP, ofile= outsolut)


  prep.exam(MASTtex, V , instructor=instructor, examdate=examdate, course=course, examname=examname, instructions=instructions)


 
#### OLD:    prep.exam(MASTtex, V, exnumber=exnumber, seqnum=seqnum , examdate=examdate)

####  system(paste(sep=" ", "latex", outMAST ))

####  system(paste(sep=" ", "latex", outMAST ))

  
 
 #### system(paste(sep=" ", "dvips -Ppdf",  MASTdvi , " >" , MASTps ))
  
####  system(paste(sep=" ", "ps2pdf", MASTps,"  >", MASTpdf))

  cat("To get the final output, change directory to current directory.", sep="\n")
  cat("Execute the following system commands outside of R:", sep="\n")
  cat(paste(sep=" ", "latex", outMAST ), sep="\n")
  cat(paste(sep=" ", "latex", outMAST ), sep="\n")
  
 cat(paste(sep=" ", "dvips -Ppdf",  MASTdvi , " >" , MASTps ), sep="\n")
 cat(paste(sep=" ", "ps2pdf", MASTps,"  >", MASTpdf), sep="\n")

  
}

