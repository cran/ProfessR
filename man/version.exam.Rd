\name{version.exam}
\alias{version.exam}

\title{Create 1 instance of a specific Exam}
\description{
  Create 1 instance of a specific Exam
}
\usage{
version.exam(Qbank, V, exnumber = "Exam 1", seqnum = "2", examdate ='', instructor="",  course="", instructions="")
}

\arguments{
  \item{Qbank}{question bank}
  \item{V}{Character string output files}
  \item{exnumber}{Exam number}
  \item{seqnum}{Version Number}
  \item{examdate}{Date of the examination}
  \item{instructor}{character, name of teacher}
  \item{course}{character, nqme of course}
  \item{instructions}{vector of character strings}
}

\value{
 Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{ran.exam, make.exam, prep.exam}
\examples{
##
data(QBANK1)

examdate="THURS Sep 20 2007"

version.exam(QBANK1, "exam1A" , exnumber="Exam 1", seqnum="1", examdate=examdate)




examdate="THURS Sep 20 2007"


seqnum="1"
exnumber="Exam 1"
V = "exam1A"
outtex = paste(sep=".",V, "tex" )
outMAST  = paste(sep="", V, "MAST" )


MASTtex  = paste(sep=".", outMAST , "tex" )

outsolut  = paste(sep="", V, "solutions.tex" )
Me = "Jonathan M. Lees"

course="GEOL 105"

examname=paste(sep=" ", exnumber, "Seq", seqnum)

K = length(QBANK1)

instructions=c(
paste(sep=" ", "There are",K," number of questions."),
"Answer all questions.","Use number 2 pencil",
"Mark each box clearly.")



version.exam(QBANK1, "exam1B" , exnumber="Exam 1", seqnum="B",
examdate=examdate, instructor=Me, course=course , instructions=instructions)



}

\keyword{misc}
