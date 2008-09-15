\name{prep.exam}
\alias{prep.exam}

\title{Prepare Exam for Latex (simple style)}
\description{
  Prepare Exam for Latex - use simple styles
}
\usage{
prep.exam(OF, incfile, instructor="", examdate="", course="",  examname="", instructions="")
}

\arguments{
  \item{OF}{Character string output files}
  \item{incfile}{Character, include file name for questions}
  \item{instructor}{}
  \item{examdate}{Date of the examination}
  \item{course}{Name of the course, character}
  \item{examname}{title of exam}
  \item{instructions}{character vector of instructions}
 
}

\value{
  Side Effects

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{version.exam}
\examples{



\dontrun{
######  since the program produces a file on the local
######   system, do not run this example


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



instructions=c("There are 50 questions.", "Answer all questions.", "Mark clearly.")
\dontrun{
prep.exam(outMAST, outtex , instructor=Me, examdate=examdate, course=course, examname=examname, instructions=instructions)
}

}



}

\keyword{misc}
