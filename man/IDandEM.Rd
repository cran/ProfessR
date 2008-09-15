\name{IDandEM}
\alias{IDandEM}
\title{Match ID and Email file}
\description{
  Match ID and Email file
}
\usage{
IDandEM(scrfn, sisroster, sel = 1:2, hnote = "Exam Results", SEND = TRUE)
}
\arguments{
  \item{scrfn}{list(ID=number, nam="name on scantron") }
  \item{sisroster}{list(ID=number,  lastname='last name of student',  fullname='full name of student') }
  \item{sel}{numeric, index= specify for a specific student}
  \item{hnote}{text, subject line on E-mail }
  \item{SEND}{logical, if FALSE, do not send }
}
\details{
  A set of files has been separated and
  stored.  Each file is to sent to a different student with the exam
  results.
  
}
\value{
Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  The IDs of the reference data base (the roster) must match the
  IDs in the list of files.  If not, use repair.id to
  fix the scantron IDs
}
\seealso{repair.id}
\examples{

\dontrun{
##  read in the names of the files
zfile = scan(file="ALLIDS", list(name="", ID=0, tfile=""), sep="," )
##  read in a roster.  The roster has
##   email addresses that are attached tot he files
##  by matching the ID in the zfile with the IDs in the data base
load(file="/home/lees/Class/GEOL_105/Grades_2008/EXAM1/BB1.RDATA")

jroster = BB1

IDandEM(zfile, jroster, sel=1:10, hnote="GEOL105 EXAM3 Results", SEND=FALSE )
IDandEM(zfile, jroster,  hnote="GEOL105 EXAM3 Results", SEND=FALSE )

######## actual sending
IDandEM(zfile, jroster, hnote="GEOL105 EXAM3 Results", SEND=TRUE )



}




}
\keyword{misc}
