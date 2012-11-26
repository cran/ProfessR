\name{readSCANTRON}
\alias{readSCANTRON}
\title{Read Scantron 
}
\description{Read UNC scantron 
}
\usage{
readSCANTRON(fn = "t9543b.raw.csv", nq = 50, istart = 6)
}
\arguments{
  \item{fn}{character, name of digital file with raw scores
}
  \item{nq}{integer, Number of questions to read
}
  \item{istart}{integer, start of column for first question
}
}
\details{
  The data is scanned by machine.  If a student
  marks on the exam past the correct number of questions,
  the machine assumes there are legitimate repsonses beyond the key.
  
}
\value{list:
  \item{Nstudents}{number of students}
  \item{Nquestions}{number of questions} 
  \item{Nams}{names of students} 
  \item{ids}{Ids of students} 
  \item{studans}{matrix, student answers} 
  \item{key}{key for grading} 
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
\dontrun{

datadir =  "./DATA"
rawfn1 = paste(datadir,'t6200a.raw.csv', sep="/")

B1 = readSCANTRON(rawfn1)
}


}
\keyword{misc}
