\name{COMPbank}
\alias{COMPbank}
\title{Compare Question Banks
}
\description{Compare two question banks to find non-duplicated
  questions
}
\usage{
COMPbank(Qbank1, Qbank2)
}
\arguments{
  \item{Qbank1}{Question Bank 1
}
  \item{Qbank2}{Question Bank 2
}
}

\details{Uses match to find matching questions
  in the two question banks.  
}
\value{Vector index of questions in Qbank2 that are not
  found in Qbank1. 
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Only the questions are compared,
  the answers are ingnored.  The return vector will be
  a set of questions that are not duplicated,
  i.e. unique to question bank 2.
}


\seealso{SELbank
}
\examples{

\dontrun{
LF = list.files(path="/home/lees/Class/GEOL_105/TESTBANK/EXAM_1", pattern="txt", full.names=TRUE )

kbank = vector(mode='list')
######   read in the question banks, each in one file
for(i in 1:length(LF))
  {
    h = Get.testbank(LF[i])
    kbank[[i]] = Get.testbank(LF[i])

  }
names(kbank) = LF
Kbank =  vector(mode='list')

for(i in 1:length(kbank))
  {

Kbank = c(Kbank, kbank[[i]])

  }

q2 = COMPbank(Kbank, kbank[[3]] )

###########  to extract these:
subq2 = subsetbank(kbank[[3]] , q2)
###########  to get the overlapping questions:

olap = 1:length(kbank[[3]])
olap[-q2]


}


}
\keyword{misc}
