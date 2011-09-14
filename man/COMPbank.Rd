\name{COMPbank}
\alias{COMPbank}
\title{Compare Question Banks
}
\description{Compare two question banks to find duplication
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
\value{Vector index of QB2 that are not
  found in QB1.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Only the questions are compared,
  the answers are ingnored.
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
}


}
\keyword{misc}
