\name{subsetbank}
\alias{subsetbank}
\title{Subset a Question Bank
}
\description{Extract a subset from a question bank
}
\usage{
subsetbank(QBANK, sel)
}
\arguments{
  \item{QBANK}{Question Bank List
}
  \item{sel}{integer vector of index to specific questions
}
}
\details{for selecting specific questions
}
\value{Question Bank with selections
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{SELbank, COMPbank
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

##########   get 50 odd numbered sample questions

isel = seq(from=1, to=100, by=2)

oddset1 = subsetbank(Kbank, isel)

}


}
\keyword{misc}
