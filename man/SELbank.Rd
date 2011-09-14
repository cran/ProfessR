\name{SELbank}
\alias{SELbank}
\title{Select Questions from a bank
}
\description{Select, random set of questions
  from a test bank.
}
\usage{
SELbank(QB, N)
}
\arguments{
  \item{QB}{Question bank
}
  \item{N}{integer, number of questions to select
}
}
\details{Progam uses sample to get a random perturbation,
  and then pulls out the first N questions
}
\value{Question bank
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{Get.testbank
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

##########   get 50 sample questions
NEWQB = SELbank(Kbank, 50)

}

}
\keyword{misc}
