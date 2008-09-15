\name{DUPbank}
\alias{DUPbank}
\title{Find Duplicate Questions}
\description{
  Finds dupliucated questions in a set of Question Banks
}
\usage{
DUPbank(Qbank)
}
\arguments{
  \item{Qbank}{a list of Question Banks}
}
\details{
  The program only checks the questions, not the answers.
  One could thus have several questions with the same
  wording, but different answers.  I might change this in the future.
  Given the list of duplicated questions one should edit the
  original question bank files to remove them.
}
\value{
  \item{A}{vector of duplicated questions}
  \item{F}{vector of duplicated files where the questions were extracted}
   \item{I}{vector of duplicated indexes  where the questions were extracted}
   \item{N}{vector of duplicated  indexes where the questions were extracted}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{
data(QBANK1)

###  force some questions to be duplicates:
QBANK1[[51]]=QBANK1[[25]]
QBANK1[[52]]=QBANK1[[12]]
QBANK1[[14]]=QBANK1[[4]]


DQ = DUPbank(QBANK1)

DQ
}
\keyword{misc}
