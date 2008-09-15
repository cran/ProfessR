\name{DUMPbank}
\alias{DUMPbank}

\title{Dump a Question Bank}
\description{
 Save an ASCII version of a selected Question Bank
}
\usage{
DUMPbank(ofile, QB, sep = "\n", append=TRUE)
}

\arguments{
  \item{ofile}{character, output file}
  \item{QB}{QuestinBank Structure}
  \item{sep}{seperator between questions}
   \item{append}{logical, if FALSE a new file is created}
 
}

\value{
 Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{Get.testbank}
\examples{
data(QBANK1)
DUMPbank("my.questions", QBANK1, sep = "\n")

QB1=Get.testbank("my.questions")

}

\keyword{misc}
