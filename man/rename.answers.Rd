\name{rename.answers}
\alias{rename.answers}

\title{Rename Answers}
\description{
  Rename the answers on a Question Bank
}
\usage{
rename.answers(Qbank, newnames = letters[1:26], sep = ") ")
}

\arguments{
  \item{Qbank}{Question Bank}
  \item{newnames}{vector of new names}
  \item{sep}{separator between name of Answer and Answer String}
}
\details{
  Takes the given list of questions, and returns same list with answers
  rpefaces by a different set of itemizers
}
\value{
 Question Bank List
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{Get.testbank}
\examples{
data(QBANK1)


newnames=letters[1:26]
NEWQB = rename.answers(QBANK1, newnames=newnames ) 
NEWQB[[35]]

newnames=1:26
NEWQB = rename.answers(QBANK1, newnames=newnames ) 
NEWQB[[35]]

newnames=LETTERS[1:26]
NEWQB = rename.answers(QBANK1, newnames=newnames ) 
NEWQB[[35]]


}

\keyword{misc}
