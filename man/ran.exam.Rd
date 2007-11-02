\name{ran.exam}
\alias{ran.exam}

\title{Random order of Exam}
\description{
  Randomly re-order the questions in a Question Bank 
 
}
\usage{
ran.exam(Qbank)
}

\arguments{
  \item{Qbank}{Question Bank List}
}
\details{
 randomly re-order the questions in a Question Bank
}
\value{
 Question bank
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{Get.testbank}
\examples{
data(QBANK1)
NEWQB = ran.exam(QBANK1)

}

\keyword{misc}
