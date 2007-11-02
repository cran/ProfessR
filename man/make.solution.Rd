\name{make.solution}
\alias{make.solution}

\title{Create Solution File}
\description{
 Create Solution File in Latex
}
\usage{
make.solution(Qbank, ofile = "answers.tex")
}

\arguments{
  \item{Qbank}{Question Bank}
  \item{ofile}{Output File}
}
\details{
  Creates a latex file suitable for printing
  solution to the exam.
}
\value{
 Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

data(QBANK1)


 make.solution(QBANK1, ofile= "solutions.tex")


}

\keyword{misc}
