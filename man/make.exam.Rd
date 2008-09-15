\name{make.exam}
\alias{make.exam}

\title{Make Exam}
\description{
 Given a question bank, create a test.
}
\usage{
make.exam(Qbank, ofile = "examq.tex")
}

\arguments{
  \item{Qbank}{Question bank list}
  \item{ofile}{Output file}
}
\details{
 Dumps out a tex file with the questions
}
\value{
 Side Effects - output to a TEX file.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{prep.exam}
\examples{

data(QBANK1)

\dontrun{
make.exam(QBANK1, ofile="exam1.tex")
}



}

\keyword{misc}
