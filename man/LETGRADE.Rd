\name{LETGRADE}
\alias{LETGRADE}

\title{Letter Grade}
\description{
  given a numeric grade return a letter grade
}
\usage{
LETGRADE(g)
}

\arguments{
  \item{g}{numeric grade between 1-100}
}
\details{
 returns a grade based on a 4 point spread
}
\value{
  character vector of grades
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Failing grade is "E" by default.
  There is no "A+" in this program (UNC policy)
}

\examples{
g = rnorm(25, m=82, sd=10)
g[g>100] = 100
g[g<1] = 1


L = LETGRADE(g)

cbind(g, L)


}

\keyword{misc}
