\name{checkgrades}
\alias{checkgrades}
\title{Check Grade Distribution}
\description{
  View grades sorted and listed
  with raw score, letter and scaled score, with optional ID and name
}
\usage{
checkgrades(D1, id = NULL, names = NULL)
}
\arguments{
  \item{D1}{output of do.grades}
  \item{id}{character vector, ID for students}
  \item{names}{character vector, names of students}
}

\value{
  Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{do.grades, DUMPgrades}
\examples{

g = rnorm(n=130, m=82, sd=10)
g[g>100] = 100
g[g<1] = 1

B = boxplot(g)

divs = c(min(g), B$stats[1:4] + diff(B$stats)/2, max(g) )



###  to run interactively, remove the divs
###  D1 = do.grades(g, tit="GEOL 105 Exam 1")

### otherwise use previously calculated divs:
D1 = do.grades(g, divs=divs, tit="GEOL 105 Exam 1")
checkgrades(D1 )



}
\keyword{misc}
