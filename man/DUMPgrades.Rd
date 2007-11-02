\name{DUMPgrades}
\alias{DUMPgrades}
\title{Dump grades to a file}
\description{
  Dump grades to a file
}
\usage{
DUMPgrades(D1, file = NULL, id = NULL, names = NULL)
}
\arguments{
  \item{D1}{list output from do.grades}
  \item{file}{file name, a csv will be added as a suffix}
  \item{id}{vector of student IDs}
  \item{names}{character vector of student names}
}

\value{
  Side effects
}
\references{}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{do.grades}
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
DUMPgrades(D1, file="TEST1grades" )



}
\keyword{misc}
