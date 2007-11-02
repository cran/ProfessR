\name{show.dist}
\alias{show.dist}

\title{Show Distribution of Grades}
\description{
  Show Distribution of Grades
}
\usage{
show.dist(W)
}

\arguments{
  \item{W}{list output of do.grades}
}
\details{
  Print out the distribution of letter grades
}
\value{
 Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{do.grades}
\examples{
g = rnorm(n=130, m=82, sd=10)
g[g>100] = 100
g[g<1] = 1

B = boxplot(g)

divs = c(min(g), B$stats[1:4] + diff(B$stats)/2, max(g) )
D1 = do.grades(g, divs=divs, tit="GEOL 105 Exam 1")

show.dist(D1)

}

\keyword{misc}
