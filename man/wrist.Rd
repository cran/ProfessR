\name{wrist}
\alias{wrist}

\title{Write Histogram}
\description{
 Write grades on Histogram
}
\usage{
wrist(DB)
}

\arguments{
  \item{DB}{Output of do.grades}
}
\details{
Used internally in plotting programs  
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

hist(g)
wrist(D1)

}

\keyword{misc}
