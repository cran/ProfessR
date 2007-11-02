\name{jist}
\alias{jist}

\title{Add letter grades to histogram}
\description{
 Given a vector of grades, add the letters to an existing histogram.
}
\usage{
jist(h, Z, L, col = 2)
}

\arguments{
  \item{h}{histogram list}
  \item{Z}{grades from original data}
  \item{L}{letters associated with grades}
  \item{col}{color for plotting letters}
}
\details{
  This will add information on an existing histogram plot.
}
\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{do.grades}
\examples{


g = rnorm(130, m=82, sd=10)
g[g>100] = 100
g[g<1] = 1

B = boxplot(g)


divs = c(min(g), B$stats[1:4] + diff(B$stats)/2, max(g) )

####G1  = do.grades(g, cut=20, tit="GEOL 105 Exam 1")

##########  replot with existing divisions:
D1 = do.grades(g, divs=divs, tit="GEOL 105 Exam 1")

jist(D1$hist, D1$grades, D1$lett)


}

\keyword{aplot}
