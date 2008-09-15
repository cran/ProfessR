\name{getlet}
\alias{getlet}
\alias{getlet2}

\title{Get Letter Grades}
\description{
 Get letter grades from list of numeric scores
}
\usage{
getlet(ggrades, divs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ggrades}{vector of grades}
  \item{divs}{numerical vector of divisions}
}
\details{
  Returns letter grades scaled linearly between divisions.
}
\value{
 
LIST:
  \item{ggrades}{Input grades}
  \item{lett}{letter values}
  \item{scor}{scores after scaling}
  \item{divs}{divisions used in setting scores}
  \item{LETS}{Letters for grades}
  \item{SCRS}{numeric divisions used for LETS}

    \item{olett}{letter values, older version}
    \item{oscor}{scores after scaling, older version binned }
    
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{do.grades}
\examples{

g = rnorm(130, m=82, sd=10)

g[g>100] = 100
g[g<1] = 1



B = boxplot(g)


divs = c(min(g), B$stats[1:4] + diff(B$stats)/2, max(g) )

G = getlet(g, divs)

cbind(G$LETS, G$SCRS)



data.frame(G$grades, G$lett, G$scor)



}

\keyword{misc}
