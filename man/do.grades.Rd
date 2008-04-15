\name{do.grades}
\alias{do.grades}

\title{Do Grades}
\description{
 Calculate the grades of a class of students, given raw scores on exam
}
\usage{
do.grades(ggrades, divs = NULL, cut = 0, tit = "Exam Grades",
breaks=length(ggrades)/3, ...)
}

\arguments{
  \item{ggrades}{Raw grades}
  \item{divs}{divisions for grades (optional)}
  \item{cut}{low end Cut off to remove 0 from statistics }
  \item{tit}{Title for Figure}
  \item{breaks}{breaks for the histogram, default=length(ggrades)/3}
  \item{\dots}{other parameters for hist}
}
\details{
  To remove students who do not take the test a low end cut off
  is used to excise any grades below that level.  Both mean, and
  standard deviations are shown as well as median and quartiles.
}
\value{
  grades=ggrades, lett=letts, scor=scores, divs=divs, LETS=LETS, SCRS=SCRS, hist=HA
 LIST:
 \item{grades}{raw scores}
 \item{lett}{letter grades}
 \item{scor}{scaled grades}
 \item{divs}{divisions, estimated by user or provided as input}
 \item{LETS}{letter grades assigned}
 \item{SCRS}{Scores related to LETS}
 \item{hist}{histogram structure}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Grades are determined linearly within a division
}

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


}

\keyword{misc}
\keyword{hplot}
\keyword{iplot}
