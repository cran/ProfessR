\name{phist}
\alias{phist}
\title{Plot Histogram with Grades labeled}
\description{Plot Histogram with Grades labeled
}
\usage{
phist(G, Z = 1, L = 1, col = 2, add = FALSE, tit = "GEOL 105 Exam 1")
}
\arguments{
  \item{G}{Histogram list from do.grades}
  \item{Z}{numerical grades}
  \item{L}{text, vector, Letter Grades}
  \item{col}{color for text}
  \item{add}{logical, add=TRUE, add to existing plot}
  \item{tit}{title for plot}
}

\value{
  List:
  \item{x}{x location on plot}
  \item{y}{y location on plot}
    \item{L}{Label printed}
}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\seealso{do.grades}
\examples{
\dontrun{
newID3 =  repair.id(DBB, raw3)
raw3$id=newID3
raw3$ID=newID3


}



}
\keyword{misc}
