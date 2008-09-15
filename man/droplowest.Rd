\name{droplowest}
\alias{droplowest}
\alias{bestscores}

\title{Drop lowest grade
}
\description{Drop the lowest grade from a matrix of grades.  Matrix is
  assumed to be N by m where m is the number of exams (columns), N the number of
  students (rows)
}
\usage{
droplowest(z)
}
\arguments{
  \item{z}{Matrix of scores, rows are students, columns are exam scores
}
}
\details{Best matrix output is sorted, so the grades do not reflect the original
  order of exam scores.  To drop the two lowest scores, apply this
  program twice, running it a second time on the best outut.
}
\value{
  \item{minind}{Index of minimum score}
  \item{best}{matrix of scores with the lowest dropped}
  \item{midgrade}{mean value of best scores}
  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{do.grades
}
\examples{

#########   generate fake exam scores, 10 students, 3 exams
z = matrix(runif(3*10, 50, 100), ncol=3   )
 A  = droplowest(z)
 cbind(A$best,  A$minind, z, A$midgrade)


}
\keyword{misc}
