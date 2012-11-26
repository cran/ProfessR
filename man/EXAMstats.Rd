\name{EXAMstats}
\alias{EXAMstats}
\title{Exam Statistics
}
\description{Statistical Analysis of
  Examination where the results are either
  correct or incorrect.  
}
\usage{
EXAMstats(j, key)
}
\arguments{
  \item{j}{matrix of student responses
}
  \item{key}{key of correct answers
}
}
\details{
  At this statge no partial credit is given.
}
\value{List
  \item{H}{Matrix: question, correct response, student responses,
    difficulty, Desc, BiSer}
  \item{kr20}{Kruder-Richardson reliability  statistic}
}
\references{
 Kuder, G. F., and Richardson, M. W. (1937). The theory of the estimation
  of test reliability. Psychometrika, 2(3), 151-160.

Cortina, J. M., (1993). What Is Coefficient Alpha? An Examination of Theory and Applications. 
Journal of Applied Psychology, 78(1), 98-104.

}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{There is a slightly different implementation
  if partial credit is employed.  See 
}
\seealso{readSCANTRON
}
\examples{
\dontrun{
B2 = readSCANTRON(rawfn2)

Estat = EXAMstats(B2$studans, B2$key)

Estat$kr20

}


}
\keyword{misc}
