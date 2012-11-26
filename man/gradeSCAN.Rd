\name{gradeSCAN}
\alias{gradeSCAN}
\title{Grade a SCANTRON
}
\description{Grade each row of a matrix which is
  a record of the scanned anwers from a test.
}
\usage{
gradeSCAN(j, key)
}
\arguments{
  \item{j}{matrix,  scanned answers from the grading center
}
  \item{key}{vector, key for grading
}
}
\details{Program sums correct answers and returns the score for each row.
}
\value{vector of scores
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\keyword{misc}
