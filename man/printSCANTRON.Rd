\name{printSCANTRON}
\alias{printSCANTRON}
\title{Print Scantron
}
\description{Print results from scantron center
}
\usage{
printSCANTRON(B1)
}
\arguments{
  \item{B1}{
    list, output of readSCANTRON:
    must have elements studans, Nams, ids
}
}

\value{side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Prints the matrix returned from the scantron center.
}


\seealso{readSCANTRON
}
\examples{
\dontrun{

datadir =  "./DATA"
rawfn1 = paste(datadir,'t6200a.raw.csv', sep="/")

B1 = readSCANTRON(rawfn1)
printSCANTRON(B1)

}



}
\keyword{misc}
