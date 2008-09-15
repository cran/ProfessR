\name{CHECKbank}
\alias{CHECKbank}
\title{Check a set of Question banks}
\description{
  Sequentially check a set of Question banks.
   Makes sure there is a QUESTION: and ANSWER for each question.
}
\usage{
CHECKbank(QB)
}
\arguments{
  \item{QB}{list of question banks}
}

\value{
  Printed Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{seebank}

\examples{
 data(QBANK1)
CHECKbank(QBANK1)

############  modify by inserting an error:
QBANK1[[4]]$numANS=NULL

### recheck:
CHECKbank(QBANK1)


}

\keyword{misc}
