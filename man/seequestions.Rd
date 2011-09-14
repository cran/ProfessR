\name{seequestions}
\alias{seequestions}
\title{See Questions Sequentially
}
\description{Print questions to the screen
}
\usage{
seequestions(QB)
}
\arguments{
  \item{QB}{
}
}
\details{Prints just the questions to the screen.
}
\value{Prints to screen
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{seebank
}
\examples{

\dontrun{
LF = list.files(path="/home/lees/Class/GEOL_105/TESTBANK/EXAM_1", pattern="txt", full.names=TRUE )

kbank = vector(mode='list')
######   read in the question banks, each in one file
for(i in 1:length(LF))
  {
    h = Get.testbank(LF[i])
    kbank[[i]] = Get.testbank(LF[i])

  }
names(kbank) = LF

cbind( seequestions(kbank[[1]]) )



}






}
\keyword{misc}
