\name{SEARCHbank}
\alias{SEARCHbank}
\title{Search Question Bank for Keyword
}
\description{Search a question bank for key words.
}
\usage{
SEARCHbank(gw, y = "humidity")
}
\arguments{
  \item{gw}{Question Bank
}
  \item{y}{key word
}
}
\details{Dumps to the screen the
  questions that match the key.
}
\value{Side effects - dumps to the screen.
  returns a vector of questions that match.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{seebank,Get.testbank,SELbank,COMPbank 
}


\examples{

\dontrun{
####  seebank program is interactive -
data(QBANK1)
SEARCHbank(QBANK1, "humidity" )
}



}
\keyword{misc}
