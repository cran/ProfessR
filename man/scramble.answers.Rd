\name{scramble.answers}
\alias{scramble.answers}
\title{Scramble Answers}
\description{
  Randomly rearrange answers within a question of a test bank
}
\usage{
scramble.answers(Qbank)
}

\arguments{
  \item{Qbank}{Question Bank (list of Questions) }
}
\details{
 Takes the given list of questions, and returns same list with answers scrambled.
}
\value{
 Question Bank List
}
\note{

  Since some question require that the
  answers be ordered in a certain way, these are not Randomized
  in this scrambling process.
  These include:

  \code{
  c("all of the above",
        "none of the above",
        "None of these are correct",
        "all of the choices are correct",
        "All of the choices are correct",
        "Both choices are correct",
        "None of the choices are correct",
        "Both of the choices are correct",
        "All of these are correct",
        'Neither of these are correct')
	}

}



\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{Get.testbank}
\examples{

data(QBANK1)


QBANK1[[35]]

 NEWQB = scramble.answers(QBANK1) 
NEWQB[[35]]

}

\keyword{misc}
