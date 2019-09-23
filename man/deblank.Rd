\name{deblank}
\alias{deblank}
\title{Remove blanks from strings.
}
\description{Remove blanks from strings.
}
\usage{
deblank(a)
}
\arguments{
  \item{a}{Character string
}
}
\details{Removes all blanks from strings. The functuon works
on vectors of strings, removing blanks on each element.
}
\value{Character string with no blanks.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\examples{


j = c(' James', 'Jones ', 'Alpha Dog')

deblank(j)


}
\keyword{misc}
