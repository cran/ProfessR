\name{ridNA}
\alias{ridNA}
\title{Replace NA with somehting else
}
\description{Replace NA with somehting else
}
\usage{
ridNA(z, temp)
}
\arguments{
  \item{z}{vector
}
  \item{temp}{replacement
}
}

\value{vector with NA's replaces
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

z = 1:10
z[z>8] = NA

ridNA(z, 0)



}
\keyword{misc}
