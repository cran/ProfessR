\name{fix.names}
\alias{fix.names}
\title{Fix Down Loaded Names 
}
\description{
  Fix names to remove problematic
  alphanumeric characters like spaces, quotes
}
\usage{
fix.names(nam,  upper=FALSE, lower=FALSE)
}
\arguments{
  \item{nam}{string
}
  \item{upper}{logical, TRUE= convert to upper case
  }
 \item{lower}{logical, TRUE= convert to lower case
}
  
}
\details{Currently only space, single and double quotes.
}
\value{string, with quaote replaced with underscore
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

####  examples with embedded quotes are not available
####    because they interfere with R documentation

LAM = "SILENCED LAMB"
fix.names(LAM, lower=TRUE)

LAM = "Silence my Lamb"
fix.names(LAM, upper=TRUE)

LAM = "SILeNCED LAMB"
fix.names(LAM)

###   try with single quote
LAM = "O'brian LAMB"
fix.names(LAM)


}
\keyword{misc}
