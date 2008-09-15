\name{repair.id}
\alias{repair.id}
\title{Repair Poorly Bubbled Student ID}
\description{Repair Poorly Bubbled Student IDs by matching to
  a reliable data base of names and IDs.  Routine
  offers a set of possible matches if several
  may be appropriate.
}
\usage{
repair.id(sisroster, scrfn)
}
\arguments{
  \item{sisroster}{Reference Data set}
  \item{scrfn}{Scantron Output}
}
\details{
  Program searchers for missing ID's by
  attempting to match up names.
  
}
\value{
  \item{newid}{New vector of IDs that correspond
  to the scantron input}
 
}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\keyword{misc}
