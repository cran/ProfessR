\name{GetStudentNames}
\alias{GetStudentNames}
\title{Extract Student Names from Roster.
}
\description{
Given a roster of students, with (lastname, first name)
format, extract a unique set of first names, with no blanks.
}
\usage{
GetStudentNames(c1, dup.lets=1)
}
\arguments{
  \item{c1}{Character vector
}
\item{dup.lets}{NUmber of letters to add from last
name in the event that first names are duplicated.}
}
\details{
The function assumes the names are comma separated with
lastname, firstname order.  The code separates the names,
removes blanks from the first name, and finds a unique set of names.
If first names are not unique, the function extracts the first letters of
the last names and the duplicated names and appends with a period.
}
\value{
Character vector of unique first names
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

g.first =c("Jason","Skyler","Adrian","Berkley","Jack",'David',
'David', 'Jim', 'Jim')
g.last =c('Joyce', 'Einstein', 'Hertz', 'Bailey', 'Compton',
'Jones', 'Wilson', 'Smith', 'Anderson' )

c2 = paste(g.last, g.first, sep=', ')

K = GetStudentNames(c2)

}
\keyword{misc}
