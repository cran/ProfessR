\name{getgroup}
\alias{getgroup}
\title{Create Groupings of Students
}
\description{Create groups of students and plot groups to screen.
}
\usage{
getgroup(g.first, n = 2)
}
\arguments{
  \item{g.first}{Character vector of student names.
}
  \item{n}{number per group
}
}
\details{Class roster will be divided into n groups and displayed on the
the screen.
}
\value{List of groups with names.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
The class is currently randomized in this version.
}


\seealso{GetStudentNames
}
\examples{


g.last =c('Joyce', 'Einstein', 'Hertz', 'Bailey',
'Compton', 'Jones', 'Wilson', 'Smith', 'Anderson' )

getgroup(g.last, n = 3)

}
\keyword{misc}
