\name{ProfessR-package}
\alias{ProfessR-package}
\alias{ProfessR}
\docType{package}
\title{
Grades Setting and Exam Maker
}
\description{
  Programs to determine student grades and create
  examinations from Question banks.  Programs will create numerous
  multiple choice exams,
  randomly shuffled, for different versions of same question list.
}
\details{
\tabular{ll}{
Package: \tab ProfessR\cr
Type: \tab Package\cr
Version: \tab 1.1-1\cr
Date: \tab 2008-09-15\cr
License: \tab GPL\cr
}

}
\author{
Jonathan M. Lees

Maintainer: Jonathan M. Lees<jonathan.lees@unc.edu>

}

\keyword{ package }

\examples{

################  making tests:

data(QBANK1)
make.exam(QBANK1, ofile="exam1.tex")



#################  setting grades:
g = rnorm(n=130, m=82, sd=10)
g[g>100] = 100
g[g<1] = 1

B = boxplot(g)

divs = c(min(g), B$stats[1:4] + diff(B$stats)/2, max(g) )

D1 = do.grades(g, divs=divs, tit="GEOL 105 Exam 1")




}
