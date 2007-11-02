\name{QBANK1}
\alias{QBANK1}
\alias{QBANK2}

\docType{data}
\title{Example Question Bank}
\description{
 Example Question Bank, 50 question, multiple Choice
}
\usage{data(QBANK1)}
\format{
List:
  \item{Q}{Question in latex format (character string)}
  \item{A}{Possible Answers in latex format (vector of character strings)}
  \item{a}{Correct Answer in latex format (character string)}
  \item{numANS}{index number corresponding to correct answer}
  \item{FIG}{character: full path to figure, tag for figure}
}
\details{

  An example input question in ascii format is
  constructed using three tag identifiers:
  "QUESTION:", "ANSWER:" and (optionally) "FIG:".
  The format is shown here:
  


}


\examples{
data(QBANK1)
## maybe str(QBANK1) ; plot(QBANK1) ...
print(QBANK1[[1]])


}
\keyword{datasets}
