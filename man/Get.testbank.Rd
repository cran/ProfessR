\name{Get.testbank}
\alias{Get.testbank}

\title{Get Test Bank From Ascii Text Files}
\description{
 Get Test Bank From Ascii Text Files
}
\usage{
Get.testbank(fn)
}

\arguments{
  \item{fn}{File Name}
}
\details{
  Structure of input file is strict: see the vignette
  for an example.
  Each questions starts with the
  tag QUESTION: (there is a space following the colon on all tags)
  followed by answers
  with the correct answer indicated by
  the tag ANSWER: .
  The tag FIG:  allows the examiner
  to include a figure with a latex tag
  for reference. For example:
  \samp{
    QUESTION: What was the world like during the Late Paleocene Torrid Age?
    ANSWER: a. Most of the world was wetter and warmer.
    b. Most of the world was drier and warmer.
    c. Most of the world was wetter, but a little cooler.
    d. Most of the world was a desert.
    e. It is impossible to estimate conditions at that time.
  }
}

\value{
  List: list of Questions 
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

\dontrun{
fn = "MY.questions"
Qbank = Get.testbank(fn)

##########  use existing database:
data(QBANK1)
#####  dump out question bank in correct format:
DUMPbank("my.questions", QBANK1, sep = "\n")
###  read it in:
QB1=Get.testbank("my.questions")
}

}
\keyword{misc}
