\name{UNCkeytron}
\alias{UNCkeytron}
\title{Create a KEY for the scantron
}
\description{Create a KEY for the scantron
}
\usage{
UNCkeytron(g, fout, LAB = "KEY")
}
\arguments{
  \item{g}{vector of correct answers
}
  \item{fout}{output file name
}
  \item{LAB}{Label to print on key
}
}
\details{Given a vector of correct answers the program will
  create a postscript file with a facsimile of the scantron used for
  examinations at  UNC Chapel Hill.  The Bubbles will be filled
  and can be used to prepare a number 2 pencil version.
  
}
\value{Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  Currently only eps outputs - future versions may be different.
  At this time, the code creates postscript code, which can be converted
  to png, pdf or other formats with software outside of R.
  In linux I use a perlscript,
   \verb{
     /home/lees/Progs/Perl/ps2png.prl files.eps
     }
  which, in turn, calls, epstopdf
  and
  \verb{
    gs -dBATCH -sDEVICE=png16m -dNOPAUSE -r200 -sOutputFile=$outpf $inpf
    }
}


\seealso{getKEY}


\examples{

\dontrun{

fkeyA = "/Users/lees/SCANTRON/A.FINAL.key"
fkeyB = "/Users/lees/SCANTRON/B.FINAL.key"
 FKEY1 = getKEY(fkeyA)

    FKEY2 = getKEY(fkeyB)

UNCkeytron(FKEY1, "AKEYfinal.eps", "A KEY final")
UNCkeytron(FKEY2, "BKEYfinal.eps", "B KEY final")


}




}
\keyword{misc}
