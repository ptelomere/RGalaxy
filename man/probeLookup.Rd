\name{probeLookup}

\alias{probeLookup}
\title{Get the PFAM and SYMBOL names for a set of Affymetrix probe IDs}

\description{
    Given a space-separated list of Affymetrix probe IDs, return
    the PFAM and SYMBOL names. Uses \code{hgu95av2.db} and the
    \code{select} method from the \code{AnnotationDbi} package.
}

\usage{
probeLookup(probe_ids=GalaxyCharacterParam(
  required=TRUE,
  testValues="1002_f_at 1003_s_at"),
    outputfile=GalaxyOutput("probeLookup", "csv"))
}

\arguments{
  \item{probe_ids}{
    A space-separated list of Affymetrix probe IDs.
  }
  \item{outputfile}{
    The name of a .csv file where the returned output is to be written.
  }
}

\value{
  invisible(NULL)
}

\examples{
  t <- tempfile()
  probeLookup("1002_f_at 1003_s_at", t)
  readLines(t)
}

\details{
    Given one or more Affymetrix probe IDs separated by spaces,
    this function will return the PFAM and SYMBOL names associated
    with them in human. The function uses the \code{hgu95av2.db}
    package and the \code{select} function in \code{AnnotationDbi}.
}


