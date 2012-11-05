\name{addTwoNumbers}

\alias{addTwoNumbers}
\title{Add two numbers}

\description{
    An example function that can be made into a Galaxy tool.
    Takes two numbers, adds them, and returns a file containing
    the result.
}

\usage{
addTwoNumbers(number1=GalaxyNumericParam(required=TRUE),
        number2=GalaxyNumericParam(required=TRUE),
        sum=GalaxyOutput("sum", "txt"))
}

\arguments{
  \item{number1}{
    The first number to add.
  }
  \item{number2}{
    The second number to add.
  }
  \item{sum}{
    Where the result of the addition should be written.
  }
}

\value{
  invisible(NULL)
}

\seealso{
    \code{\link{galaxy}}, 
    \code{\link{GalaxyConfig}}, \code{\link{GalaxyOutput}},
    \code{\link{addTwoNumbers}}

}

\examples{
t <- tempfile()
addTwoNumbers(2, 2, t)
readLines(t, warn=FALSE)
}

