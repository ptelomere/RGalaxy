\name{addTwoNumbersWithTest}

\alias{addTwoNumbersWithTest}
\title{Add two numbers (with functional test)}

\description{
    An example function that can be made into a Galaxy tool.
    Takes two numbers, adds them, and returns a file containing
    the result. This version demonstrates a functional test.
}

\usage{
addTwoNumbersWithTest(number1=GalaxyNumericParam(required=TRUE, testValues=5L),
        number2=GalaxyNumericParam(required=TRUE, testValues=5L),
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
addTwoNumbersWithTest(2, 2, t)
readLines(t, warn=FALSE)
runFunctionalTest(addTwoNumbersWithTest) ## should return TRUE

}

