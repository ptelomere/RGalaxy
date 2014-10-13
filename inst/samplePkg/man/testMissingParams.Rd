\name{testMissingParams}
\alias{testMissingParams}
\title{Test which parameters are supplied and what values they have.}
\usage{
  testMissingParams(requiredParam = GalaxyCharacterParam(),
    paramWithDefault = GalaxyIntegerParam(1),
    optionalParam = GalaxyCharacterParam(),
    outfile = GalaxyOutput("myoutput", "txt"))
}
\arguments{
  \item{requiredParam}{A required parameter}

  \item{paramWithDefault}{A parameter with a default value}

  \item{optionalParam}{An optional parameter}

  \item{outfile}{where to write diagnostic output}
}
\description{
  This function displays the values of its parameters, or
  'missing' if a parameter value is not supplied.
}
\details{
  These are the details.
}

