\name{getFriendlyName}

\alias{getFriendlyName}
\title{Change a camelCase name to a friendlier version}

\description{
    Takes a string like "fooBarBaz" and returns "Foo Bar Baz".
}

\usage{
getFriendlyName(camelName)
}

\arguments{
  \item{camelName}{
    The "camelCased" name to make friendly.
  }
}

\value{
  The friendly version of the camel-cased name.
}

\seealso{
    \code{\link{galaxy}},
    \code{\link{GalaxyConfig}}, \code{\link{GalaxyOutput}}
}
\details{
    Used by \code{galaxy()} to create default labels based on function
    and parameter names.
}

\examples{
getFriendlyName("fooBarBaz")
}

