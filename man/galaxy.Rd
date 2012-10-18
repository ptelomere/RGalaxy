\name{galaxy}

\alias{galaxy}
\title{Make a function available in Galaxy}

\description{
    Makes an R function available in the Galaxy web platform.
    Automates all the work necessary to expose an R function
    in Galaxy. See the vignette for more information
}

\usage{
galaxy(func, manpage, ..., name=getFriendlyName(deparse(substitute(func))), 
    package=NULL, is.exported=NULL, version, galaxyConfig, packageSourceDir,
    useRserve=FALSE)
}

\arguments{
  \item{func}{
    \bold{Required.}
    The function to make available in Galaxy. This function should be 
    entirely-self contained, and should accept as arguments the full paths
    to its input and output file(s).
  }
  \item{manpage}{
    \bold{Required.}
    The full path to the Rd-formatted manual page for the function, 
    or if \code{package} is specified, an alias that will pull up that manpage.
  }
  \item{...}{
    \bold{Required.}
    A list of at least one \code{\link{GalaxyParam}} object and at
    least one \code{\link{GalaxyOutput}} objects.
  }
  \item{name}{
    \bold{Required.}
    Text describe the action this function performs. Becomes a clickable
    link in Galaxy. By default, if your function is called "fooBar", 
    \code{name} is set to "Foo Bar".
  }
  \item{package}{
    The name of the package where \code{func} lives, or NULL (the default) if 
    it is not in a package. 
  }
  \item{is.exported}{
    If \code{package} is supplied, a logical indicating whether \code{func} is 
    exported from \code{package}.
  }
  \item{version}{
    The version of this function. If \code{package} is \code{TRUE},
    defaults to the version of \code{package}.
  }
  \item{galaxyConfig}{
    \bold{Required.}
    A \code{link{GalaxyConfig}} object describing your
    Galaxy configuration.
  }
  \item{packageSourceDir}{
    If present, points to the directory of a source package upon which to run
    \code{roxygenize()} from the \code{roxygen2} package, creating manual
    pages from source code comments.
  }
  \item{useRserve}{
    Sends the function to a running Rserve for evaluation. This can
    make functions run faster since dependencies have already been loaded.
  }
}

\value{
  invisible(NULL)
}

\seealso{
    \code{\link{GalaxyParam}}, \code{\link{GalaxyConfig}},
    \code{\link{GalaxyOutput}}
}

\examples{
## set up galaxyHome (you don't need to do this if 
## you really have Galaxy installed)
galaxyHome <- tempdir()
dir.create(galaxyHome, recursive=TRUE, showWarnings=FALSE)
file.copy(system.file("galaxy", "tool_conf.xml", package="RGalaxy"),
    file.path(galaxyHome, "tool_conf.xml"), overwrite=FALSE)


galaxy(functionToGalaxify,
    manpage="functionToGalaxify",
    package="RGalaxy",
    version=packageDescription("RGalaxy")$Version,
    galaxyConfig=GalaxyConfig(galaxyHome, "mytool", "Test Section", "testSectionId"))

}

