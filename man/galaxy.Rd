\name{galaxy}

\alias{galaxy}
\title{Make a function available in Galaxy}

\description{
    Makes an R function available in the Galaxy web platform.
    Automates all the work necessary to expose an R function
    in Galaxy. See the vignette for more information
}

\usage{
galaxy(func, 
        package=getPackage(func),
        manpage=func, 
        name=getFriendlyName(func),
        version=getVersion(func),
        galaxyConfig,
        dirToRoxygenize,
        RserveConnection=NULL,
        path.to.R="",
        functionalTestDirectory)

}

\arguments{
  \item{func}{
    \bold{Required.}
    TA character vector naming the function to make available in Galaxy.
    This function should be     entirely-self contained, and should accept
    as arguments the full paths to its input and output file(s).
  }
  \item{package}{
    The name of the package where \code{func} lives, or NULL if 
    it is not in a package. By default, \code{RGalaxy} will try to determine
    which package the code lives in and set the parameter to NULL if it 
    cannot.
  }

  \item{manpage}{
    The full path to the Rd-formatted manual page for the function, 
    or if the \code{func} is in a package, an alias that will pull up
    that manpage.
  }
  \item{name}{
    Text describe the action this function performs. Becomes a clickable
    link in Galaxy. By default, if your function is called "fooBar", 
    \code{name} is set to "Foo Bar".
  }
  \item{version}{
    The version of this function. If \code{func} lives in a package,
    defaults to the version of \code{package}.
  }
  \item{galaxyConfig}{
    \bold{Required.}
    A \code{link{GalaxyConfig}} object describing your
    Galaxy configuration.
  }
  \item{dirToRoxygenize}{
    If present, points to the directory of a source package upon which to run
    \code{roxygenize()} from the \code{roxygen2} package, creating manual
    pages from source code comments.
  }
  \item{RserveConnection}{
    If set (by calling \code{\link{RserveConnection}}), 
    sends the function to a running Rserve for evaluation. This can
    make functions run faster since dependencies have already been loaded.
    If NULL (the default), function is run normally.
    See vignette for more information.
  }
  \item{path.to.R}{
  If not empty, Galaxy will look for R in this directory. This is 
  useful if there are multiple versions of R on your system and your
  Galaxy configuration prevents you from setting your PATH to point
  to the right one.
  }
  \item{functionalTestDirectory}{
  If \code{package} is not specified, this directory is searched
  for functional test fixtures.
  }
}

\value{
  invisible(NULL)
}

\seealso{
   \code{\link{GalaxyConfig}},
   \code{\link{GalaxyOutput}},
   \code{\link{GalaxyParam}}

}

\examples{
## set up galaxyHome (you don't need to do this if 
## you really have Galaxy installed)
galaxyHome <- tempdir()
dir.create(galaxyHome, recursive=TRUE, showWarnings=FALSE)
file.copy(system.file("galaxy", "tool_conf.xml", package="RGalaxy"),
    file.path(galaxyHome, "tool_conf.xml"), overwrite=FALSE)

\dontrun{
galaxy("functionToGalaxify",
    galaxyConfig=GalaxyConfig(galaxyHome, "mytool",
      "Test Section", "testSectionId"),
    RserveConnection=RserveConnection())
}
}