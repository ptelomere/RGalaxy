\name{galaxy}

\alias{galaxy}
\title{Make a function available in Galaxy}

\description{
    Makes an R function available in the Galaxy web platform.
    Automates all the work necessary to expose an R function
    in Galaxy. See the vignette for more information
}

\usage{
galaxy(func, manpage, galaxyOutputs, name, package=NULL, is.exported=NULL,
    paramList, version, galaxyConfig)
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
  \item{galaxyOutputs}{
    \bold{Required.}
    A list of one or more \code{\link{GalaxyOutput}} objects containing
    the names of files produced by \code{func}.
  }
  \item{name}{
    \bold{Required.}
    One word to describe the action this function performs. Becomes a clickable
    link in Galaxy.
  }
  \item{package}{
    The name of the package where \code{func} lives, or NULL (the default) if 
    it is not in a package. 
  }
  \item{is.exported}{
    If \code{package} is supplied, a logical indicating whether \code{func} is 
    exported from \code{package}.
  }
  \item{paramList}{
    \bold{Required.}
    A named list of \code{\link{GalaxyParam}} objects describing 
    each parameter to the function.
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


params <- list(
    GalaxyParam(name="inputfile1", type="data", label="Matrix 1"),
    GalaxyParam(name="inputfile2", type="data", label="Matrix 2"),
    GalaxyParam(name="plotTitle", type="text", label="Plot Title"),
    GalaxyParam(name="plotSubTitle", type="text", label="Plot Subtitle"))

galaxy(functionToGalaxify,
    manpage="functionToGalaxify",
    galaxyOutputs=list(GalaxyOutput("outputfile1.csv"),
        GalaxyOutput("outputfile2.csv")),
    name="Add",
    package="RGalaxy",
    paramList=params,
    version=packageDescription("RGalaxy")$Version,
    galaxyConfig=GalaxyConfig(galaxyHome, "mytool", "Test Section", "testSectionId"))

}

