\name{galaxify}

\alias{galaxify}
\title{Make a function available in Galaxy}

\description{
}

\usage{
galaxify(func, func.name, manpage, galaxy.home, name, 
    package=NULL, is.exported=NULL,
    param.list, section.name, section.id, tool.dir, version)
}

\arguments{
  \item{func}{
    \bold{Required.}
    The function to make available in Galaxy. This function should be 
    entirely-self contained, and should accept as arguments the full paths
    to its input and output file(s).
  }
  \item{func.name}{
    \bold{Required.}
    The name of the function in \code{func}.
  }
  \item{manpage}{
    \bold{Required.}
    The full path to the Rd-formatted manual page for the function, 
    or if \code{package} is specified, an alias that will pull up that manpage.
  }
  \item{galaxy.home}{
    \bold{Required.}
    The full path to the galaxy distribution on the local machine.
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
  \item{param.list}{
    \bold{Required.}
    A named list of \code{\link{GalaxyParam}} objects describing 
    each parameter to the function.
  }
  \item{section.name}{
    \bold{Required.}
    The name of the section of tools where \code{func} should appear.
  }
  \item{section.id}{
    \bold{Required.}
    The id of the section of tools where \code{func} should appear.
  }
  \item{tool.dir}{
    \bold{Required.}
    The directory under \code{galaxy.home}/tools where \code{func}
    and its associated XML file will be stored. Will be created 
    if it does not already exist.
  }
  \item{version}{
    The version of this function. If \code{package} is \code{TRUE},
    defaults to the version of \code{package}.
  }
}

\value{
  invisible(NULL)
}

\seealso{
}

\examples{
## set up galaxy.home
galaxy.home <- tempdir()
dir.create(galaxy.home, recursive=TRUE, showWarnings=FALSE)
file.copy(system.file("galaxy", "tool_conf.xml", package="RGalaxy"),
    file.path(galaxy.home, "tool_conf.xml"), overwrite=FALSE)


params <- list(
    GalaxyParam(name="inputfile1", type="data", label="Matrix 1"),
    GalaxyParam(name="inputfile2", type="data", label="Matrix 2"),
    GalaxyParam(name="plotTitle", type="text", label="Plot Title"),
    GalaxyParam(name="plotSubTitle", type="text", label="Plot Subtitle"),
    GalaxyParam(name="outputfile1", type="output", format="csv"),
    GalaxyParam(name="outputfile2", type="output", format="pdf"))

galaxify(functionToGalaxify, "functionToGalaxify",
    manpage="functionToGalaxify",
    galaxy.home=galaxy.home, name="Add", 
    package="RGalaxy",
    param.list=params, tool.dir="mytool",
    version=packageDescription("RGalaxy")$Version,
    section.name="Test Section",
    section.id="testSectionId")

}

