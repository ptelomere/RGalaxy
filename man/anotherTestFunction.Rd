\name{anotherTestFunction}

\alias{anotherTestFunction}
\title{Add two matrices}

\description{
    An example function that can be made into a Galaxy tool.
    Reads matrices from two tab-delimited files, adds them, and writes
    the result to a comma-separated file and a PDF plot.
}

\usage{
anotherTestFunction(inputfile1=GalaxyInputFile(),
    inputfile2=GalaxyInputFile(),
    plotTitle=GalaxyCharacterParam(c("TitleA"="A", "TitleB"="B")),
    plotSubTitle=GalaxyCharacterParam("My subtitle"),
    outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "pdf"))
}

\arguments{
  \item{inputfile1}{
    The filename of the \code{first} tab-separated matrix.
  }
  \item{inputfile2}{
    The filename of the second tab-separated matrix.
  }
  \item{plotTitle}{
    The title of the plot to create.
  }
  \item{plotSubTitle}{
    The subtitle of the plot to create.
  }
  \item{outputfile1}{
    The filename of the comma-separated output file to generate.
  }
  \item{outputfile2}{
    The filename of the PDF plot file to create.
  }
}

\value{
  invisible(NULL)
}

\seealso{
    \code{\link{galaxy}},
    \code{\link{GalaxyConfig}}, \code{\link{GalaxyOutput}}
}

\examples{

anotherTestFunction(system.file("extdata", "a.tsv", package="RGalaxy"),
    system.file("extdata", "b.tsv", package="RGalaxy"),
    "My Plot Title", "My Plot Subtitle",
    "output.csv", "output.pdf")
    
}

