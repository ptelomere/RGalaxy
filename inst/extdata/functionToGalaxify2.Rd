\name{functionToGalaxify2}

\alias{functionToGalaxify2}
\title{Add two matrices}

\description{
    An example function that can be made into a Galaxy tool
    Reads matrices from two tab-delimited files, adds them, and writes
    the result to a comma-separated file and a PDF plot.
}

\usage{
functionToGalaxify2(inputfile1=GalaxyInputFile(),
        inputfile2=GalaxyInputFile(),
        plotTitle=GalaxyCharacterParam(),
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
}
\details{
    This trivial method illustrates some best practices to use when writing
    functions to be adapted as Galaxy tools. For example, any error conditions
    should be handled with \code{stop} with a useful/informative error
    message. The Galaxy user will see these messages if an error occurs.
    
    Functions which take datasets as input should accept as arguments
    the filenames pointing to those datasets. The Galaxy user interface
    will allow the user to chose the dataset graphically.
    
    Return values of functions are ignored. Function output should be
    written to one or more files, and the names of these files should
    be passed into the function as arguments.
    
    Functions should be documented with a manual page. \code{RGalaxy}
    will use this manual page to fill in relevant sections of the
    Galaxy XML file.
}

\examples{

functionToGalaxify2(system.file("extdata", "a.tsv", package="RGalaxy"),
    system.file("extdata", "b.tsv", package="RGalaxy"),
    "My Plot Title", "My Plot Subtitle",
    "output.csv", "output.pdf")
    
}

