\name{GalaxyInputFile-class}
\Rdversion{1.1}
\docType{class}
\alias{GalaxyInputFile-class}
\alias{GalaxyInputFile}

\title{Class \code{"GalaxyInputFile"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
    Represents a dataset hosted on Galaxy, to be passed to an
    R function that you expose in Galaxy.
}
\usage{
GalaxyInputFile(required=TRUE)
}
\arguments{
  \item{required}{Whether to require that the user provide this dataset.}
}


\section{Methods}{
No methods defined with class "GalaxyInputFile" in the signature.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
\code{link{galaxy}}, \code{\link{GalaxyConfig}}
}
\examples{
    inputfile1=GalaxyInputFile()
    
}
\keyword{classes}
