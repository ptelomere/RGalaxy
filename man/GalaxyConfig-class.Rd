\name{GalaxyConfig-class}
\Rdversion{1.1}
\docType{class}
\alias{GalaxyConfig-class}
\alias{GalaxyConfig}

\title{Class \code{"GalaxyConfig"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
    Represents information about the configuration of Galaxy.
}
\usage{
GalaxyConfig(galaxyHome, toolDir, sectionName, sectionId)
}

\arguments{
    \item{galaxyHome}{
        \bold{Required.}
        The full path to your Galaxy distribution. 
    }
    \item{toolDir}{
        \bold{Required.}
        The directory where the files associated with your
        function will reside.
    }
    \item{sectionName}{
        \bold{Required}
        A friendly name for the section of Galaxy tools that will 
        contain your function. Multiple tools can be in a single
        section; section names are headings in the left-hand side
        of the Galaxy window. If multiple tools are to reside in
        the same section, they must have identical values for
        \code{sectionName} and \code{sectionId}.
    }
    \item{sectionId}{
        An internal identifier for the section of Galaxy tools
        that will contain your function. If multiple tools
        are to reside in the same section, they must have
        identical values for \code{sectionName} and 
        \code{sectionId}.
    }
}
\section{Methods}{
No methods defined with class "GalaxyConfig" in the signature.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{link{galaxy}}, \code{\link{GalaxyOutput}}
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
GalaxyConfig(galaxyHome=getwd(), toolDir="mytool", sectionName="Test Section",
    sectionId="testSectionId")

}
\keyword{classes}
