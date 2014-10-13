\name{RserveConnection-class}
\Rdversion{1.1}
\docType{class}
\alias{RserveConnection-class}
\alias{RserveConnection}

\title{Class \code{"RserveConnection"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
    Represents a connection to Rserve.
}
\usage{
RserveConnection(...)
}

\arguments{
    \item{\dots}{
    	\describe{
    		\item{host}{character(1) The name of the host.
    		Defaults to "localhost".
    		}
    		\item{port}{integer(1) The port. Defaults to 6311L.}
	    }
	 }
}
\section{Methods}{
No methods defined with class "RserveConnection" in the signature.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{link{galaxy}}, \code{\link{GalaxyOutput}}
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{

RserveConnection(host="myhost", port=2012L)
}
\keyword{classes}
