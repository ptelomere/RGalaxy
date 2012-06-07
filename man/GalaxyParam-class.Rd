\name{GalaxyParam-class}
\Rdversion{1.1}
\docType{class}
\alias{GalaxyParam-class}
\alias{GalaxyParam}

\title{Class \code{"GalaxyParam"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
    Represents information about a parameter to a function
    that is exposed in Galaxy.
}
\usage{
GalaxyParam(name, type, label, value, min, max, format, force_select, display,
    checked, size, selectoptions)
}

\arguments{
    \item{name}{
        \bold{Required.}
        The name of the parameter.
    }
    \item{type}{
        \bold{Required.}
        The type of the parameter. One of:
        text, integer, float, boolean, select, file, data
    }
    \item{label}{
        \bold{Required}
        A friendly label for the parameter.
    }
    \item{value}{
        The default value for the parameter. If the function
        being added to Galaxy has a default value defined in its
        signature, that wil be used automatically.
    }
    \item{min}{
        The minimum allowed value for the parameter, if
        \code{type} is integer or float.
    }
    \item{max}{
        The maximum allowed value for the parameter if 
        \code{type} is integer or float.
    }
    \item{format}{
        Only supplied if \code{type} is \code{data}. Must be one
        of the supported data types listed in 
        \code{GALAXY_HOME/datatypes_conf.xml} file.
    }
    \item{force_select}{
        Logical. Whether to force user to select an option in the list.
        Only supplied if \code{type} is \code{select}.
    }
    \item{display}{
        If \code{type} is \code{select}, this can be either
        \code{checkboxes} or \code{radio}, and determines how to
        render this parameter in HTML. Default is a drop-down
        select list.
    }
    \item{checked}{
        Logical. If \code{type} is \code{boolean}, determines whether
        check box should be checked.
    }
    \item{size}{
        If \code{type} is \code{text}, this determines
        the width (in characters) of the resulting text box.
    }
    \item{selectoptions}{
        If \code{type} is \code{select}, this is a list of
        names and values to populate a drop-down list or set
        of checkboxes. All items in the list must be named.
        The names are the friendly names as displayed in 
        the dropdown list; the values are what get passed
        to the R function.
    }
}
\section{Methods}{
No methods defined with class "GalaxyParam" in the signature.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{link{galaxy}}, \code{\link{GalaxyConfig}}
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
params <- list(
    GalaxyParam(name="inputfile1", type="data", label="Matrix 1"),
    GalaxyParam(name="inputfile2", type="data", label="Matrix 2"),
    GalaxyParam(name="plotTitle", type="text", label="Plot Title"),
    GalaxyParam(name="plotSubTitle", type="text", label="Plot Subtitle"))


}
\keyword{classes}
