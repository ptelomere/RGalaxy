\name{GalaxyParam-class}
\Rdversion{1.1}
\docType{class}
\alias{GalaxyParam-class}
\alias{GalaxyParam}

\title{Class \code{"GalaxyParam"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
    Represents information about a parameter to a function
    that is exposed in Galaxy. Should only be used if the function
    signature itself does not fully describe the way the parameter
    should be exposed in Galaxy.
}
\usage{
GalaxyParam(label, min, max, force_select, display,
    checked, size, required=FALSE,
    requiredMsg="This field is required.")
}

\arguments{
    \item{label}{
        \bold{Required}
        A friendly label for the parameter, if the name of the 
        parameter is not sufficient.
    }
    \item{min}{
        The minimum allowed value for the parameter, if
        the parameter type is numeric or integer.
    }
    \item{max}{
        The maximum allowed value for the parameter if 
        the parameter type is numeric or integer.
    }
    \item{force_select}{
        Logical. Whether to force user to select an option in the list.
        Only supplied if the parameter is a select list, that is, a vector
        of more than one element.
    }
    \item{display}{
        If the parameter is a select list, this can be either
        \code{checkboxes} or \code{radio}, and determines how to
        render this parameter in HTML. Default is a drop-down
        select list.
    }
    \item{checked}{
        Logical. If the parameter type is \code{logical}, determines whether
        check box should be checked.
    }
    \item{size}{
        If the parameter type is character, this determines
        the width (in characters) of the resulting text box.
    }
    \item{required}{
        Logical. Whether the item is required. If \code{TRUE}, the
        user will be forced by Galaxy to enter a value for this item.
    }
    \item{requiredMsg}{
        If \code{required} is \code{TRUE} and the user tries to 
        run the Galaxy tool without specifying a value for this 
        parameter, this message will be displayed.
    }
}
\section{Methods}{
No methods defined with class "GalaxyParam" in the signature.
}


\seealso{
\code{link{galaxy}}, \code{\link{GalaxyConfig}}, \code{link{GalaxyOutput}}
}
\examples{
    title <- GalaxyParam(required=TRUE, label="Override the default label")
}
\keyword{classes}
