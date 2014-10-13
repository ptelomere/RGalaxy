\name{GalaxyClasses}
\alias{GalaxyCharacterParam}
\alias{GalaxyIntegerParam}
\alias{GalaxyLogicalParam}
\alias{GalaxyNumericParam}
\alias{GalaxySelectParam}
\alias{GalaxyCharacterParam-class}
\alias{GalaxyIntegerParam-class}
\alias{GalaxyLogicalParam-class}
\alias{GalaxyNumericParam-class}
\alias{GalaxySelectParam-class}
\alias{GalaxyClasses}
\alias{GalaxyParam}
\title{Galaxy Parameter Classes}
\docType{class}
\usage{

  GalaxyIntegerParam(...)

  GalaxyNumericParam(...)

  GalaxyCharacterParam(...)

  GalaxyLogicalParam(...)

  GalaxySelectParam(...)
}
\arguments{
  \item{\dots}{
    Arguments can be a single unnamed argument, corresponding to the
    name of the class, e.g., integer, numeric, character, or logical.

    The first argument can be missing, can be of length 1 or can be
    a vector of a greater length, in which case it is rendered as
    a dropdown (select) list.

    \describe{
      \item{testValues:}{
        (optional) Either an integer(1), numeric(1), character(1), or logical(1),
        depending on the class used. A value to be used in functional testing 
        (running your function with a fixed set of inputs and checking that the
        outputs match what is expected).
      }
    }


    Additional parameters are as follows:

    \describe{
      \item{label:}{
      A character(1).
      The friendly name for this parameter. By default, RGalaxy will
      use the parameter's name ("fooBar" is changed to "Foo Bar") if
      this is not supplied.
      }
      \item{min:}{(optional) An integer(1). If the parameter is
      integer or numeric, this specifies a minimum value for the 
      parameter. Galaxy will not allow lower values.}
      \item{max:}{(optional) An integer(1). If the parameter is
      integer or numeric, this specifies a maximum value for the 
      parameter. Galaxy will not allow higher values.}
      \item{force_select:}{(default: FALSE) A logical(1).
      If this parameter is a dropdown list (i.e., the first argument
      is a vector of length > 1), a TRUE value forces the user
      to select an item.
      }
      \item{display:}{(optional) A character(1).
      If parameter is a dropdown list, this can be set to "checkboxes"
      or "radio" which renders the list as a set of check boxes or
      radio buttons. Defaults to a drop-down menu select list.
      }
      \item{checked:}{(default: FALSE) A logical(1).
      Whether the rendered checkbox should be checked. Only
      applicable if the parameter is a logical of length < 2.}
      \item{size:}{(default: 60L) An integer(1) The width of
      the field, in characters. For character parameters only.}
      \item{required:}{(default: FALSE) A logical(1). 
      Whether Galaxy will require this field be filled in.
      }
      \item{requiredMsg:}{(default: "This field is required.")
      A character(1) If \code{required} is TRUE, the message
      to display to the user when the leave the field empty.
      }


    }
  }
}
\description{
  Galaxy Parameter Classes These classes encapsulate
  parameters to be passed to functions exposed in Galaxy.
}
\details{
	The arguments to these classes are based on 
	\url{http://wiki.g2.bx.psu.edu/Admin/Tools/Tool\%20Config\%20Syntax#A.3Cparam.3E_tag_set}
}

\examples{
	GalaxyIntegerParam()
	GalaxyIntegerParam(1L)
	GalaxySelectParam(c(a=1L, b=2L))

	GalaxyNumericParam(2.0, required=TRUE)

	GalaxySelectParam(c("First Choice"="one",
		"Second Choice"="two")) # a select list
	GalaxyLogicalParam(checked=TRUE)
}
