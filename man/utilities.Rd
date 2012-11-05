\name{gmessage}
\alias{gmessage}
\alias{gstop}
\alias{gwarning}
\title{Sends informational, warning, and error messages
to the user.}
\usage{
  gmessage(..., appendLF = TRUE)

  gstop(..., call. = FALSE)

  gwarning(..., call. = FALSE, immediate. = FALSE)
}
\arguments{
  \item{...}{Passed to \code{\link[base]{sprintf}}.}

  \item{appendLF}{Passed to \code{\link[base]{message}}.}

  \item{call.}{Passed to \code{\link[base]{stop}} or
  \code{\link[base]{warning}}.}

  \item{immediate.}{Passed to \code{\link[base]{warning}}.}
}
\value{
  NULL
}
\description{
  Send error, warning, and informational messages to the
  user. Use these instead of \code{\link[base]{message}},
  \code{\link[base]{warning}}, and
  \code{\link[base]{stop}}. Output is wrapped consistently
  and passed through \code{\link[base]{sprintf}} so you can
  use inline formatting (see examples). Output of
  \code{gstop} will appear in Galaxy user's web browser.
}
\examples{
gmessage("This is an \%s message.", "example")
\dontrun{
gstop("Encountered a \%s error.", "serious")
}
\dontrun{
 gwarning("Something is not quite right.")
}
}
\seealso{
  \code{\link[base]{message}}, \code{\link[base]{warning}},
  \code{\link[base]{stop}}, \code{\link[base]{sprintf}}
}

