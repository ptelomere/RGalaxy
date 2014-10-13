
.msg <-
    function(fmt, ..., width=getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    txt <- strwrap(sprintf(fmt, ...), width=width, exdent=2)
    paste(txt, collapse="\n")
}

##' Sends informational, warning, and error messages
##' to the user.
##'
##' Send error, warning, and informational
##' messages to the user. Use these instead of
##' \code{\link[base]{message}}, \code{\link[base]{warning}},
##' and \code{\link[base]{stop}}. Output is wrapped consistently
##' and passed through \code{\link[base]{sprintf}} so you
##' can use inline formatting (see examples). Output
##' of \code{gstop} will appear in Galaxy user's web browser.
##' @param ... Passed to \code{\link[base]{sprintf}}.
##' @param appendLF Passed to \code{\link[base]{message}}.
##' @return NULL
##' @examples
##' gmessage("This is an %s message.", "example")
##' @export
##' @rdname utilities
##' @seealso \code{\link[base]{message}}, \code{\link[base]{warning}},
##' \code{\link[base]{stop}}, \code{\link[base]{sprintf}}
gmessage <-
    function(..., appendLF=TRUE)
{
    message(.msg(...), appendLF=appendLF)
}

##' @param call. Passed to \code{\link[base]{stop}} or
##' \code{\link[base]{warning}}.
##' @export
##' @rdname utilities
##' @examples
##' \dontrun{
##' gstop("Encountered a %s error.", "serious")
##'}
gstop <-
    function(..., call.=FALSE)
{
    stop(.msg(...), call.=call.)
}

##' @param  immediate. Passed to \code{\link[base]{warning}}.
##' @export
##' @examples
##' \dontrun{
##'  gwarning("Something is not quite right.")    
##' }
##' @rdname utilities
gwarning <-
    function(..., call.=FALSE, immediate.=FALSE)
{
    warning(.msg(...), call.=call., immediate.=immediate.)
}

.printf <- function(...) cat(noquote(sprintf(...)), "\n")

getPackage <- function(func)
{
    f <- match.fun(func)
    env <- NULL
    tryCatch(env <- environment(f),
        error=function(x) {})
    if (is.null(env)) return(NULL)
    name <- environmentName(env)
    if (name == "R_GlobalEnv") return(NULL)
    if (name == "") return(environmentName(parent.env(env)))
    name
}

getVersion <- function(func)
{
    tryCatch(packageDescription(getPackage(func))$Version,
        error=function(x) NULL)
}

