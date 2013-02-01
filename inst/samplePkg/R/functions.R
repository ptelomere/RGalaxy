#' Test whether a required parameter works as expected.
#'
#' This function tests whether a required function works as expected.
#' @param requiredOption An option that is required.
#' @param outputfile The name of a file where diagnostic output is written.
#' @details These are the details.
testRequiredOption <- 
    function(requiredOption=GalaxyCharacterParam(required=TRUE,
        requiredMsg="THIS FIELD IS MANDATORY"),
        outputfile=GalaxyOutput("mytext", "txt"))
{
    write(sprintf("requiredOption==%s", requiredOption), file=outputfile)
}



#' Test which parameters are supplied and what values they have.
#'
#' This function displays the values of its parameters, or 'missing'
#' if a parameter value is not supplied.
#' @param requiredParam A required parameter
#' @param paramWithDefault A parameter with a default value
#' @param optionalParam An optional parameter
#' @param outfile where to write diagnostic output
#' @details These are the details.
testMissingParams <- function(requiredParam=GalaxyCharacterParam(),
    paramWithDefault=GalaxyIntegerParam(1),
     optionalParam=GalaxyCharacterParam(),
     outfile=GalaxyOutput("myoutput", "txt"))
{
    m <- match.call()
    args <- sapply(names(m)[-1], function(nm) m[[nm]])

    f <- formals()
    isSymbol <- sapply(f, is.symbol)
    f[isSymbol] <- "missing"
    f[names(args)] <- args
    for (name in names(f))
    {
        write(sprintf("%s==%s\n", name, f[name]), file=outfile, append=TRUE)
    }
}

#' Test that checkboxes get the appropriate value
#'
#' This function displays the values of its parameters, or 'missing'
#' if a parameter value is not supplied.
#' @param checkbox1 A checkbox to be set to TRUE
#' @param checkbox2 A checkbox to be set to FALSE
#' @param optionalParam An optional parameter
#' @param outfile where to write diagnostic output
#' @details These are the details.
testCheckboxes <- function(checkbox1=GalaxyLogicalParam(), 
    checkbox2=GalaxyLogicalParam(),
    outfile=GalaxyOutput("myoutput", "txt"))
{
    if(!is(checkbox1, "logical")) gstop("checkbox1 is not logical!")
    if(!is(checkbox2, "logical")) gstop("checkbox2 is not logical!")
    if(!checkbox1) gstop("checkbox1 is FALSE!")
    if(checkbox2) gstop("checkbox2 is TRUE!")
    write(sprintf("checkbox1==%s\ncheckbox2==%s", checkbox1, checkbox2),
        file=outfile, append=TRUE)
}
