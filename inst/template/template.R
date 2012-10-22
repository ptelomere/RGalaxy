#!/usr/bin/env Rscript

## begin warning handler
withCallingHandlers({

options('useFancyQuotes' = FALSE)

suppressPackageStartupMessages(library("optparse"))

option_list <- list()

@POPULATE_OPTION_LIST@

opt <- parse_args(OptionParser(option_list=option_list))

@LIBRARY@

@FUNCTION@

params <- list()
for(param in names(opt))
{
    if (!param == "help")
        params[param] <- opt[param]
}

setClass("GalaxyRemoteError", contains="character")
wrappedFunction <- function(f)
{
    tryCatch(do.call(f, params),
        error=function(e) new("GalaxyRemoteError", conditionMessage(e)))
}


@DOCALL@

## end warning handler
}, warning = function(w) {
    cat(paste("Warning:", conditionMessage(w), "\n"))
    invokeRestart("muffleWarning")
})
