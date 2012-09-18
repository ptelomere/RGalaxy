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

do.call(@FULLFUNCNAME@, params)

## end warning handler
}, warning = function(w) {
    cat(paste("Warning:", conditionMessage(w), "\n"))
    invokeRestart("muffleWarning")
})
