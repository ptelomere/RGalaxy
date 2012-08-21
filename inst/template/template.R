#!/usr/bin/env Rscript

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
