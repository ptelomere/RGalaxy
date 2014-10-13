#!/usr/bin/env Rscript

## begin warning handler
withCallingHandlers({

options('useFancyQuotes' = FALSE)

suppressPackageStartupMessages(library("optparse"))

option_list <- list()

option_list$inputfile1 <- make_option('--inputfile1', type='character')
option_list$inputfile2 <- make_option('--inputfile2', type='character')
option_list$plotTitle <- make_option('--plotTitle', type='character')
option_list$plotSubTitle <- make_option('--plotSubTitle', type='character')
option_list$outputfile1 <- make_option('--outputfile1', type='character')
option_list$outputfile2 <- make_option('--outputfile2', type='character')


opt <- parse_args(OptionParser(option_list=option_list))

suppressPackageStartupMessages(library(RGalaxy))

## function body not needed here, it is in package

params <- list()
for(param in names(opt))
{
    if (!param == "help")
        params[param] <- opt[param]
}

do.call(functionToGalaxify, params)

## end warning handler
}, warning = function(w) {
    cat(paste("Warning:", conditionMessage(w), "\n"))
    invokeRestart("muffleWarning")
})
