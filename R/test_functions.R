functionToGalaxify <- function(inputfile1=GalaxyInputFile(),
    inputfile2=GalaxyInputFile(),
    plotTitle=GalaxyCharacterParam(testValues="test plot title"),
    plotSubTitle=GalaxyCharacterParam("My subtitle",
        testValues="test plot subtitle"),
    outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "png"))
{
    ## Make sure the file can be read
    data1 <- tryCatch({
        as.matrix(read.delim(inputfile1, row.names=1))
    }, error=function(err) {
        gstop("failed to read first data file: ", conditionMessage(err))
    })
    
    data2 <- tryCatch({
        as.matrix(read.delim(inputfile2, row.names=1))
    }, error=function(err) {
        gstop("failed to read second data file: ", conditionMessage(err))
    })
    
    data3 <- data1 + data2
    
    write.csv(data3, file=outputfile1)
    
    png(outputfile2)
    if (missing(plotTitle)) plotTitle <- ""
    plot(data3, main=plotTitle, sub=plotSubTitle)
    dev.off()
}

#' A variation on functionToGalaxify that takes a multiple-choice option.
#' @details There are no details.
#' @param inputfile1 the first matrix
#' @param inputfile2 the second matrix
#' @param plotTitle the plot title
#' @param plotSubTitle the plot subtitle
#' @param outputfile1 the csv output file
#' @param outputfile2 the pdf output file
testFunctionWithSelect <- function(inputfile1=GalaxyInputFile(),
    inputfile2=GalaxyInputFile(),
    plotTitle=GalaxyCharacterParam(c("TitleA"="A", "TitleB"="B"),
        force_select=TRUE),
    plotSubTitle=GalaxyCharacterParam("My subtitle"),
    outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "pdf"))
{
    functionToGalaxify(inputfile1, inputfile2, plotTitle,
        plotSubTitle, outputfile1, outputfile2)
}

#' A variation on functionToGalaxify that takes a multiple-choice option
#' using the GalaxySelectParam class.
#' @details There are no details.
#' @param inputfile1 the first matrix
#' @param inputfile2 the second matrix
#' @param plotTitle the plot title
#' @param plotSubTitle the plot subtitle
#' @param outputfile1 the csv output file
#' @param outputfile2 the pdf output file
testFunctionWithGalaxySelectParam <- function(inputfile1=GalaxyInputFile(),
    inputfile2=GalaxyInputFile(),
    plotTitle=GalaxySelectParam(c("TitleA"="A"),
        force_select=TRUE),
    plotSubTitle=GalaxyCharacterParam("My subtitle"),
    outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "pdf"))
{
    functionToGalaxify(inputfile1, inputfile2, plotTitle,
        plotSubTitle, outputfile1, outputfile2)
}



anotherTestFunction <- function(inputfile1=GalaxyInputFile(),
    inputfile2=GalaxyInputFile(),
    plotTitle=GalaxyCharacterParam(c("TitleA"="A", "TitleB"="B")),
    plotSubTitle=GalaxyCharacterParam("My subtitle"),
    outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "pdf"))
{
    ## Make sure the file can be read
    data1 <- tryCatch({
        as.matrix(read.delim(inputfile1, row.names=1))
    }, error=function(err) {
        gstop("failed to read first data file: ", conditionMessage(err))
    })
    
    data2 <- tryCatch({
        as.matrix(read.delim(inputfile2, row.names=1))
    }, error=function(err) {
        gstop("failed to read second data file: ", conditionMessage(err))
    })
    
    data3 <- data1 + data2
    
    write.csv(data3, file=outputfile1)
    
    pdf(outputfile2)
    if (missing(plotTitle)) plotTitle <- ""
    plot(data3, main=plotTitle, sub=plotSubTitle)
    dev.off()
}

#' a foo function
#'
#' @details nothing
#' @param input An input dataset
#' @param x the x param
#' @param y the y param
#' @param z the z param
#' @param output the output
foo = function(input = GalaxyInputFile(),
    x = GalaxyNumericParam(), y=TRUE,
    z=GalaxyCharacterParam(c("Seattle", "Tacoma", "Olympia")),
    output=GalaxyOutput("pdf")) 
{
    pdf(output)
    plot(cars)
    dev.off()
}

addTwoNumbers <- 
    function(
        number1=GalaxyNumericParam(required=TRUE),
        number2=GalaxyNumericParam(required=TRUE),
        sum=GalaxyOutput("sum", "txt"))
{
    cat(number1 + number2, file=sum)
}

addTwoNumbersWithTest <- 
    function(
        number1=GalaxyNumericParam(required=TRUE, testValues=5L),
        number2=GalaxyNumericParam(required=TRUE, testValues=5L),
        sum=GalaxyOutput("sum", "txt"))
{
    cat(number1 + number2, file=sum)
}

probeLookup <- function(
    probe_ids=GalaxyCharacterParam(
        required=TRUE,
        testValues="1002_f_at 1003_s_at"),
    outputfile=GalaxyOutput("probeLookup", "csv"))
{
    suppressPackageStartupMessages(library(hgu95av2.db))
    ids <- strsplit(probe_ids, " ")[[1]]
    results <- select(hgu95av2.db, keys=ids, columns=c("SYMBOL","PFAM"),
        keytype="PROBEID")
    write.csv(results, file=outputfile)
}
