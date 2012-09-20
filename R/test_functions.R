functionToGalaxify <- function(inputfile1=GalaxyInputFile(),
    inputfile2=GalaxyInputFile(), plotTitle=character(),
    plotSubTitle="My subtitle", outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "pdf"))
{
    ## Make sure the file can be read
    data1 <- tryCatch({
        as.matrix(read.delim(inputfile1, row.names=1))
    }, error=function(err) {
        stop("failed to read first data file: ", conditionMessage(err))
    })
    
    data2 <- tryCatch({
        as.matrix(read.delim(inputfile2, row.names=1))
    }, error=function(err) {
        stop("failed to read second data file: ", conditionMessage(err))
    })
    
    data3 <- data1 + data2
    
    write.csv(data3, file=outputfile1)
    
    pdf(outputfile2)
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
    inputfile2=GalaxyInputFile(), plotTitle=c("TitleA"="A", "TitleB"="B"),
    plotSubTitle="My subtitle", outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "pdf"))
{
    functionToGalaxify(inputfile1, inputfile2, plotTitle,
        plotSubTitle, outputfile1, outputfile2)
}

anotherTestFunction <- function(inputfile1=GalaxyInputFile(),
    inputfile2=GalaxyInputFile(), plotTitle=c("TitleA"="A", "TitleB"="B"),
    plotSubTitle="My subtitle",
    outputfile1=GalaxyOutput("mydata", "csv"),
    outputfile2=GalaxyOutput("myplot", "pdf"))
{
    ## Make sure the file can be read
    data1 <- tryCatch({
        as.matrix(read.delim(inputfile1, row.names=1))
    }, error=function(err) {
        stop("failed to read first data file: ", conditionMessage(err))
    })
    
    data2 <- tryCatch({
        as.matrix(read.delim(inputfile2, row.names=1))
    }, error=function(err) {
        stop("failed to read second data file: ", conditionMessage(err))
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
    x = numeric(), y=TRUE, z=c("Seattle", "Tacoma", "Olympia"),
    output=GalaxyOutput("pdf")) 
{
    pdf(output)
    plot(cars)
    dev.off()
}
    