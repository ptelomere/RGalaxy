functionToGalaxify <- function(inputfile1, inputfile2, plotTitle,
    plotSubTitle="My subtitle", outputfile1, outputfile2)
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
    plot(data3)
    dev.off()
}
