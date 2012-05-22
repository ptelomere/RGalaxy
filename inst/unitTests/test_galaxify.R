galaxy.home = "fake_galaxy_dir"
tool.dir <- "RGalaxy_test_tool"
func.name <- "functionToGalaxify"

dir.create(galaxy.home, recursive=TRUE, showWarnings=FALSE)
file.copy(system.file("galaxy", "tool_conf.xml", package="RGalaxy"),
    file.path(galaxy.home, "tool_conf.xml"), overwrite=FALSE)


test_validity_method <- function()
{
    ## test the GalaxyParam validity method
    checkException(GalaxyParam(), "GalaxyParam with no parameters created!")
    checkException(GalaxyParam(type="type"),
        "GalaxyParam with no name or label created!")
    checkException(GalaxyParam(name="name", type="output"),
        "If type==output, 'format' field is required.")
    checkException(GalaxyParam(name="name", type="type"),
        "GalaxyParam with no label created!")
    checkException(GalaxyParam(name="name", type="foo", format="bla"),
        "don't use format unless type is data or output")
    checkException(GalaxyParam(name="name", type="foo", label="label", size=12),
        "only use size if type is text")
    checkException(GalaxyParam(name="n", type="t", label="l", max=21),
        "only use min or max if type is integer or float")
    checkException(GalaxyParam(name="n", type="t", label="l", min=21),
        "only use min or max if type is integer or float")
    checkException(GalaxyParam(name="n", type="t", label="l", max=1, min=21),
        "only use min or max if type is integer or float")
    checkException(GalaxyParam(name="n", type="integer",
        label="l", max=1, min=21),
        "min is larger than max")
    checkException(GalaxyParam(name="n", type="t", label="l",
        force_select="true"),
        "force_select can only be used if type is select.")
    checkException(GalaxyParam(name="n", type="select", label="l",
        force_select="true enough", selectoptions=list(a="one")),
        "force_select must have value of 'true' or 'false'")
    checkException(GalaxyParam(name="n", type="t", label="l",
        display="radio"),
        "display can only be used if type is select.")
    checkException(GalaxyParam(name="n", type="select", label="l",
        display="tv", selectoptions=list(a="one")),
        "display must have value of 'checkboxes' or 'radio'")
    checkException(GalaxyParam(name="n", type="select", label="l",
        display="tv", selectoptions=list("one")),
        "all elements of selectoptions must be named")
    
}

test_galaxy_param <- function()
{
    gp <- GalaxyParam(name="name", type="select", label="label",
        selectoptions=list(a="one"))
    checkTrue(validObject(gp), "gp is not valid!")
    checkTrue(class(gp)=="GalaxyParam", "gp has wrong class!")
}

test_galaxify <- function() 
{
    params <- list(
        GalaxyParam(name="inputfile1", type="data", label="Matrix 1"),
        GalaxyParam(name="inputfile2", type="data", label="Matrix 2"),
        GalaxyParam(name="plotTitle", type="text", label="Plot Title"),
        GalaxyParam(name="plotSubTitle", type="text", label="Plot Subtitle"),
        GalaxyParam(name="outputfile1", type="output", format="csv"),
        GalaxyParam(name="outputfile2", type="output", format="pdf"))

    galaxify(functionToGalaxify, func.name,
        manpage="functionToGalaxify",
        galaxy.home=galaxy.home, name="Add", 
        package="RGalaxy",
        param.list=params, tool.dir=tool.dir,
        version=packageDescription("RGalaxy")$Version,
        section.name="Test Section",
        section.id="testSectionId")
    
    R_file <- file.path(galaxy.home, "tools", tool.dir,
        paste(func.name, "R", sep="."))
    XML_file <- file.path(galaxy.home, "tools", tool.dir, 
        paste(func.name, "xml", sep="."))
    
    checkTrue(file.exists(R_file),
        paste("R script", R_file, "does not exist!"))
    checkTrue(file.exists(XML_file),
        paste("XML file", XML_file, "does not exist!"))
        
    doc <- xmlInternalTreeParse(XML_file)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
    
}

test_galaxify_on_function_not_in_package <- function() 
{
    params <- list(
        GalaxyParam(name="inputfile1", type="data", label="Matrix 1"),
        GalaxyParam(name="inputfile2", type="data", label="Matrix 2"),
        GalaxyParam(name="plotTitle", type="text", label="Plot Title"),
        GalaxyParam(name="plotSubTitle", type="text", label="Plot Subtitle"),
        GalaxyParam(name="outputfile1", type="output", format="csv"),
        GalaxyParam(name="outputfile2", type="output", format="pdf"))

    source(system.file("extdata", "functionToGalaxify2.R", package="RGalaxy"))
    manpage <- system.file("extdata", "functionToGalaxify2.Rd", package="RGalaxy")
    galaxify(functionToGalaxify, func.name,
        manpage=manpage,
        galaxy.home=galaxy.home, name="Add", 
        param.list=params, tool.dir=tool.dir,
        version=packageDescription("RGalaxy")$Version,
        section.name="Test Section",
        section.id="testSectionId")
    
    R_file <- file.path(galaxy.home, "tools", tool.dir,
        paste(func.name, "R", sep="."))
    XML_file <- file.path(galaxy.home, "tools", tool.dir, 
        paste(func.name, "xml", sep="."))
    
    checkTrue(file.exists(R_file),
        paste("R script", R_file, "does not exist!"))
    checkTrue(file.exists(XML_file),
        paste("XML file", XML_file, "does not exist!"))
        
    doc <- xmlInternalTreeParse(XML_file)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
    
}



test_missing_parameters <- function()
{
    checkException(galaxify(), "Can't call galaxify() with no arguments")
    ## todo add more...
}

test_galaxify_with_select <- function() 
{
    selectoptions <- list("TitleA"="A", "TitleB"="B")
    params <- list(
        GalaxyParam(name="inputfile1", type="data", label="Matrix 1"),
        GalaxyParam(name="inputfile2", type="data", label="Matrix 2"),
        GalaxyParam(name="plotTitle", type="select", label="Plot Title",
            selectoptions=selectoptions),
        GalaxyParam(name="plotSubTitle", type="text", label="Plot Subtitle"),
        GalaxyParam(name="outputfile1", type="output", format="csv"),
        GalaxyParam(name="outputfile2", type="output", format="pdf"))

    galaxify(functionToGalaxify, func.name,
        manpage="functionToGalaxify",
        galaxy.home=galaxy.home, name="Add", 
        package="RGalaxy",
        param.list=params, tool.dir=tool.dir,
        version=packageDescription("RGalaxy")$Version,
        section.name="Test Section",
        section.id="testSectionId")
    
    destDir <- file.path(galaxy.home, "tools", tool.dir)
    
    R_file <- file.path(destDir,
        paste(func.name, "R", sep="."))
    XML_file <- file.path(destDir, 
        paste(func.name, "xml", sep="."))
    
    checkTrue(file.exists(R_file),
        paste("R script", R_file, "does not exist!"))
    checkTrue(file.exists(XML_file),
        paste("XML file", XML_file, "does not exist!"))
        
    doc <- xmlInternalTreeParse(XML_file)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
    optionNodes <-
        xpathApply(doc, "/tool/inputs/param[@name='plotTitle']/option")
    checkTrue(length(optionNodes)==length(selectoptions),
        "wrong number of option nodes!")
    optionAttrs <-
        xpathApply(doc, "/tool/inputs/param[@name='plotTitle']/option",
            xmlAttrs)
    checkEquals(xpathApply(doc,
        "/tool/inputs/param[@name='plotSubTitle']", xmlAttrs)[[1]]["value"],
        "My subtitle", "value attribute does not have argument default value",
        checkNames=FALSE)
    checkTrue(!any(is.null(unlist(optionAttrs))), 
        "missing value attribute on option node(s)")
    
    ## fixme, why is there a trailing space here?
    checkEquals(sub("\\s+$", "", capture.output(xpathApply(doc,
        "/tool/description/text()")[[1]])),
        "Add two matrices",
        "description (title in manpage) is wrong")
    R_exe <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
    d <- tempdir()
    tsv1 <- system.file("extdata", "a.tsv", package="RGalaxy")
    tsv2 <- system.file("extdata", "b.tsv", package="RGalaxy")
    
    outputMatrix <- file.path(d, "output.csv")
    outputPdf <- file.path(d, "output.pdf")
    args <- sprintf('-f %s "%s" "%s" "%s" "%s" "%s" "%s"',
        R_file, tsv1,
        tsv2, "My Plot Title",
        "My Plot Subtitle", outputMatrix,
        outputPdf)
    args <- sprintf('%s "%s" "%s" "%s" "%s" "%s" "%s"',
        R_file, tsv1,
        tsv2, "My Plot Title",
        "My Plot Subtitle", outputMatrix,
        outputPdf)
    
    res <- system2(R_exe, args, stdout="", stderr="")
    sprintf("res = %d", res)
    checkTrue(res == 0, "R script returned nonzero code")
    checkTrue(file.exists(outputMatrix), "output matrix was not generated")
    checkTrue(file.exists(outputPdf), "output plot was not generated")
    m1 <- as.matrix(read.delim(tsv1, row.names=1))
    m2 <- as.matrix(read.delim(tsv2, row.names=1))
    m3 <- as.matrix(read.csv(outputMatrix, row.names=1))
    checkEquals(m3, m1 + m2, "output matrix has incorrect values")
    
}
