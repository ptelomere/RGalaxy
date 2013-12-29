galaxyHome = "fake_galaxy_dir"
toolDir <- "RGalaxy_test_tool"
funcName <- "functionToGalaxify"

dir.create(galaxyHome, recursive=TRUE, showWarnings=FALSE)
dir.create(sprintf("%s/test-data", galaxyHome),
    recursive=TRUE, showWarnings=FALSE)
file.copy(system.file("galaxy", "tool_conf.xml", package="RGalaxy"),
    file.path(galaxyHome, "tool_conf.xml"), overwrite=TRUE)

.setUp <- function()
{
    file.copy(system.file("galaxy", "tool_conf.xml", package="RGalaxy"),
        file.path(galaxyHome, "tool_conf.xml"), overwrite=TRUE)
    
}

## These checks no longer happen in the GalaxyParam validity method.
old_test_validity_method <- function()
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
    checkException(GalaxyParam(name="name", type="data", format="gfkghfkjghfkgjhfjg"),
        "use only supported formats")
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
        force_select=TRUE),
        "force_select can only be used if type is select.")
    checkException(GalaxyParam(name="n", type="t", label="l",
        display="radio"),
        "display can only be used if type is select.")
    checkException(GalaxyParam(name="n", type="select", label="l",
        display="tv", selectoptions=list(a="one")),
        "display must have value of 'checkboxes' or 'radio'")
    checkException(GalaxyParam(name="n", type="select", label="l",
        display="tv", selectoptions=list("one")),
        "all elements of selectoptions must be named")
    checkException(GalaxyParam(name="n", type="text", label="l",
        display="tv", selectoptions=list("one")),
        "selectoptions can only be used if type is select.")
    
    ## Test the GalaxyOutput validity method
    checkException(GalaxyOutput(file="bogus.filetype"),
        "use only supported formats")
    
}

## This test no longer applies.
old_test_galaxy_param <- function()
{
    gp <- GalaxyParam(type="select", label="label",
        selectoptions=list(a="one"))
    checkTrue(validObject(gp), "gp is not valid!")
    checkTrue(class(gp)=="GalaxyParam", "gp has wrong class!")
}

test_galaxy <- function() 
{
    galaxy("functionToGalaxify",
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section", 
            "testSectionId"))
    
    R_file <- file.path(galaxyHome, "tools", toolDir,
        paste(funcName, "R", sep="."))
    XML_file <- file.path(galaxyHome, "tools", toolDir, 
        paste(funcName, "xml", sep="."))
    
    checkTrue(file.exists(R_file),
        paste("R script", R_file, "does not exist!"))
    checkTrue(file.exists(XML_file),
        paste("XML file", XML_file, "does not exist!"))
        
    doc <- xmlInternalTreeParse(XML_file)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
        
}

test_galaxy_on_function_not_in_package <- function() 
{

    base::source(system.file("extdata", "functionToGalaxify2.R", package="RGalaxy"))
    manpage <- system.file("extdata", "functionToGalaxify2.Rd", package="RGalaxy")
    galaxy("functionToGalaxify2",
        manpage=manpage,
        version=packageDescription("RGalaxy")$Version,
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section",
            "testSectionId"))
    
    R_file <- file.path(galaxyHome, "tools", toolDir,
        paste(funcName, "R", sep="."))
    XML_file <- file.path(galaxyHome, "tools", toolDir, 
        paste(funcName, "xml", sep="."))
    
    checkTrue(file.exists(R_file),
        paste("R script", R_file, "does not exist!"))
    checkTrue(file.exists(XML_file),
        paste("XML file", XML_file, "does not exist!"))
        
    doc <- xmlInternalTreeParse(XML_file)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
    
}



test_missing_parameters <- function()
{
    checkException(galaxy(), "Can't call galaxy() with no arguments")
}


test_galaxy_sanity_checks <- function()
{
#    checkException(galaxy(ls, 1, galaxyConfig=1),
#       "galaxy allows unnamed parameters")

    galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section", 
        "testSectionId")

    checkException(galaxy("ls", a=2, galaxyConfig=galaxyConfig),
        "galaxy allows 'plain' types")

#    checkException(galaxy())
#    selectoptions <- list("TitleA"="A", "TitleB"="B")
    
    ## todo add new check for this:
    
#    checkException(galaxy(functionToGalaxify,
#        manpage="functionToGalaxify",
#        package="RGalaxy",
#        version=packageDescription("RGalaxy")$Version,
#        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section",
#            "testSectionId"),
#            "galaxy() got no GalaxyParam objects but did not throw an exception")
#    )
    
# todo and this:
#    checkException(galaxy(functionToGalaxify,
#        manpage="functionToGalaxify",
#        inputfile1=GalaxyParam(type="data", label="Matrix 1"),
#        inputfile2=GalaxyParam(type="data", label="Matrix 2"),
#        plotTitle=GalaxyParam(type="select", label="Plot Title",
#            selectoptions=selectoptions, force_select=TRUE),
#        plotSubTitle=GalaxyParam(type="text", label="Plot Subtitle"),
#        name="Add", 
#        package="RGalaxy",
#        version=packageDescription("RGalaxy")$Version,
#        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section",
#            "testSectionId")),
#            "galaxy() got no GalaxyOutput objects but did not throw an exception")



# and this (applies only if user has supplied GalaxyParam objects...)

#    checkException(galaxy(galaxy(functionToGalaxify,
#        manpage="functionToGalaxify",
#        blablabla=GalaxyParam(type="data", label="Matrix 1"),
#        inputfile2=GalaxyParam(type="data", label="Matrix 2"),
#        plotTitle=GalaxyParam(type="select", label="Plot Title",
#            selectoptions=selectoptions, force_select=TRUE),
#        plotSubTitle=GalaxyParam(type="text", label="Plot Subtitle"),
#        outputfile1=GalaxyOutput("csv"),
#        outputfile2=GalaxyOutput("pdf"),
#        name="Add", 
#        package="RGalaxy",
#        version=packageDescription("RGalaxy")$Version,
#        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section",
#            "testSectionId"))),
#            "function parameters and galaxy() named parameters do not match")
    
}

test_galaxy_with_select <- function()
{
    selectoptions <- list("TitleA"="A", "TitleB"="B")
    
    funcName <- "testFunctionWithSelect"
    galaxy("testFunctionWithSelect",
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section",
            "testSectionId"))
    
    destDir <- file.path(galaxyHome, "tools", toolDir)
    
    
    
    R_file <- file.path(destDir,
        paste(funcName, "R", sep="."))
    XML_file <- file.path(destDir, 
        paste(funcName, "xml", sep="."))
    
    checkTrue(file.exists(R_file),
        paste("R script", R_file, "does not exist!"))
    checkTrue(file.exists(XML_file),
        paste("XML file", XML_file, "does not exist!"))
        
    doc <- xmlInternalTreeParse(XML_file)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
    optionNodes <-
        xpathApply(doc, "/tool/inputs/param[@name='plotTitle']/option")
    checkEquals(length(selectoptions), length(optionNodes),
        "wrong number of option nodes!")
    optionAttrs <-
        xpathApply(doc, "/tool/inputs/param[@name='plotTitle']/option",
            xmlAttrs)
    checkEquals(xpathApply(doc,
        "/tool/inputs/param[@name='plotSubTitle']", xmlAttrs)[[1]]["value"],
        "My subtitle", "value attribute does not have argument default value",
        checkNames=FALSE)
    checkEquals(xpathApply(doc,
        "/tool/inputs/param[@name='plotTitle']", 
        xmlAttrs)[[1]]["force_select"], "TRUE",
        "force_select is not TRUE", checkNames=FALSE)
    checkTrue(!any(is.null(unlist(optionAttrs))), 
        "missing value attribute on option node(s)")
    
    ## fixme, why is there a trailing space here?
    checkEquals(sub("\\s+$", "", capture.output(xpathApply(doc,
        "/tool/description/text()")[[1]])),
        "A variation on functionToGalaxify that takes a multiple-choice option.",
        "description (title in manpage) is wrong")
    R_exe <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
    d <- tempdir()
    tsv1 <- system.file("extdata", "a.tsv", package="RGalaxy")
    tsv2 <- system.file("extdata", "b.tsv", package="RGalaxy")
    
    outputMatrix <- file.path(d, "output.csv")
    outputPdf <- file.path(d, "output.pdf")
    tmpl <- paste("%s --inputfile1=%s --inputfile2=%s --plotTitle=%s",
        "--plotSubTitle=%s --outputfile1=%s --outputfile2=%s")
    args <- sprintf(tmpl, R_file, tsv1, tsv2, "My_Plot_Title",
        "My_Plot_Subtitle", outputMatrix, outputPdf)
    
    res <- system2(R_exe, args, stdout="", stderr="")
    checkTrue(res == 0, "R script returned nonzero code")
    checkTrue(file.exists(outputMatrix), "output matrix was not generated")
    checkTrue(file.exists(outputPdf), "output plot was not generated")
    m1 <- as.matrix(read.delim(tsv1, row.names=1))
    m2 <- as.matrix(read.delim(tsv2, row.names=1))
    m3 <- as.matrix(read.csv(outputMatrix, row.names=1))
    checkEquals(m3, m1 + m2, "output matrix has incorrect values")
    
}

test_required_option <- function()
{
    base::source(system.file("samplePkg", "R", "functions.R", package="RGalaxy"))
    galaxy("testRequiredOption",
        manpage=system.file("samplePkg", "man",
        "testRequiredOption.Rd", package="RGalaxy"),
        version="0.99.0",
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir,
            "Test Section", "testSectionId"))
    checkTrue(file.exists(system.file("samplePkg", "man",
        "testRequiredOption.Rd", package="RGalaxy")))
    destDir <- file.path(galaxyHome, "tools", toolDir)
    R_file <- file.path(destDir,
        paste("testRequiredOption", "R", sep="."))
    XML_file <- file.path(destDir, 
        paste("testRequiredOption", "xml", sep="."))
    checkTrue(file.exists(R_file),
        paste("R script", R_file, "does not exist!"))
    checkTrue(file.exists(XML_file),
        paste("XML file", XML_file, "does not exist!"))
        
    doc <- xmlInternalTreeParse(XML_file)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
    validatorNode <-
        xpathApply(doc, "/tool/inputs/param[@name='requiredOption']/validator")
    checkEquals("THIS FIELD IS MANDATORY",
        xmlAttrs(validatorNode[[1]])['message'], checkNames=FALSE)
    paramNode <- xpathApply(doc, "/tool/inputs/param[@name='requiredOption']")
    checkEquals("[required] Required Option",
        xmlAttrs(paramNode[[1]])['label'], checkNames=FALSE)    
}

test_missing_param <- function()
{
    base::source(system.file("samplePkg", "R", "functions.R", package="RGalaxy"))
    galaxy("testMissingParams",
        manpage=system.file("samplePkg", "man",
        "testMissingParams.Rd", package="RGalaxy"),
        version="0.99.0",
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir,
            "Test Section", "testSectionId"),
        dirToRoxygenize=system.file("samplePkg", package="RGalaxy"))
    checkTrue(file.exists(system.file("samplePkg", "man",
        "testMissingParams.Rd", package="RGalaxy")))
    destDir <- file.path(galaxyHome, "tools", toolDir)
    R_file <- file.path(destDir,
        paste("testMissingParams", "R", sep="."))
    R_exe <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
    d <- tempfile()
    args <- sprintf("%s --requiredParam required --outfile=%s", R_file, d)
    res <- system2(R_exe, args, stdout="", stderr="")
    checkEquals(0, res)
    output <- paste(readLines(d), collapse="")
    expected <- paste("requiredParam==required",
        "paramWithDefault==GalaxyIntegerParam(1)optionalParam==GalaxyCharacterParam()outfile==", d, sep="")
    checkEquals(expected, output)
}

test_checkboxes <- function()
{
    base::source(system.file("samplePkg", "R", "functions.R", package="RGalaxy"))
    galaxy("testCheckboxes",
        manpage=system.file("samplePkg", "man",
        "testCheckboxes.Rd", package="RGalaxy"),
        version="0.99.0",
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir,
            "Test Section", "testSectionId"))
    checkTrue(file.exists(system.file("samplePkg", "man",
        "testCheckboxes.Rd", package="RGalaxy")))
    destDir <- file.path(galaxyHome, "tools", toolDir)
    R_file <- file.path(destDir,
        paste("testCheckboxes", "R", sep="."))
    R_exe <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
    d <- tempfile()
    args <- sprintf("%s --checkbox1 TRUE --checkbox2 FALSE --outfile=%s",
        R_file, d)
    res <- system2(R_exe, args, stdout="", stderr="")
    checkEquals(0, res)
}

test_multiple_galaxifications_do_not_overwrite_each_other <- function()
{
    file.copy(system.file("galaxy", "tool_conf.xml", package="RGalaxy"),
        file.path(galaxyHome, "tool_conf.xml"), overwrite=TRUE)
    
    galaxy("functionToGalaxify",
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section",
            "testSectionId"))
    
    galaxy("anotherTestFunction",
        galaxyConfig=GalaxyConfig(galaxyHome, toolDir, "Test Section",
            "testSectionId"))
    toolfile <- file.path(galaxyHome, "tool_conf.xml")
    doc <- xmlInternalTreeParse(toolfile)
    checkTrue(any(class(doc)=="XMLInternalDocument"), "invalid XML file!")
    xpath <- "/toolbox/section[@name='Test Section']"
    toolNodes <- xpathSApply(doc, xpath)
    tools <- xmlChildren(toolNodes[[1]])
    
    checkEquals(2, length(xmlChildren(toolNodes[[1]])))
}


## more things to test:
##GalaxyParam label overrides default
## label gets set in the first place!