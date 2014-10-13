RtoGalaxyTypeMap <- list("character"="text", "integer"="integer",
    "numeric"="float", "logical"="boolean",
    "GalaxyIntegerParam"="integer", "GalaxyNumericParam"="float",
    "GalaxyCharacterParam"="text", "GalaxyLogicalParam"="boolean")


printf <- function(...) print(noquote(sprintf(...)))

editToolConfXML <-
    function(galaxyHome, sectionName, sectionId, toolDir, funcName)
{
    toolConfFile <- file.path(galaxyHome, "tool_conf.xml")
    if (!file.exists(toolConfFile))
        gstop("Invalid galaxyHome, no tool_conf.xml file!")
    doc <- xmlInternalTreeParse(toolConfFile)
    toolboxNode <- xpathSApply(doc, "/toolbox")
    section <- xpathSApply(doc, 
        sprintf("/toolbox/section[@name='%s']", sectionName))
    if (length(section) == 0)
    {
        sectionNode <- newXMLNode("section", parent=toolboxNode)
    } else {
        sectionNode <- section[[1]]
        toolNodes <- xmlChildren(sectionNode)
        expectedName = sprintf("%s/%s.xml", toolDir, funcName)
        nodeToRemove <- NULL
        if (length(toolNodes) > 0)
        {
            for (i in 1:length(toolNodes))
            {
                node = toolNodes[[i]]
                if ((!is.null(xmlAttrs(node))) && xmlAttrs(node)['file'] == expectedName) {
                    nodeToRemove <- node
                    break
                }
            }
            if (!is.null(nodeToRemove)) removeNodes(nodeToRemove)
        }
    }
    
    xmlAttrs(sectionNode)["name"] <- sectionName
    xmlAttrs(sectionNode)["id"] <- sectionId
    toolNode <- newXMLNode("tool", parent=sectionNode)
    xmlAttrs(toolNode)["file"] <- sprintf("%s/%s.xml", toolDir, funcName)
    saveXML(doc, file=toolConfFile)
}


isTestable <- function(funcInfo, funcName, package, functionalTestDirectory) 
{   
    testables <- logical(0)
    for (info in funcInfo)
    {
        testable <- FALSE
        if (!is.null(info$testValues) && length(info$testValues) > 0)
            testable <- TRUE
        if (length(info['type']) > 0 &&
            info['type'] %in% c("GalaxyInputFile", "GalaxyOutput"))
        {
            if (missing(package) || is.null(package))
            {
                if (missing(functionalTestDirectory))
                {
                    testable <- FALSE
                } else {
                    testFile <- file.path(functionalTestDirectory,
                        funcName, info$param)
                    testable <- file.exists(testFile)
                }
            } else {
                testFile <- system.file("functionalTests", funcName,
                    info$param, package=package)
                testable <- file.exists(testFile)
            }
        }
        testables <- append(testables, testable)
    }
    all(testables)
}


getFuncName <- function(func)
{
    if (is.function(func))
        return(deparse(substitute(func)))
    else if (is.character(func))
        return(func)
}

## todo break into smaller functions
## todo - handle it at the R level if a required parameter is missing.
##     This should not happen when functions are called from Galaxy,
##     but will arise when functions are called from R.
galaxy <- 
    function(func, 
        package=getPackage(func),
        manpage=func, 
        name=getFriendlyName(func),
        version=getVersion(func),
        galaxyConfig,
        dirToRoxygenize,
        RserveConnection=NULL,
        path.to.R="",
        functionalTestDirectory)
{
#    force(manpage)
    if (is.function(func))
    {
        funcName <- deparse(substitute(func))
        functionToGalaxify <- func        
    }
    else if (is.character(func))
    {
        funcName <- func
        functionToGalaxify <- match.fun(func)
    }

    requiredFields <- c("func", "galaxyConfig")
    missingFields <- character(0)
    
#    if (!missing(dirToRoxygenize)) 
#        roxygenize(dirToRoxygenize, roclets=("rd"))

    for (requiredField in requiredFields)
    {
        is.missing <- do.call(missing, list(requiredField))
        if (is.missing)
        {
            missingFields <- c(missingFields, requiredField)
        }
    }
    if (length(missingFields)>0)
    {
        msg <- "The following missing fields are required: \n"
        msg <- c(msg, paste(missingFields, collapse=", "))
        gstop(msg)
    }

    

    rd <- getManPage(manpage, package)
    title <- getTitle(rd)

    fullToolDir <- file.path(galaxyConfig@galaxyHome, "tools",
        galaxyConfig@toolDir)
    dir.create(file.path(fullToolDir), recursive=TRUE, showWarnings=FALSE)
    scriptFileName <-  file.path(fullToolDir, paste(funcName, ".R", sep=""))
    funcInfo <- list()

    if  (  length(names(formals(functionToGalaxify)))   > length(formals(functionToGalaxify)) )
        gstop("All arguments to Galaxy must be named.")

    for (param in names(formals(functionToGalaxify)))
        funcInfo[[param]] <- getFuncInfo(functionToGalaxify, param)
    

    if (!isTestable(funcInfo, funcName, package, functionalTestDirectory)) 
        gwarning("Not enough information to create a functional test.")
        
    if (!suppressWarnings(any(lapply(funcInfo,
        function(x)x$type=="GalaxyOutput"))))
    {
        gstop(paste("You must supply at least one GalaxyOutput",
            "object."))
    }
    



    createScriptFile(scriptFileName, functionToGalaxify, funcName, funcInfo,
        package, RserveConnection)
    
    xmlFileName <- file.path(fullToolDir, paste(funcName, "xml", sep="."))
    unlink(xmlFileName)
    
    editToolConfXML(galaxyConfig@galaxyHome, galaxyConfig@sectionName,
        galaxyConfig@sectionId, galaxyConfig@toolDir, funcName)
    
    xml <- newXMLNode("tool")
    xmlAttrs(xml)["id"]  <- funcName
    if (!is.null(package))
        version <- packageDescription(package)$Version
    xmlAttrs(xml)["name"] <- name
    xmlAttrs(xml)["version"] <- version

    descNode <- newXMLNode("description", newXMLTextNode(title),
        parent=xml)
    
    commandText <- paste(funcName, ".R\n", sep="")
    
    for (nm in names(funcInfo))
    {
        commandText <- paste(commandText, "       ",
            sprintf("#if str($%s).strip() != \"\":\n", nm),
            "          ", sprintf("--%s=\"$%s\"", nm, nm),
            "\n       #end if\n",
            sep="")
        
    }
    commandText <- paste(commandText, "2>&1", sep="\n")
    
    commandNode <- newXMLNode("command", newXMLTextNode(commandText),
        parent=xml)
    rpath <- "Rscript"
    if (path.to.R != "")
        rpath <- file.path(path.expand(path.to.R), "bin", "Rscript")

    xmlAttrs(commandNode)["interpreter"] <- sprintf("%s --vanilla", rpath)
    inputsNode <- newXMLNode("inputs", parent=xml)
    outputsNode <- newXMLNode("outputs", parent=xml)
    if (isTestable(funcInfo, funcName, package))
        testsNode <- newXMLNode("tests", parent=xml)
    
    for (nm in names(funcInfo))
    {
        item <- funcInfo[nm][[1]]
        galaxyItem <- eval(formals(functionToGalaxify)[nm][[1]])
        if (!item$type == "GalaxyOutput")
        {
            paramNode <- newXMLNode("param", parent=inputsNode)
            if (galaxyItem@required)
            {
                validatorNode <- newXMLNode("validator", parent=paramNode)
                xmlAttrs(validatorNode)["type"] <- "empty_field"
                xmlAttrs(validatorNode)["message"] <- galaxyItem@requiredMsg
                xmlAttrs(paramNode)['optional'] <- 'false'
            } else {
                validatorNode <- newXMLNode("validator", parent=paramNode)
                xmlAttrs(validatorNode)["type"] <- "empty_field"
                ##dummyParam <- GalaxyParam()
                xmlAttrs(validatorNode)["message"] <-
                    galaxyItem@requiredMsg
                xmlAttrs(paramNode)['optional'] <- 'true'
            }
            if (item$type == "GalaxyInputFile")
            {
                xmlAttrs(paramNode)["optional"] <-
                    tolower(
                        as.character(!eval(formals(functionToGalaxify)[[nm]])@required))
            }
            
            xmlAttrs(paramNode)["name"] <- nm
            type <- RtoGalaxyTypeMap[[item$type]]
            if (item$type == "GalaxyInputFile")
            {
                type <- "data"
                if (length(galaxyItem@formatFilter))
                    xmlAttrs(paramNode)["format"] <- galaxyItem@formatFilter

            }
            if (item$length > 1 || "GalaxySelectParam" %in% class(galaxyItem))
            {
                type <- "select"
            }
            xmlAttrs(paramNode)["type"] <- type

            if(!is.null(item$default))
                xmlAttrs(paramNode)["value"] <- eval(item$default)
            else
                if (type %in% c("integer", "float"))
                    xmlAttrs(paramNode)["value"] <- ""

            xmlAttrs(paramNode)["help"] <- getHelpFromText(rd, nm)
            
            if (length(galaxyItem@label)) ## this really should always be true!
                item$label <- galaxyItem@label

            if ( galaxyItem@required){
                item$label <- paste("[required]", item$label)
            }
            

            if (type == "boolean")
            {
                if (length(galaxyItem@checked))
                    xmlAttrs(paramNode)['checked'] <-
                        tolower(as.character(galaxyItem@checked))
            }

            if (type == "text") {
                galaxyItem
                if (length(galaxyItem@size))
                xmlAttrs(paramNode)['size'] = as.character(galaxyItem@size)
            }

            if(type %in% c("integer", "float"))
            {
                if(length(galaxyItem@min))
                    xmlAttrs(paramNode)['min'] <- as.character(galaxyItem@min)
                if(length(galaxyItem@max))
                    xmlAttrs(paramNode)['max'] <- as.character(galaxyItem@max)
            }

            
            xmlAttrs(paramNode)['label'] <- item$label
            
            
            if (type=="select")
            {
                xmlAttrs(paramNode)['force_select'] <-
                    as.character(galaxyItem@force_select)
                if (length(galaxyItem@display))
                    xmlAttrs(paramNode)['display'] <-
                        as.character(galaxyItem@display)


                if (!is.null(item$selectoptions))
                {
                    selectoptions <- eval(item$selectoptions)
                    idx <- 1
                    for (value in selectoptions)
                    {
                        option <- names(selectoptions)[[idx]]
                        if (is.null(option)) option <- value
                        optionNode <- newXMLNode("option", option,
                            parent=paramNode)
                        xmlAttrs(optionNode)['value'] <- value
                        idx <- idx + 1
                    }
                    
                }

            }
            invisible(NULL)
            
        } else
        {
            dataNode <- newXMLNode("data", parent=outputsNode)
            if (is.null(item$default))
                gstop(sprintf("GalaxyOutput '%s' must have a parameter.", nm))
            galaxyOutput <- eval(item$default)
            xmlAttrs(dataNode)["format"] <- galaxyOutput@format
            xmlAttrs(dataNode)["name"] <- nm
            xmlAttrs(dataNode)["label"] <- as.character(galaxyOutput)
            
        }
    }
    
    if (isTestable(funcInfo, funcName, package))
    {
        testDataDir <- file.path(galaxyConfig@galaxyHome, "test-data", funcName)
        if (!file.exists(testDataDir))
            dir.create(testDataDir)
        #testFileDest <- file.path(funcName)
        testNode <- newXMLNode("test", parent=testsNode)
        for (info in funcInfo)
        {
            testParamNode <- newXMLNode("param", parent=testNode)
            xmlAttrs(testParamNode)["name"] <- info$param
            if (length(info$type) > 0 && 
                info$type %in% c("GalaxyInputFile", "GalaxyOutput"))
            {
                srcFile <- system.file("functionalTests", funcName, info$param,
                    package=package)
                if (!file.exists(file.path(testDataDir, info$param)))
                    file.copy(srcFile, testDataDir)
                xmlAttrs(testParamNode)["file"] <-
                    sprintf("%s/%s", funcName, info$param)
            }
            if (!is.null(info$testValues) && length(info$testValues) > 0)
            {
                ## for now, just assume one value
                xmlAttrs(testParamNode)['value'] <- info$testValues
            }
        }
    }
    
    helpText <- ""
    helpText <- generateHelpText(rd)
    
    helpNode <- newXMLNode("help", newXMLTextNode(helpText), parent=xml)
    saveXML(xml, file=xmlFileName)
}


generateHelpText <- function(rd)
{
    ret <- character(0)
    for (section in c("Description", "Details"))
    {
        txt <- parseSectionFromText(rd, section, FALSE)
        if (nchar(txt))
            ret <- c(ret, "", sprintf("**%s**", section), "", txt)
    }
    paste(ret, collapse="\n")
}

displayFunction <- function(func, funcName)
{
    funcCode <- capture.output(func) ## TODO what if func is in a package and unexported?
    funcCode <- grep("<bytecode: ", funcCode, fixed=TRUE, invert=TRUE, value=TRUE)
    funcCode <- grep("<environment: ", funcCode, fixed=TRUE, invert=TRUE, value=TRUE)
    s <- sprintf("\n%s <- %s", funcName, paste(funcCode, collapse="\n"))
    s
}

createScriptFile <- function(scriptFileName, func, funcName, funcInfo,
    package, RserveConnection)
{
    unlink(scriptFileName)

    repList <- list()
    
    funcCode <- displayFunction(func, funcName)

    repList$FUNCTION <- funcCode
    repList$FUNCNAME <- funcName
    
    repVal <- ""
    
    
    for (name in names(funcInfo))
    {
        item <- funcInfo[name][[1]]
        if (item$length > 1)
            type <- "character"
        else
            type <- item$type
        if (type %in% c("GalaxyOutput", "GalaxyInputFile")) type <- "character"
        # TODO - more idiomatically
        if (type == "GalaxyCharacterParam") type <- "character"
        if (type == "GalaxyIntegerParam") type <- "integer"
        if (type == "GalaxyNumericParam") type <- "numeric"
        if (type == "GalaxyLogicalParam") type <- "logical"
        if (type == "GalaxySelectParam")
        {
            tmp <- item$selectoptions
            gsp <- eval(tmp)
            type <- class(gsp[1])
        }
        repVal <- paste(repVal, "option_list$",
            sprintf("%s <- make_option('--%s', type='%s')\n",
            name, name, type),
            sep="")
    }
    
    repList$POPULATE_OPTION_LIST <- repVal
    
    if (!is.null(package)) {
        repList$FUNCTION <- "## function body not needed here, it is in package"
        repList$LIBRARY <- paste("suppressPackageStartupMessages(library(", package, "))", sep="")
        do.call(library, list(package))
        repList$FULLFUNCNAME <- funcName
    } else {
        repList$LIBRARY <- ""
        repList$FULLFUNCNAME <- funcName
    }
    if (!is.null(RserveConnection))
    {
        if (!require(RSclient))
            gstop(paste("The RSclient package is required. Install it with:\n",
                "biocLite('RSclient', siteRepos='http://www.rforge.net')"))
        repList$LIBRARY <- "suppressPackageStartupMessages(library(RSclient))"
        repList$DOCALL <- 
            paste(sprintf("c <- RS.connect(host='%s', port=%s)",
                RserveConnection@host,
                RserveConnection@port),
            "RS.eval(c, options('useFancyQuotes' = FALSE))",
            "RS.eval(c, suppressPackageStartupMessages(library(RGalaxy)))",
            "RS.assign(c, 'params', params)",
            "RS.assign(c, 'wrappedFunction', wrappedFunction)",
            "RS.eval(c, setClass('GalaxyRemoteError', contains='character'))",
            sprintf("res <- RS.eval(c, wrappedFunction(%s))",
                repList$FULLFUNCNAME),
            "RS.close(c)",
            "if(is(res, 'GalaxyRemoteError'))RGalaxy::gstop(res)",
            sep="\n")

    } else {
        repList$DOCALL <- sprintf(
            paste("suppressPackageStartupMessages(library(RGalaxy))",
            "do.call(%s, params)", sep="\n"),
            repList$FULLFUNCNAME)
    }
    
    copySubstitute(system.file("template", "template.R", package="RGalaxy"),
        scriptFileName, repList)
}

getSupportedExtensions <- function(galaxyHome=".")
{
    confFile <- file.path(galaxyHome, "datatypes_conf.xml")
    if (!file.exists(confFile))
    {
        confFile <- system.file("galaxy", "datatypes_conf.xml", package="RGalaxy")
        if (!file.exists(confFile)) gstop("datatypes_conf not found!")
    }
    doc <- xmlInternalTreeParse(confFile)
    extNodes <- xpathSApply(doc, "/datatypes/registration/datatype")
    tmp <- lapply(extNodes, xmlAttrs)
    unlist(lapply(tmp, "[[", "extension"))
}

checkInputs <- function(a, b=1, c)
{
    m <- match.call()
    args <- sapply(names(m)[-1], function(nm) m[[nm]])

    f <- formals()
    isSymbol <- sapply(f, is.symbol)
    f[isSymbol] <- "missing"
    f[names(args)] <- args
    f
}

## TODO: fix so "numOTUs" returns "Num OTUs" instead of "Num O T Us"
getFriendlyName <- function(camelName)
{
    if (is.function(camelName))
        input <- deparse(substitute(camelName))
    else
        input <- camelName
    chars <- strsplit(input, split="")
    ret <- ""
    i <- 1
    for (char in chars[[1]])
    {
        if(char %in% LETTERS && i > 1) ret <- c(ret, " ")
        if(i == 1) char <- toupper(char)
        ret <- c(ret, char)
        i <- i + 1
    }
    paste(ret, collapse="", sep="")
}

getFuncInfo <- function(func, param)
{
    ret <- list()
    ret$param <- param
    ret$selectoptions <- NULL
    f <- formals(func)[[param]]
    cl <- NULL
    tryCatch(cl <- class(eval(f)), error=function(x){})
    if (is.null(cl))
        gstop(sprintf("No type specified for parameter '%s'.", param))
    ret$type <- class(eval(f))
    if (!extends(ret$type, "Galaxy"))
        gstop("'%s' must be a Galaxy class.", ret$type)
    if (ret$type == "list") 
    {
        msg <- sprintf("'list' is an invalid type for parameter '%s'.\n",
            param)
        msg <- c(msg,
            "Use a subclass of GalaxyParam")
        gstop(msg)
    }
    ret$length <- length(eval(f))
    if (ret$length == 1)
        ret$default <- f
    else if (ret$length == 0)
        ret$default <- NULL ## ??
    else
        ret$selectoptions <- f

    if ("GalaxySelectParam" %in% class(cl) || "GalaxySelectParam" == cl)
        ret$selectoptions <- f
    ret$label <- getFriendlyName(param)
    if (extends(cl, "GalaxyNonFileParam"))
        ret$testValues <- eval(f)@testValues
    else
        ret$testValues <- NULL
    return(ret)
}

##' Run the functional test associated with a function.
##' 
##' FIXME
##' @return Whether the test passes.
##' @param func A function to be exposed in Galaxy.
##' @param functionalTestDirectory Where to find functional tests, if func is not
##' in a package.
runFunctionalTest <- function(func, functionalTestDirectory)
{
    funcName <- deparse(substitute(func))
    if (missing(functionalTestDirectory))
    {
        package <- getPackage(func)
        fixtureDir <- system.file("functionalTests",
            funcName, package=package)

    } else {
        package <- NULL
        fixtureDir <- file.path(functionalTestDirectory, funcName)
    }
    funcInfo <- list()
    for (param in names(formals(func)))
        funcInfo[[param]] <- getFuncInfo(func, param)

    if (missing(functionalTestDirectory) && is.null(package))
        gstop("Function must be in a package.")
    if (!isTestable(funcInfo, funcName, package, functionalTestDirectory)) 
        gstop("Not enough information to run functional test.")
    params <- list()
    outfiles <- list()
    fixtures <- list()
    for (info in funcInfo)
    {
        if (info$type == "GalaxyOutput")
        {
            outfiles[info$param] <- tempfile()
            params[info$param] <- outfiles[info$param]
        } else if (info$type == "GalaxyInputFile") {
            params[info$param] <- system.file("functionalTests",
                funcName, info$param, package=package)
        } else {
            params[info$param] <- info$testValues
        }
    }
    res <- do.call(func, params)
    diffOK <- logical(0)
    for (outfilename in names(outfiles))
    {

        generated <- unlist(outfiles[outfilename])
        fixture <- file.path(fixtureDir, funcInfo[[outfilename]]$param)
        diff <- tools:::md5sum(generated) == tools:::md5sum(fixture)
        diffOK <- c(diffOK, diff)
        if(!diff)
            gwarning("Generated '%s' differs from fixture", unname(outfilename))
    }
    all(diffOK)
}
