

printf <- function(...) print(noquote(sprintf(...)))

editToolConfXML <-
    function(galaxyHome, sectionName, sectionId, toolDir, funcName)
{
    toolConfFile <- file.path(galaxyHome, "tool_conf.xml")
    if (!file.exists(toolConfFile))
        stop("Invalid galaxyHome, no tool_conf.xml file!")
    doc <- xmlInternalTreeParse(toolConfFile)
    toolboxNode <- xpathSApply(doc, "/toolbox")
    section <- xpathSApply(doc, 
        sprintf("/toolbox/section[@name='%s']", sectionName))
    if (length(section)>0)
        removeNodes(section)
    
    sectionNode <- newXMLNode("section", parent=toolboxNode)
    xmlAttrs(sectionNode)["name"] <- sectionName
    xmlAttrs(sectionNode)["id"] <- sectionId
    toolNode <- newXMLNode("tool", parent=sectionNode)
    xmlAttrs(toolNode)["file"] <- sprintf("%s/%s.xml", toolDir, funcName)
    saveXML(doc, file=toolConfFile)
}

galaxy <- 
    function(func, manpage, ..., name, package=NULL, is.exported=NULL,
        version, galaxyConfig, packageSourceDir)
{
    
    requiredFields <- c("func", "manpage", "name",
        "galaxyConfig")
    missingFields <- character(0)
    
    if (!missing(packageSourceDir)) roxygenize(packageSourceDir)
    
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
        stop(msg)
    }

    paramList <- list(...)
    if (!length(paramList)) {
        stop("You must pass parameters to galaxy().")
    }
    
    if (!suppressWarnings(any(lapply(paramList,
        function(x)class(x)=="GalaxyParam"))))
    {
        stop(paste("You must pass one GalaxyParam object",
            "for each parameter to your function."))
    }
    
    if (!suppressWarnings(any(lapply(paramList,
        function(x)class(x)=="GalaxyOutput"))))
    {
        stop(paste("You must supply at least one GalaxyOutput",
            "object."))
    }

    
    
    if (any(which(nchar(names(paramList))==0)) || is.null(names(paramList)))
    {
        stop("All ... arguments to galaxy() must be named.")
    }

    funcName <- deparse(substitute(func))


    rd <- getManPage(manpage, package)
    title <- getTitle(rd)
    
    fullToolDir <- file.path(galaxyConfig@galaxyHome, "tools",
        galaxyConfig@toolDir)
    dir.create(file.path(fullToolDir), recursive=TRUE, showWarnings=FALSE)
    scriptFileName <-  file.path(fullToolDir, paste(funcName, ".R", sep=""))
    createScriptFile(scriptFileName, func, funcName, paramList,
        package, is.exported)
    
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
    for (name in names(paramList))
    {
        commandText <- paste(commandText, "       ",
            sprintf("#if str($%s).strip() != \"\":\n", name),
            "          ", sprintf("--%s=\"$%s\"", name, name),
            "\n       #end if\n",
            sep="")
    }
    commandText <- paste(commandText, "2>&1", sep="")
    
    commandNode <- newXMLNode("command", newXMLTextNode(commandText),
        parent=xml)
    xmlAttrs(commandNode)["interpreter"] <- "Rscript --vanilla"
    inputsNode <- newXMLNode("inputs", parent=xml)
    outputsNode <- newXMLNode("outputs", parent=xml)
    
    
    for (name in names(paramList))
    {
        item <- paramList[name][[1]]
        if (class(item) %in% "GalaxyParam")
        {
            paramNode <- newXMLNode("param", parent=inputsNode)
            if (item@required)
            {
                validatorNode <- newXMLNode("validator", parent=paramNode)
                xmlAttrs(validatorNode)["type"] <- "empty_field"
                xmlAttrs(validatorNode)["message"] <- item@requiredMsg
            }
            xmlAttrs(paramNode)["name"] <- name
            xmlAttrs(paramNode)["type"] <- item@type

            if(length(item@value)==0 && nchar(formals(func)[name])>0)
                item@value <- as.character(unlist(formals(func)[name]))


            xmlAttrs(paramNode)["help"] <- getHelpFromText(rd, name)
            attributeFields <- c("label", "value", "min", "max",
                "force_select", "display", "checked", "size")


            if (item@required) item@label <- paste("[required]", item@label)
            for (field in attributeFields)
            {
                value <- as.character(slot(item, field))
                if (length(value) > 0)
                    xmlAttrs(paramNode)[field] <- value
            }
            if (item@type=="select")
            {
                for (option in names(item@selectoptions))
                {
                    value <- item@selectoptions[option]
                    optionNode <- newXMLNode("option", option,
                        parent=paramNode)
                    xmlAttrs(optionNode)["value"] <- value
                }

            }
            invisible(NULL)
            
        } else if (class(item) %in% "GalaxyOutput")
        {
            dataNode <- newXMLNode("data", parent=outputsNode)
            xmlAttrs(dataNode)["format"] <- item@format
            xmlAttrs(dataNode)["name"] <- name
            
        }
    }
    
    
    
    ##testsNode <- newXMLNode("tests", parent=xml) ## TODO - enable these?
    ## todo, fill in test section
    
    helpText <- "" ## TODO, generate help text
    helpText <- generateHelpText(rd)
    
    helpNode <- newXMLNode("help", newXMLTextNode(helpText), parent=xml)
    saveXML(xml, file=xmlFileName)
}


generateHelpText <- function(rd)
{
    ret <- character(0)
    ret <- c(ret, "", "**Description**", "",
        parseSectionFromText(rd, "Description"))
    ret <- c(ret, "", "**Details**", "", parseSectionFromText(rd, "Details"))
    
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

createScriptFile <- function(scriptFileName, func, funcName, paramList, package, is.exported)
{
    unlink(scriptFileName)

    existingFuncParams <- sort(names(formals(funcName)))
    proposedFuncParams <- sort(names(paramList))
    if(!all(proposedFuncParams %in% existingFuncParams)) {
        stop(paste("The named arguments you passed do not match",
        "the arguments accepted by your function."))
    }


    repList <- list()
    
    funcCode <- displayFunction(func, funcName)

    repList$FUNCTION <- funcCode
    repList$FUNCNAME <- funcName
    
    galaxyToRtypeMap <- list("text"="character", "integer"="integer",
        "float"="numeric", "data"="character", "boolean"="logical",
        "select"="character", "file"="character")
    
    repVal <- ""
    for(name in names(paramList))
    {
        item <- paramList[name][[1]]
        if ("GalaxyParam" %in% class(item)) type = galaxyToRtypeMap[item@type]
        else type="character"
        
        repVal <- paste(repVal, "option_list$",
            sprintf("%s <- make_option('--%s', type='%s')\n",
            name, name, type),
            sep="")
    }
    
    repList$POPULATE_OPTION_LIST <- repVal
    
    if (!is.null(package)) {
        repList$FUNCTION <- "## function body not needed here, it is in package"
        repList$LIBRARY <- paste("library(", package, ")", sep="")
        do.call(library, list(package))
        if ((!is.null(is.exported)) && length(is.exported)>0 && 
            is.exported==FALSE)
        {
            repList$FULLFUNCNAME <- sprintf("%s:::%s", package, funcName)
        } else {
            repList$FULLFUNCNAME <- funcName
        }
        
    } else {
        repList$LIBRARY <- ""
        repList$FULLFUNCNAME <- funcName
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
        if (!file.exists(confFile)) stop("datatypes_conf not found!")
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

