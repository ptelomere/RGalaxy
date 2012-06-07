

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
    function(func, manpage, galaxyOutputs, name, package=NULL, is.exported=NULL,
        paramList, version, galaxyConfig)
{
    
    requiredFields <- c("func", "manpage", "galaxyOutputs", "name",
        "paramList", "galaxyConfig")
    missingFields <- character(0)
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
    
    commandText <- paste(funcName, ".R ", sep="")
    for (item in paramList)
    {
        commandText <- paste(commandText, '"$', item@name, '" ', sep="")
    }
    commandText <- paste(commandText, "2>&1", sep="")
    
    commandNode <- newXMLNode("command", newXMLTextNode(commandText),
        parent=xml)
    xmlAttrs(commandNode)["interpreter"] <- "Rscript"
    inputsNode <- newXMLNode("inputs", parent=xml)
    
    
    for (item in paramList)
    {
        paramNode <- newXMLNode("param", parent=inputsNode)
        xmlAttrs(paramNode)["name"] <- item@name
        xmlAttrs(paramNode)["type"] <- item@type
        
        if(length(item@value)==0 && nchar(formals(func)[item@name])>0)
            item@value <- unlist(formals(func)[item@name])
        
        
        xmlAttrs(paramNode)["help"] <- getHelpFromText(rd, item@name)
        ## FIXME - should 'label' be required, not optional?
        optionalFields <- c("label", "value", "min", "max",
            "force_select", "display", "checked", "size")
            
        for (field in optionalFields)
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
    }
    
    outputsNode <- newXMLNode("outputs", parent=xml)
    for (item in galaxyOutputs)
    {
        dataNode <- newXMLNode("data", parent=outputsNode)
        xmlAttrs(dataNode)["format"] <- item@format
        xmlAttrs(dataNode)["name"] <- item@file
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

    scat <- function(msg) {
        if (missing(msg))
        msg <- ""
        write(msg, file=scriptFileName, append=TRUE)
    }
    scat("#!/usr/bin/env Rscript")
    scat()
    scat("options('useFancyQuotes' = FALSE)")
    
    funcCode <- displayFunction(func, funcName)

    scat("args <- commandArgs(TRUE)")
    scat(paste("if (!length(args)==", length(paramList), ")",
      "stop('Wrong number of command-line arguments provided.')"))

    toBoolean <- function(x)
    {
        if (tolower(x) %in% c("yes", "on", "true")) return(TRUE)
        if (tolower(x) %in% c("no", "off", "false")) return(FALSE)
        return(NA)
    }
    
    itemNum = 1
    for(item in paramList)
    {
        scat(sprintf("%s <- args[%d]", item@name, itemNum))
        if (item@type == "integer")
        {
            scat(sprintf("%s <- as.integer(%s)", item@name, item@name))
        } else if (item@type == "float") {
            scat(sprintf("%s <- as.numeric(%s)", item@name, item@name))
        } else if (item@type == "boolean") {
            scat(sprintf("%s <- %s", item@name, toBoolean(item@name)))
        }
        itemNum <- itemNum + 1 
    }
    
    
    if (!is.null(package)) {
        scat(paste("library(", package, ")"))
        do.call(library, list(package))
        if ((!is.null(is.exported)) && length(is.exported)>0 && 
            is.exported==FALSE)
        {
            funcName <- sprintf("%s:::%s", package, funcName)
        }
    }
    


    
    scat()
    

    scat(funcCode)

    scat()

    fCall <- paste(funcName, "(", sep="")
    arglist <- lapply(paramList, function(x)
    {
        paste(x@name, "=", x@name, sep="")
    })
    sArgslist <- paste(arglist, collapse=", ")
    fCall <- paste(fCall, sArgslist, ")", sep="")
    scat(fCall)

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
