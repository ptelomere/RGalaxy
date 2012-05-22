

printf <- function(...) print(noquote(sprintf(...)))

editToolConfXML <-
    function(galaxy.home, section.name, section.id, tool.dir, func.name)
{
    toolConfFile <- file.path(galaxy.home, "tool_conf.xml")
    if (!file.exists(toolConfFile))
        stop("Invalid galaxy.home, no tool_conf.xml file!")
    doc <- xmlInternalTreeParse(toolConfFile)
    toolboxNode <- xpathSApply(doc, "/toolbox")
    section <- xpathSApply(doc, 
        sprintf("/toolbox/section[@name='%s']", section.name))
    if (length(section)>0)
        removeNodes(section)
    
    sectionNode <- newXMLNode("section", parent=toolboxNode)
    xmlAttrs(sectionNode)["name"] <- section.name
    xmlAttrs(sectionNode)["id"] <- section.id
    toolNode <- newXMLNode("tool", parent=sectionNode)
    xmlAttrs(toolNode)["file"] <- sprintf("%s/%s.xml", tool.dir, func.name)
    saveXML(doc, file=toolConfFile)
}


galaxify <- 
    function(func, func.name, manpage, galaxy.home, name, 
        package=NULL, is.exported=NULL,
        param.list, section.name, section.id, tool.dir, version)
{
    requiredFields <- c("func", "func.name", "galaxy.home", "name",
        "param.list", "section.name", "section.id", "tool.dir",
        "manpage")
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
        msg <- paste(missingFields, collapse=", ")
        stop(msg)
    }
    rd <- getManPage(manpage, package)
    title <- getTitle(rd)
    
    fullToolDir <- file.path(galaxy.home, "tools", tool.dir)
    dir.create(file.path(fullToolDir), recursive=TRUE, showWarnings=FALSE)
    scriptFileName <-  file.path(fullToolDir, paste(func.name, ".R", sep=""))
    createScriptFile(scriptFileName, func, func.name, param.list,
        package, is.exported)
    
    xmlFileName <- file.path(fullToolDir, paste(func.name, "xml", sep="."))
    unlink(xmlFileName)
    
    editToolConfXML(galaxy.home, section.name, section.id, tool.dir, func.name)
    
    xml <- newXMLNode("tool")
    xmlAttrs(xml)["id"]  <- func.name
    if (!is.null(package))
        version <- packageDescription(package)$Version
    xmlAttrs(xml)["name"] <- name
    xmlAttrs(xml)["version"] <- version
    descNode <- newXMLNode("description", newXMLTextNode(title),
        parent=xml)
    
    commandText <- paste(func.name, ".R ", sep="")
    for (item in param.list)
    {
        commandText <- paste(commandText, '"$', item@name, '" ', sep="")
    }
    commandText <- paste(commandText, "2>&1", sep="")
    
    commandNode <- newXMLNode("command", newXMLTextNode(commandText),
        parent=xml)
    xmlAttrs(commandNode)["interpreter"] <- "Rscript"
    inputsNode <- newXMLNode("inputs", parent=xml)
    
    outputsNode <- newXMLNode("outputs", parent=xml)
    
    for (item in param.list)
    {
        if (item@type == "output") 
        {
            dataNode <- newXMLNode("data", parent=outputsNode)
            xmlAttrs(dataNode)["format"] <- item@format
            xmlAttrs(dataNode)["name"] <- item@name
        } else {
            paramNode <- newXMLNode("param", parent=inputsNode)
            xmlAttrs(paramNode)["name"] <- item@name
            xmlAttrs(paramNode)["type"] <- item@type
            
            if(length(item@value)==0 && nchar(formals(func)[item@name])>0)
                item@value <- unlist(formals(func)[item@name])
            
            
            xmlAttrs(paramNode)["help"] <- getHelpFromText(rd, item@name)
            optionalFields <- c("label", "value", "min", "max",
                "force_select", "display", "checked", "size")
            for (field in optionalFields)
            {
                value <- slot(item, field)
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
        }
        invisible(NULL)
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

displayFunction <- function(func, func.name)
{
    funcCode <- capture.output(func) ## TODO what if func is in a package and unexported?
    funcCode <- grep("<bytecode: ", funcCode, fixed=TRUE, invert=TRUE, value=TRUE)
    funcCode <- grep("<environment: ", funcCode, fixed=TRUE, invert=TRUE, value=TRUE)
    s <- sprintf("\n%s <- %s", func.name, paste(funcCode, collapse="\n"))
    s
}

createScriptFile <- function(scriptFileName, func, func.name, param.list, package, is.exported)
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
    
    #scat("dbg <- function(msg) {cat(paste(msg,'\n',sep=''),append=T,file='~/dev/galaxy-dist/mylog')}")
    
    funcCode <- displayFunction(func, func.name)

    scat("args <- commandArgs(TRUE)")
    scat(paste("if (!length(args)==", length(param.list), ")",
      "stop('Wrong number of command-line arguments provided.')"))

    scat("print(args)")
    #scat("dbg(length(args))")
    #scat("dbg(args)")
    toBoolean <- function(x)
    {
        if (tolower(x) %in% c("yes", "on", "true")) return(TRUE)
        if (tolower(x) %in% c("no", "off", "false")) return(FALSE)
        return(NA)
    }
    
    itemNum = 1
    for(item in param.list)
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
            func.name <- sprintf("%s:::%s", package, func.name)
        }
    }
    


    
    scat()
    

    scat(funcCode)

    scat()

    fCall <- paste(func.name, "(", sep="")
    arglist <- lapply(param.list, function(x)
    {
        paste(x@name, "=", x@name, sep="")
    })
    sArgslist <- paste(arglist, collapse=", ")
    fCall <- paste(fCall, sArgslist, ")", sep="")
    scat(fCall)

}

