RtoGalaxyTypeMap <- list("character"="text", "integer"="integer",
    "numeric"="float", "logical"="boolean")


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

## todo break into smaller functions
galaxy <- 
    function(func, manpage, ..., 
        name=getFriendlyName(deparse(substitute(func))),
        package=NULL, is.exported=NULL,
        version, galaxyConfig, packageSourceDir,
        useRserve=FALSE)
{
    requiredFields <- c("func", "manpage", "galaxyConfig")
    missingFields <- character(0)
    
    if (!missing(packageSourceDir)) 
        roxygenize(packageSourceDir, roclets=("rd"))
    
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

    funcName <- deparse(substitute(func))

    rd <- getManPage(manpage, package)
    title <- getTitle(rd)
    
    fullToolDir <- file.path(galaxyConfig@galaxyHome, "tools",
        galaxyConfig@toolDir)
    dir.create(file.path(fullToolDir), recursive=TRUE, showWarnings=FALSE)
    scriptFileName <-  file.path(fullToolDir, paste(funcName, ".R", sep=""))
    funcInfo <- list()
    for (param in names(formals(func)))
        funcInfo[[param]] <- getFuncInfo(func, param)
        
        
    if (!suppressWarnings(any(lapply(funcInfo,
        function(x)x$type=="GalaxyOutput"))))
    {
        stop(paste("You must supply at least one GalaxyOutput",
            "object."))
    }
    
    createScriptFile(scriptFileName, func, funcName, funcInfo, paramList,
        package, is.exported, useRserve)
    
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
    
    for (name in names(funcInfo))
    {
        commandText <- paste(commandText, "       ",
            sprintf("#if str($%s).strip() != \"\":\n", name),
            "          ", sprintf("--%s=\"$%s\"", name, name),
            "\n       #end if\n",
            sep="")
        
    }
    
    commandNode <- newXMLNode("command", newXMLTextNode(commandText),
        parent=xml)
    xmlAttrs(commandNode)["interpreter"] <- "Rscript --vanilla"
    inputsNode <- newXMLNode("inputs", parent=xml)
    outputsNode <- newXMLNode("outputs", parent=xml)
    
    
    for (name in names(funcInfo))
    {
        item <- funcInfo[name][[1]]
        param <- paramList[name][[1]]
        if (!item$type == "GalaxyOutput")
        {
            paramNode <- newXMLNode("param", parent=inputsNode)
            if ( (!is.null(param)) && param@required)
            {
                validatorNode <- newXMLNode("validator", parent=paramNode)
                xmlAttrs(validatorNode)["type"] <- "empty_field"
                xmlAttrs(validatorNode)["message"] <- param@requiredMsg
                xmlAttrs(paramNode)['optional'] <- 'false'
            } else {
                validatorNode <- newXMLNode("validator", parent=paramNode)
                xmlAttrs(validatorNode)["type"] <- "empty_field"
                dummyParam <- GalaxyParam()
                xmlAttrs(validatorNode)["message"] <-
                    dummyParam@requiredMsg
                xmlAttrs(paramNode)['optional'] <- 'false'
            }
            if (item$type == "GalaxyInputFile")
            {
                xmlAttrs(paramNode)["optional"] <-
                    tolower(
                        as.character(!eval(formals(func)[[name]])@required))
            }
            
            xmlAttrs(paramNode)["name"] <- name
            type <- RtoGalaxyTypeMap[[item$type]]
            if (item$type == "GalaxyInputFile") type <- "data"
            if (item$length > 1) type <- "select"
            xmlAttrs(paramNode)["type"] <- type

            if(!is.null(item$default))
                xmlAttrs(paramNode)["value"] <- eval(item$default)
            else
                if (type %in% c("integer", "float"))
                    xmlAttrs(paramNode)["value"] <- ""

            xmlAttrs(paramNode)["help"] <- getHelpFromText(rd, name)
            
            if ((!is.null(param)) && length(param@label))
                item$label <- param@label
            
            attributeFields <- c("label",  "min", "max",
                "force_select", "display", "checked", "size")
            if ( (!is.null(param)) && param@required){
                item$label <- paste("[required]", item$label)
                
            }
            for (field in attributeFields)
            {
                if (!is.null(param))
                {
                    value <- as.character(slot(param, field))
                    if (length(value) > 0)
                        xmlAttrs(paramNode)[field] <- value
                }
            }

            if (is.null(param))
                size <- numeric(0)
            else
                size <- as.character(slot(param, "size"))
            if (type == "text" && length(size) == 0)
                xmlAttrs(paramNode)['size'] <- "60"
            
            xmlAttrs(paramNode)['label'] <- item$label
            
            
            if (type=="select")
            {
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
                stop(sprintf("GalaxyOutput '%s' must have a parameter.", name))
            galaxyOutput <- eval(item$default)
            xmlAttrs(dataNode)["format"] <- galaxyOutput@format
            xmlAttrs(dataNode)["name"] <- name
            xmlAttrs(dataNode)["label"] <- as.character(galaxyOutput)
            
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

createScriptFile <- function(scriptFileName, func, funcName, funcInfo,
    paramList, package, is.exported, useRserve)
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
    if (useRserve)
    {
        repList$LIBRARY <- "suppressPackageStartupMessages(library(RSclient))"
        repList$DOCALL <- paste("c <- RS.connect()",
            "RS.eval(c, options('useFancyQuotes' = FALSE))",
            "RS.assign(c, 'params', params)",
            "RS.assign(c, 'wrappedFunction', wrappedFunction)",
            "RS.eval(c, setClass('GalaxyRemoteError', contains='character'))",
            sprintf("res <- RS.eval(c, wrappedFunction(%s))",
                repList$FULLFUNCNAME),
#            sprintf("res <- RS.eval(c, do.call(%s, params))", 
#                repList$FULLFUNCNAME),
            "RS.close(c)",
            "if(is(res, 'GalaxyRemoteError'))stop(res)",
            sep="\n")

    } else {
        repList$DOCALL <- sprintf("do.call(%s, params)", repList$FULLFUNCNAME)
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

## todo: fix so "numOTUs" returns "Num OTUs" instead of "Num O T Us"
getFriendlyName <- function(camelName)
{
    chars <- strsplit(camelName, split="")
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
    ret$selectoptions <- NULL
    f <- formals(func)[[param]]
    cl <- NULL
    tryCatch(cl <- class(eval(f)), error=function(x){})
    if (is.null(cl))
        stop(sprintf("No type specified for parameter '%s'.", param))
    ret$type <- class(eval(f))
    if (ret$type == "list") 
    {
        msg <- sprintf("'list' is an invalid type for parameter '%s'.\n",
            param)
        msg <- c(msg,
            "Use a homogeneous type like 'integer', 'character', etc.")
        stop(msg)
    }
    ret$length <- length(eval(f))
    if (ret$length == 1)
        ret$default <- f
    else if (ret$length == 0)
        ret$default <- NULL ## ??
    else
        ret$selectoptions <- f
    ret$label <- getFriendlyName(param)
    return(ret)
}
