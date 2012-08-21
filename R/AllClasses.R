    setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)


setClass("Galaxy")

setClass("GalaxyConfig", contains="Galaxy",
    representation("galaxyHome"="character",
        "toolDir"="character",
        "sectionName"="character",
        "sectionId"="character"),
        validity=function(object){
            rc <- new("MsgClass", name=character(0))

            e <- function(m) {
                rc$name <- c(rc$name, m)
            }
            
            if( (!file.exists(object@galaxyHome)) && 
                (!file.info(galaxyHome)$isdir) )
            {
                e(paste("Directory", object@galaxyHome,
                    "does not exist or is not a directory."))
            }
            
            if(!nzchar(object@toolDir)) e("toolDir cannot be empty.")
            if(!nzchar(object@sectionName)) e("sectionName cannot be empty.")
            if(!nzchar(object@sectionId)) e("sectionId cannot be empty.")
            
            
            if (length(rc$name) == 0) TRUE else rc$name
            
        })

GalaxyConfig <- function(galaxyHome, toolDir, sectionName, sectionId)
{
    new("GalaxyConfig", galaxyHome=galaxyHome, toolDir=toolDir,
        sectionName=sectionName, sectionId=sectionId)
}

setClass("GalaxyParam",
    representation( 
        type="character",
        label="character", 
        value="character", ## from args(func) ??
        ## optional: not supported
        min="numeric",
        max="numeric",
        format="character",
        ## data_ref: not supported
        force_select="logical", 
        display="character", ## one of: checkboxes, radio
        ## multiple: not supported
        ## numerical: not supported
        ## hierarchy: not supported
        checked="logical",
        ## truevalue: not supported
        ## falsevalue: not supported
        size="numeric",
        selectoptions="list",
        required="logical",
        requiredMsg="character"
        
    ), contains="Galaxy", validity=function(object){
        
        empty <- function(x) {
            return(length(slot(object, x))==0)
        }

        rc <- new("MsgClass", name=character(0))
        
        e <- function(m) {
            rc$name <- c(rc$name, m)
        }
        
        for (requiredField in c("type", "label"))
        {
            if (empty(requiredField))
                e(paste("Required field", requiredField, "is missing."))
        }
        
        if(empty("type")) {
            e("type cannot be empty")
        }
        
        if(object@type == "output") {
            e("output is an invalid type, use GalaxyOutput parameters")
        }
        
        if((!object@type %in% c("output")) && !empty("format"))
            e("'format' is only used when 'type' is 'data'.")
        
        ## FIXME: get the user's actual galaxy home and look there 
        ## for supported extensions
        if (!empty("format") && !object@format %in% getSupportedExtensions())
            e(paste("The format", object@format, "is not supported."))
        
        if ((!empty("size")) && (!object@type=="text"))
            e("'type' must be 'text' if 'size' is specified.")
        
        if ((!object@type %in% c("integer", "float"))  &&
            ((!empty("min")) || (!empty("max"))))
                e("'min' and 'max' can only be used when type is 'integer' or 'float'")
        if ( (!empty("min")) && (!empty("max")) &&
            (!object@max > object@min))
                e("'max' must be larger than 'min'.")
        
        if (length(object@force_select))
        {
            if (!object@type=="select")
                e("'force_select' can only be used when 'type' is 'select'.")
        }
        

        if (!empty("display"))
        {
            if (!object@type=="select")
                e("'display' can only be used when 'type' is 'select'.")
                
            if (!object@display %in% c("checkboxes", "radio"))
                e("value of 'display' must be 'checkboxes' or 'radio'.")
        }
        
        if (object@type=="select" && empty("selectoptions"))
            e("if type is select, selectoptions must be provided")


        if ((!object@type=="select") && (!empty("selectoptions")))
            e("selectoptions should only be provided if type is select")

        
        if (!empty("selectoptions"))
        {
            l <- object@selectoptions
            if (any(which(nchar(names(l))==0)) || is.null(names(l)))
                e("each item in selectoptions must be named")
        }

        msg <- rc$name
        if (length(msg) == 0) TRUE else msg
    })

GalaxyParam <- function(
        type=character(0),
        label=character(0), 
        value=character(0),
        min=numeric(0), 
        max=numeric(0),
        format=character(0),
        force_select=logical(0),
        display=character(0),
        checked=logical(0),
        size=numeric(0),
        selectoptions=list(),
        required=FALSE,
        requiredMsg="This field is required.")
{
    new("GalaxyParam", type=type, label=label,
        value=value, min=min, max=max, format=format,
        force_select=force_select, display=display, checked=checked,
        size=size, selectoptions=selectoptions,
        required=required, requiredMsg=requiredMsg)
}

setClass("GalaxyOutput", representation(format="character"),
    contains="Galaxy", validity=function(object){
        empty <- function(x) {
            return(length(slot(object, x))==0)
        }
        rc <- new("MsgClass", name=character(0))
        e <- function(m) {
            rc$name <- c(rc$name, m)
        }

        if (empty("format")) {
            e("Format must be supplied.")
        }


        if (!object@format %in% getSupportedExtensions())
        {
            e(paste("The format", object@format, "is not supported"))
        }

        msg <- rc$name
        if (length(msg) == 0) TRUE else msg

    })

GalaxyOutput <-
    function(format)
{
    new("GalaxyOutput", format=format)
}

    

