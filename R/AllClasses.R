setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)


setClass("Galaxy", contains="VIRTUAL")

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
        label="character", 
        ## optional: not supported
        min="numeric",
        max="numeric",
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
        required="logical",
        requiredMsg="character"
        
    ), 
    prototype=list(
            label=character(0),
            min=numeric(0),
            max=numeric(0),
            force_select=FALSE,
            display=character(0),
            checked=FALSE,
            size=60L,
            required=FALSE,
            requiredMsg="This field is required."
        ),
    contains=c("Galaxy","VIRTUAL"), validity=function(object){
        
        empty <- function(x) {
            return(length(slot(object, x))==0)
        }

        rc <- new("MsgClass", name=character(0))
        
        e <- function(m) {
            rc$name <- c(rc$name, m)
        }
        

        ## TODO:
        ## Some checks cannot be in this validity method (?).
        ## For example, if it's not a text-box parameter
        ## but the user specifies "size", or if they use
        ## "min" or "max" with something other than a numeric
        ## parameter.
        ## See below for more checks.
        
#        if ((!empty("size")) && (!object@type=="text"))
#            e("'type' must be 'text' if 'size' is specified.")
#        
#        if ((!object@type %in% c("integer", "float"))  &&
#            ((!empty("min")) || (!empty("max"))))
#                e("'min' and 'max' can only be used when type is 'integer' or 'float'")
#        if ( (!empty("min")) && (!empty("max")) &&
#            (!object@max > object@min))
#                e("'max' must be larger than 'min'.")
#        
#        if (length(object@force_select))
#        {
#            if (!object@type=="select")
#                e("'force_select' can only be used when 'type' is 'select'.")
#        }
        

 #       if (!empty("display"))
 #       {
 #           if (!object@type=="select")
 #               e("'display' can only be used when 'type' is 'select'.")
 #               
 #           if (!object@display %in% c("checkboxes", "radio"))
 #               e("value of 'display' must be 'checkboxes' or 'radio'.")
 #       }
        
#        if (object@type=="select" && empty("selectoptions"))
#            e("if type is select, selectoptions must be provided")


 #       if ((!object@type=="select") && (!empty("selectoptions")))
 #           e("selectoptions should only be provided if type is select")

        
#        if (!empty("selectoptions"))
#        {
#            l <- object@selectoptions
#            if (any(which(nchar(names(l))==0)) || is.null(names(l)))
#                e("each item in selectoptions must be named")
#        }

        msg <- rc$name
        if (length(msg) == 0) TRUE else msg
    })

GalaxyParam <- function(
        label=character(0), 
        min=numeric(0), 
        max=numeric(0),
        force_select=logical(0),
        display=character(0),
        checked=logical(0),
        size=numeric(0),
        required=FALSE,
        requiredMsg="This field is required.")
{
    new("GalaxyParam", label=label,
        min=min, max=max, 
        force_select=force_select, display=display, checked=checked,
        size=size, required=required, requiredMsg=requiredMsg)
}


setClass("GalaxyNonFileParam", contains=c("GalaxyParam", "VIRTUAL"))

GalaxyIntegerParam = setClass("GalaxyIntegerParam",
    representation(testValues="integer"),
    contains=c("GalaxyNonFileParam", "integer"))

GalaxyNumericParam = setClass("GalaxyNumericParam",
    representation(testValues="numeric"),
    contains=c("GalaxyNonFileParam", "numeric"))

GalaxyCharacterParam = setClass("GalaxyCharacterParam",
    representation(testValues="character"),
    contains=c("GalaxyNonFileParam", "character"))

GalaxyLogicalParam = setClass("GalaxyLogicalParam",
    representation(testValues="logical"),
    contains=c("GalaxyNonFileParam", "logical"))

GalaxySelectParam = setClass("GalaxySelectParam",
    representation(testValues="ANY"),
    contains=c("GalaxyNonFileParam", "ANY"))



setClass("GalaxyOutput", representation(format="character"),
    contains=c("Galaxy", "character"), validity=function(object){
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

        msg <- rc$name
        if (length(msg) == 0) TRUE else msg

    })

GalaxyOutput <-
    function(basename, format)
{
    filename <- paste(basename, format, sep=".")
    new("GalaxyOutput", filename, format=format)
    ## todo add sanity checks that filename is character(1) and a valid filename
}


setClass("GalaxyInputFile", contains=c("GalaxyParam", "character"),
    representation("required"="logical", "formatFilter"="character"))


GalaxyInputFile <- function(required=TRUE, formatFilter=character(0))
{
    new("GalaxyInputFile", required=required,
        formatFilter=formatFilter)
}

setClass("GalaxyRemoteError", contains="character")

RserveConnection <- setClass("RserveConnection", contains="Galaxy",
    representation("host"="character",
        port="integer"),
    prototype=list("host"="localhost",
        "port"=6311L))
