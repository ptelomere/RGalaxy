setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)



setClass("GalaxyParam",
    representation(name="character", ## from Rd file (?)
        type="character",
        label="character", 
        value="character", ## from args(func) ??
        ## optional: not supported
        min="numeric",
        max="numeric",
        format="character",
        ## data_ref: not supported
        force_select="character", ## one of "true" or "false"
        display="character", ## one of: checkboxes, radio
        ## multiple: not supported
        ## numerical: not supported
        ## hierarchy: not supported
        checked="character", ## one of: yes, true, on
        ## truevalue: not supported
        ## falsevalue: not supported
        size="numeric",
        selectoptions="list"
        
    ), validity=function(object){
        
        empty <- function(x) {
            return(length(slot(object, x))==0)
        }

        rc <- new("MsgClass", name=character(0))
        
        e <- function(m) {
            rc$name <- c(rc$name, m)
        }
        
        for (requiredField in c("name", "type"))
        {
            if (empty(requiredField))
                e(paste("Required field", requiredField, "is missing."))
        }
        
        
        if(object@type == "output") {
            if (empty("format"))
                e("'format' is required.")
        } else {
            if (empty("label"))
                e("'label' is required!")
        }
        
        if((!object@type %in% c("data", "output")) && !empty("format"))
            e("'format' is only used when 'type' is 'data' or 'output'.")
        
        
        if ((!empty("size")) && (!object@type=="text"))
            e("'type' must be 'text' if 'size' is specified.")
        
        if ((!object@type %in% c("integer", "float"))  &&
            ((!empty("min")) || (!empty("max"))))
                e("'min' and 'max' can only be used when type is 'integer' or 'float'")
        if ( (!empty("min")) && (!empty("max")) &&
            (!object@max > object@min))
                e("'max' must be larger than 'min'.")
        
        if (!empty("force_select"))
        {
            if (!object@type=="select")
                e("'force_select' can only be used when 'type' is 'select'.")
                
            if (!object@force_select %in% c("true", "false"))
                e("'force_select' value must be 'true' or 'false'.")
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
        
        if (!empty("selectoptions"))
        {
            l <- object@selectoptions
            if (any(which(nchar(names(l))==0)) || is.null(names(l)))
                e("each item in selectoptions must be named")
        }

        msg <- rc$name
        if (length(msg) == 0) TRUE else msg
    })

GalaxyParam <- function(name=character(0),
        type=character(0),
        label=character(0), 
        value=character(0),
        min=numeric(0), 
        max=numeric(0),
        format=character(0),
        force_select=character(0),
        display=character(0),
        checked=character(0),
        size=numeric(0),
        selectoptions=list())
{
    new("GalaxyParam", name=name, type=type, label=label,
        value=value, min=min, max=max, format=format,
        force_select=force_select, display=display, checked=checked,
        size=size, selectoptions=selectoptions)
}
    
## todo add a show method:
#setMethod("show", GalaxyParam, function(object){
#})

