
getManPage <- function(manpage, package)
{
    if (is.null(package)) 
    {
        rd <- parse_Rd(manpage)
    } else {
        file <- system.file("help", package=package)
        file <- file.path(file, package)
        db <- tools:::fetchRdDB(file)
        rd <- db[manpage][[1]]
    }
    return(rd)
}

getTitle <- function(rd)
{
    return(sub("\\s+$", "", tools:::.Rd_get_metadata(rd, "title")))
}

getManSection <- function(rd, section)
{
    tmp <- tools:::.Rd_get_metadata(rd, section)
    fragStr <- paste(tmp, collapse="\n")
    tf <- tempfile()
    write(fragStr, file=tf)
    rd2 <- parse_Rd(tf, fragment=TRUE)
}

getHelpFromText <- function(rd, arg)
{
    args <- RGalaxy:::parseSectionFromText(rd, "Arguments")
    lines <- strsplit(args, "\n")[[1]]
    ret <- character()
    cont <- FALSE
    for (line in lines)
    {
        if (length(grep("^[^ ]*: ", line)>0))
        {
            segs <- strsplit(line, ": ")[[1]]
            remainder <- segs[-1]
            thisArg <- segs[1]
            if (arg == thisArg)
            {
                cont <- TRUE
                ret <- c(ret, paste(remainder, collapse=": "))
            } else {
                cont <- FALSE
            }
        } else {
            if (cont) 
            {
                ret <- c(ret, line)
            }
        }
    }
    return(sub("\\s+$", "", paste(ret, collapse="\n")))
}

parseSectionFromText <- function(rd, section, required=TRUE)
{
    text <- capture.output(Rd2txt(rd))
    ret <- character()
    keep <- FALSE
    found <- FALSE
    for (line in text)
    {
        if (length(grep("^_\b", line)>0) && length(grep(":$", line)>0))
        {
            keep <- FALSE ## need this?
            line <- sub(":$", "", line)
            line <- gsub("_", "", line, fixed=TRUE)
            line <- gsub("\b", "", line, fixed=TRUE)
            if (line == section)
            {
                found <- TRUE
                keep <- TRUE
            }
        } else {
            if (keep)
            {
                ret <- c(ret, line)
            }
        }
    }
    if (!found)
    {
        status = "Note: "
        if (required)
            status = ""
        msg <- sprintf("%sDid not find section '%s' in man page.", status, section) 
        if (required)
        {
            gstop(msg)
        } else {
            gmessage(msg)
            return("")
        }

    }
    ret <- gsub("^ *", "", ret)
    if (nchar(ret[1])==0 && length(ret)>2)
    {
        ret <- ret[2:length(ret)]
    }
    paste(ret, collapse="\n")
}

getHelp <- function(arg, rd)
{
    tbl <- tools:::.Rd_get_argument_table(rd)
    if (!arg %in% tbl[,1])
    {
        gstop(sprintf("No documentation for argument '%s'.", arg))
    }
    help <- tbl[tbl[,1] == arg, 2]
    help <- gsub(" {2,}", "", help)
    help <- gsub("\n", "", help)
    help
}
