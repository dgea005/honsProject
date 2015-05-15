
#floating function: for making environment components which relate to floating, centering,...
floatingEnv <- function(floating.environment, table.placement, latex.environments) {
    ## See e-mail from "Pfaff, Bernhard <Bernhard.Pfaff@drkw.com>"
    ## dated 7-09-2003 regarding "suggestion for an amendment of
    ## the source"
    ## See e-mail from "Mitchell, David"
    ## <David.Mitchell@dotars.gov.au>" dated 2003-07-09 regarding
    ## "Additions to R xtable package"
    ## See e-mail from "Garbade, Sven"
    ## <Sven.Garbade@med.uni-heidelberg.de> dated 2006-05-22
    ## regarding the floating environment.
    BTABLE <- paste("\\begin{", floating.environment, "}",
                    ifelse(!is.null(table.placement),
                           paste("[", table.placement, "]", sep = ""),
                           ""), "\n", sep = "")
    if ( is.null(latex.environments) || (length(latex.environments) == 0) ) {
        BENVIRONMENT <- ""
        EENVIRONMENT <- ""
    } else {
        BENVIRONMENT <- ""
        EENVIRONMENT <- ""
        if ("center" %in% latex.environments){
            BENVIRONMENT <- paste(BENVIRONMENT, "\\centering\n", sep = "")
        }
        for (i in 1:length(latex.environments)) {
            if (latex.environments[i] == "") next
            if (latex.environments[i] != "center"){
                BENVIRONMENT <- paste(BENVIRONMENT,
                                "\\begin{", latex.environments[i],
                                "}\n", sep = "")
                EENVIRONMENT <- paste("\\end{", latex.environments[i],
                                "}\n", EENVIRONMENT, sep = "")
            }
      }
    }
    ETABLE <- paste("\\end{", floating.environment, "}\n", sep = "")
    
    return(list(BTABLE = BTABLE, ETABLE = ETABLE, 
                BENVIRONMENT = BENVIRONMENT, EENVIRONMENT = EENVIRONMENT))
}


indexStart <- function(include.rownames, x){
    tmp.index.start <- 1
    if (!include.rownames) {
        while ( attr(x, "align", exact = TRUE)[tmp.index.start] == '|' )
            tmp.index.start <- tmp.index.start + 1
        tmp.index.start <- tmp.index.start + 1
    }
    return(tmp.index.start)
}


# check that width and longtable haven't been used together plus assign width argument
tbWidth <- function(width, tabular.environment){
    ## Added "width" argument for use with "tabular*" or
    ## "tabularx" environments - CR, 7/2/12t
    if (is.null(width)){
      WIDTH <-""
    } else if (is.element(tabular.environment, c("tabular", "longtable"))){
        warning("Ignoring 'width' argument.  The 'tabular' and 'longtable' environments do not support a width specification.  Use another environment such as 'tabular*' or 'tabularx' to specify the width.")
        WIDTH <- ""
    } else {
        WIDTH <- paste("{", width, "}", sep = "")
    }
    return(WIDTH)
}


#BTABULAR function
bTab <- function(tabular.environment, WIDTH, tmp.index.start, x){
    paste("\\begin{", tabular.environment, "}",
          WIDTH, "{",
          paste(c(attr(x, "align",
                       exact = TRUE)[
                       tmp.index.start:length(attr(x, "align",
                                                   exact = TRUE))],
                  "}\n"),
                sep = "", collapse = ""),
          sep = "")
}



longCaptionTop <- function(short.caption, caption, type, BTABULAR){
    ## fix 10-26-09 (robert.castelo@upf.edu) the following
    ## 'if' condition is added here to support
    ## a caption on the top of a longtable
    if (is.null(short.caption)){
        BCAPTION <- "\\caption{"
    } else {
        BCAPTION <- paste("\\caption[", short.caption, "]{", sep = "")
    }
    ECAPTION <- "} \\\\ \n"
    if ((!is.null(caption)) && (type == "latex")) {
        BTABULAR <- paste(BTABULAR,  BCAPTION, caption, ECAPTION,
                      sep = "")
    }
    return(list(BCAPTION = BCAPTION, ECAPTION = ECAPTION, BTABULAR = BTABULAR))
}



scaleBox <- function(scalebox, BTABULAR, ETABULAR){
    if (!is.null(scalebox)){
        BTABULAR <- paste("\\scalebox{", scalebox, "}{\n", BTABULAR, sep = "")
        ETABULAR <- paste(ETABULAR, "}\n", sep = "")
    }
    return(list(BTABULAR = BTABULAR, ETABULAR = ETABULAR))
}


sizeLatex <- function(size){
    ## BSIZE contributed by Benno <puetz@mpipsykl.mpg.de> in e-mail
    ## dated Wednesday, December 01, 2004
    if (is.null(size) || !is.character(size)) {
        BSIZE <- ""
        ESIZE <- ""
    } else {
        if(length(grep("^\\\\", size)) == 0){
            size <- paste("\\", size, sep = "")
        }
        BSIZE <- paste("{", size, "\n", sep = "")
        ESIZE <- "}\n"
    }
    return(list(BSIZE = BSIZE, ESIZE = ESIZE))
}


captionLatex <- function(caption.width, short.caption){
    ## Added caption width (jeff.laake@nooa.gov)
    if(!is.null(caption.width)){
        BCAPTION <- paste("\\parbox{",caption.width,"}{",sep="")
        ECAPTION <- "}"
    } else {
        BCAPTION <- NULL
        ECAPTION <- NULL
    }
    if (is.null(short.caption)){
     BCAPTION <- paste(BCAPTION,"\\caption{",sep="")
    } else {
     BCAPTION <- paste(BCAPTION,"\\caption[", short.caption, "]{", sep="")
    }
    ECAPTION <- paste(ECAPTION,"} \n",sep="")
    return(list(BCAPTION=BCAPTION, ECAPTION=ECAPTION))
}
