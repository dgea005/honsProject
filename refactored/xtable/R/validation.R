#this validates that latex inputs have been entered correctly by the user
#most of this is related to LaTeX except for type
envirValidate <- function(type, floating.environment, table.placement, caption.placement, tabular.environment, floating,
                            hline.after, x){
    if (length(type)>1) stop("\"type\" must have length 1")
    if (!all(!is.na(match(type, c("latex","html"))))) {
        stop("\"type\" must be in {\"latex\", \"html\"}")
    }
    if (("margintable" %in% floating.environment)
        & (!is.null(table.placement))) {
        warning("margintable does not allow for table placement; setting table.placement to NULL")
        table.placement <- NULL
        #this isn't returned?
    }
    if (!is.null(table.placement) &&
          !all(!is.na(match(unlist(strsplit(table.placement,  split = "")),
                            c("H","h","t","b","p","!"))))) {
        stop("\"table.placement\" must contain only elements of {\"h\",\"t\",\"b\",\"p\",\"!\"}")
    }
    if (!all(!is.na(match(caption.placement, c("bottom","top"))))) {
        stop("\"caption.placement\" must be either {\"bottom\",\"top\"}")
    }
    ## See e-mail from "John S. Walker <jsw9c@uic.edu>" dated 5-19-2003
    ## regarding "texfloat"
    ## See e-mail form "Fernando Henrique Ferraz P. da Rosa"
    ## <academic@feferraz.net>" dated 10-28-2005 regarding "longtable"
    #validate that longtable and floating haven't been used together
    if ( tabular.environment == "longtable" & floating == TRUE ) {
        warning("Attempt to use \"longtable\" with floating = TRUE. Changing to FALSE.")
        floating <- FALSE
    }
    ## Claudio Agostinelli <claudio@unive.it> dated 2006-07-28
    ## hline.after checks
    if (any(hline.after < -1) | any(hline.after > nrow(x))) {
        stop("'hline.after' must be inside [-1, nrow(x)]")
    }
    return(list(floating = floating, table.placement = table.placement))
}




#check that the additional add.to.row user argument given is in the correct specification
## Claudio Agostinelli <claudio@unive.it> dated 2006-07-28
addToRowValidate <- function(add.to.row, pos, x){
    if (is.list(add.to.row) && length(add.to.row) == 2) {
        if (is.null(names(add.to.row))) {
            names(add.to.row) <- c('pos', 'command')
        } else if (any(sort(names(add.to.row))!= c('command', 'pos'))) {
            stop("the names of the elements of 'add.to.row' must be 'pos' and 'command'")
        }
        if (is.list(add.to.row$pos) && is.vector(add.to.row$command, mode = 'character')) {
            npos <- length(add.to.row$pos)
            if (npos != length(add.to.row$command)) {
                    stop("the length of 'add.to.row$pos' must be equal to the length of 'add.to.row$command'")
            }
            if (any(unlist(add.to.row$pos) < -1) | any(unlist(add.to.row$pos) > nrow(x))) {
                    stop("the values in add.to.row$pos must be inside the interval [-1, nrow(x)]")
            }
        } else {
                stop("the first argument ('pos') of 'add.to.row' must be a list, the second argument ('command') must be a vector of mode character")
        }
    } else {
        stop("'add.to.row' argument must be a list of length 2")
    }
    return(list(add.to.row = add.to.row, npos =  npos))
}


# need to add HTML table validations
# e.g. if you use rotate column names you end up with latex commands in the html