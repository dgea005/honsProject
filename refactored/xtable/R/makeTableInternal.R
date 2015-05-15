#final part of print.xtable; uses preprocessed user args and CAPS variables from buildFunctions

#include R-version, package version, timestamp at top of table in commented form
addComment <- function(result, BCOMMENT, type, info, ECOMMENT, timestamp){
    result <- result + BCOMMENT + type + " table generated in " +
        info$language + " " + info$major + "." + info$minor +
        " by xtable " +  packageDescription('xtable')$Version +
        " package" + ECOMMENT
    if (!is.null(timestamp)){
        result <- result + BCOMMENT + timestamp + ECOMMENT    
    }
}


## Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 only.contents
captionAdd <- function(result, BTABLE, BENVIRONMENT, floating, caption, 
                        caption.placement, type, x, BCAPTION, ECAPTION, 
                        BLABEL, ELABEL, BSIZE, BTABULAR) {
    result <- result + BTABLE
    result <- result + BENVIRONMENT
    if ( floating == TRUE ) {
        if ((!is.null(caption)) &&
            (type == "html" ||caption.placement == "top")) {
            result <- result + BCAPTION + caption + ECAPTION
        }
        if (!is.null(attr(x, "label", exact = TRUE)) &&
            (type == "latex" && caption.placement == "top")) {
            result <- result + BLABEL +
                      attr(x, "label", exact = TRUE) + ELABEL
        }
    }
    result <- result + BSIZE
    result <- result + BTABULAR
    return(result)
}



colNames <- function(include.colnames, result, BROW, BTH, include.rownames, type, x,
                      STH, sanitize.colnames.function, sanitize, rotate.colnames, ETH, EROW){
    ## Claudio Agostinelli <claudio@unive.it> dated 2006-07-28
    ## include.colnames, include.rownames
    if (include.colnames) {
        result <- result + BROW + BTH
        if (include.rownames) {
             result <- result + STH
        }
        ## David G. Whiting in e-mail 2007-10-09
        if (is.null(sanitize.colnames.function)) {
             CNAMES <- sanitize(names(x), type)
        } else {
             CNAMES <- sanitize.colnames.function(names(x))
        }
        if (rotate.colnames) {
             ##added by Markus Loecher, 2009-11-16
            CNAMES <- paste("\\begin{sideways}", CNAMES, "\\end{sideways}")
        }
        result <- result + paste(CNAMES, collapse = STH)
        result <- result + ETH + EROW
    } else {
        CNAMES <- NULL
        result <- result + paste(CNAMES, collapse = STH)
        result <- result + ETH
        #an extra \\ is being added here when include.rownames = FALSE and include.colnames = FALSE
        #REMOVE EROW  
    }
    return(result)
 }




rowNames <- function(sanitize.rownames.function, x, type, rotate.rownames,sanitize){
    ## David G. Whiting in e-mail 2007-10-09
    if (is.null(sanitize.rownames.function)) {
        RNAMES <- sanitize(row.names(x), type)
    } else {
        RNAMES <- sanitize.rownames.function(row.names(x))
    }
    if (rotate.rownames) {
        ##added by Markus Loecher, 2009-11-16
        RNAMES <- paste("\\begin{sideways}", RNAMES, "\\end{sideways}")
    }
    return(RNAMES)
}



## Code for letting "digits" be a matrix was provided by
## Arne Henningsen <ahenningsen@agric-econ.uni-kiel.de>
## in e-mail dated 2005-06-04.
##if( !varying.digits ) {
## modified Claudio Agostinelli <claudio@unive.it> dated 2006-07-28
##  attr(x,"digits") <- matrix( attr( x, "digits",exact=TRUE ),
## nrow = nrow(x), ncol = ncol(x)+1, byrow = TRUE )

contentFormat = function(x, pos, format.args, varying.digits, cols,
                        sanitize, sanitizeNumbers, type, math.style.negative,
                        sanitize.text.function, NA.string) {
    for(i in 1:ncol(x)) {   
        xcol <- x[, i]
        if(is.factor(xcol))
            xcol <- as.character(xcol)
        if(is.list(xcol)) {
            xcol <- sapply(xcol, unlist)
        }
        ina <- is.na(xcol)
        is.numeric.column <- is.numeric(xcol)
        if(is.character(xcol)) {
            cols[, i+pos] <- xcol
        } else {
            if (is.null(format.args)){
                format.args <- list()
            }
            if (is.null(format.args$decimal.mark)){
                format.args$decimal.mark <- options()$OutDec
            }
            if(!varying.digits){
                curFormatArgs <-  c(list(
                              x = xcol,
                              format =
                                  ifelse(attr(x, "digits", exact = TRUE )[i+1] < 0, "E",
                                           attr(x, "display", exact = TRUE )[i+1]),
                                  digits = abs(attr(x, "digits", exact = TRUE )[i+1])),
                                  format.args)
                cols[, i+pos] <- do.call("formatC", curFormatArgs)
            }else{
                for( j in 1:nrow( cols ) ) {
                            curFormatArgs <- c(list(
                                                   x = xcol[j],
                                                   format =
                                                       ifelse(attr(x, "digits", exact = TRUE )[j, i+1] < 0,
                                                       "E", attr(x, "display", exact = TRUE )[i+1]),
                                                    digits =
                                                        abs(attr(x, "digits", exact = TRUE )[j, i+1])),
                                                    format.args)
                            cols[j, i+pos] <- do.call("formatC", curFormatArgs)
              }
            }
      }
      ## End Ian Fellows changes
      if ( any(ina) ) cols[ina, i+pos] <- NA.string
      ## Based on contribution from Jonathan Swinton <jonathan@swintons.net>
      ## in e-mail dated Wednesday, January 17, 2007
      if ( is.numeric.column ) {
          cols[, i+pos] <- sanitizeNumbers(cols[, i+pos], type, math.style.negative)
      } else {
          if (is.null(sanitize.text.function)) {
              cols[, i+pos] <- sanitize(cols[, i+pos], type)
          } else {
              cols[, i+pos] <- sanitize.text.function(cols[, i+pos])
          }
      }
    }
    return(cols)
}

#build up matrix with the params variables
addSeparators <- function(x, pos, BTD1, BTD2, BTD3, cols, ETD,
                            EROW, lastcol, BROW){
    multiplier <- 5
    full <- matrix("", nrow = nrow(x), ncol = multiplier*(ncol(x)+pos)+2)
    full[, 1] <- BROW
    full[, multiplier*(0:(ncol(x)+pos-1))+2] <- BTD1
    full[, multiplier*(0:(ncol(x)+pos-1))+3] <- BTD2
    full[, multiplier*(0:(ncol(x)+pos-1))+4] <- BTD3
    full[, multiplier*(0:(ncol(x)+pos-1))+5] <- cols
    full[, multiplier*(0:(ncol(x)+pos-1))+6] <- ETD

    full[, multiplier*(ncol(x)+pos)+2] <- paste(EROW, lastcol[-(1:2)],
                                                sep = " ")
    return(full)

}


longtableFinal <- function(booktabs, result, PHEADER, caption.placement, caption,
                           type, BCAPTION, ECAPTION, x, BLABEL, ELABEL, tabular.environment){
    ## booktabs change added the if() - 1 Feb 2012
    if(!booktabs) {
        result <- result + PHEADER
    }

    ## fix 10-27-09 Liviu Andronic (landronimirc@gmail.com) the
    ## following 'if' condition is inserted in order to avoid
    ## that bottom caption interferes with a top caption of a longtable
    if(caption.placement == "bottom"){
        if ((!is.null(caption)) && (type == "latex")) {
            result <- result + BCAPTION + caption + ECAPTION
        }
    }
    if (!is.null(attr(x, "label", exact = TRUE))) {
        result <- result + BLABEL + attr(x, "label", exact = TRUE) + ELABEL
    }
    ETABULAR <- "\\end{longtable}\n"
    return(list(result = result, ETABULAR = ETABULAR))   
}


floatingFinal <- function(caption, type, caption.placement,result,
                          BCAPTION, ECAPTION, x, BLABEL, ELABEL){
    if ((!is.null(caption)) &&
        (type == "latex" && caption.placement == "bottom")) {
        result <- result + BCAPTION + caption + ECAPTION
    }
    if (!is.null(attr(x, "label", exact = TRUE)) &&
        caption.placement == "bottom") {
        result <- result + BLABEL + attr(x, "label", exact = TRUE) + ELABEL
    }
    return(result)
}