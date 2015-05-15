### xtable package
###
### Produce LaTeX and HTML tables from R objects.
###
### Copyright 2000-2013 David B. Dahl <dahl@stat.tamu.edu>
###
### Maintained by Charles Roosen <croosen@mango-solutions.com>
###
### This file is part of the `xtable' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA
print.xtable <- function(x,
  type = getOption("xtable.type", "latex"),
  file = getOption("xtable.file", ""),
  append = getOption("xtable.append", FALSE),
  floating = getOption("xtable.floating", TRUE),
  floating.environment = getOption("xtable.floating.environment", "table"),
  table.placement = getOption("xtable.table.placement", "ht"),
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),
  latex.environments = getOption("xtable.latex.environments", c("center")),
  tabular.environment = getOption("xtable.tabular.environment", "tabular"),
  size = getOption("xtable.size", NULL),
  hline.after = getOption("xtable.hline.after", c(-1,0,nrow(x))),
  NA.string = getOption("xtable.NA.string", ""),
  include.rownames = getOption("xtable.include.rownames", TRUE),
  include.colnames = getOption("xtable.include.colnames", TRUE),
  only.contents = getOption("xtable.only.contents", FALSE),
  add.to.row = getOption("xtable.add.to.row", NULL),
  sanitize.text.function = getOption("xtable.sanitize.text.function", NULL),
  sanitize.rownames.function = getOption("xtable.sanitize.rownames.function",
                                         sanitize.text.function),
  sanitize.colnames.function = getOption("xtable.sanitize.colnames.function",
                                         sanitize.text.function),
  math.style.negative = getOption("xtable.math.style.negative", FALSE),
  html.table.attributes = getOption("xtable.html.table.attributes", "border=1"),
  print.results = getOption("xtable.print.results", TRUE),
  format.args = getOption("xtable.format.args", NULL),
  rotate.rownames = getOption("xtable.rotate.rownames", FALSE),
  rotate.colnames = getOption("xtable.rotate.colnames", FALSE),
  booktabs = getOption("xtable.booktabs", FALSE),
  scalebox = getOption("xtable.scalebox", NULL),
  width = getOption("xtable.width", NULL),
  comment = getOption("xtable.comment", TRUE),
  timestamp = getOption("xtable.timestamp", date()),
  ...)
{
    type <- tolower(type)

    # validate that output type (HTML or Latex) and various latex environments are valid
    validatedVars <- envirValidate(type, floating.environment, table.placement, 
                                  caption.placement, tabular.environment, floating, 
                                  hline.after, x)
    floating <- validatedVars$floating
    table.placement <- validatedVars$table.placement

    # assign caption and short.caption from attributes for usage
    captions <- captionOrganise(x)
    short.caption <- captions$short.caption
    caption <- captions$caption

    ## Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 include.rownames,
    ## include.colnames
    pos <- 0
    if (include.rownames) pos <- 1

    ## add.to.row checks
    if (!is.null(add.to.row)) {
        #validate that add.to.row commmands have been entered correctly, return add.to.row and npos
        res <- addToRowValidate(add.to.row, pos, x)
        add.to.row <- res$add.to.row
        npos <- res$npos
    } else {
        add.to.row <- list(pos = list(),command = character(0))
        npos <- 0
    }
    
    #assign the latex line rules to be used
    if (type == "latex") {
        ##this assigns the latex line rules depending on booktabs
        PHEADER <- lineRule(x, hline.after, booktabs)
    } else {
        PHEADER <- ""
    }

    lastcol <- rep(" ", nrow(x)+2)

    #line rule locations assigned into add.to.row
    add.to.row <- hlineLocations(booktabs, add.to.row, npos, hline.after, PHEADER) 

    #assign position and command of addtorow commands
    if ( length(add.to.row$command) > 0 ) {
         addCmds <- assignAddCommands(add.to.row, lastcol)
         lastcol <- addCmds$lastcol
    }

    #params are a set of CAPS variables used to build the table
    if (type == "latex") {
        params <- latexParams(type, tabular.environment, floating, 
                              floating.environment, table.placement, 
                              latex.environments, x, include.rownames, 
                              width, caption.placement, short.caption, 
                              caption, lastcol, scalebox, size, 
                              caption.width)
    } else {
        params <- htmlParams(html.table.attributes, caption.placement, x, pos)
    }

    #use formatted user arguments and buildParams to make and return final table
    result <- makeTable(params, file, append, comment, type, timestamp,only.contents, floating, caption, 
                        caption.placement, x, include.colnames, include.rownames, sanitize.colnames.function, 
                        sanitize, rotate.colnames,pos, sanitize.rownames.function, rotate.rownames, format.args,
                        sanitizeNumbers, math.style.negative, sanitize.text.function, NA.string, lastcol, 
                        tabular.environment, booktabs, PHEADER)

    if (print.results){
	     print(result)
    }

    return(invisible(result$text))
}


makeTable <- function(params, file, append, comment, type, timestamp, only.contents, floating, caption, caption.placement, x, 
                      include.colnames, include.rownames, sanitize.colnames.function, sanitize, rotate.colnames,
                      pos, sanitize.rownames.function, rotate.rownames, format.args, sanitizeNumbers, math.style.negative, 
                      sanitize.text.function, NA.string, lastcol, tabular.environment, booktabs, PHEADER){

                    with(params, {
                                result <- string("", file = file, append = append)
                                info <- R.Version()
                                ## modified Claudio Agostinelli <claudio@unive.it> dated 2006-07-28
                                ## to set automatically the package version
                                if (comment){
                                    result <- addComment(result, BCOMMENT, type, info, ECOMMENT, timestamp) 
                                }
                                #start adding outside environment tags; caption, label, etc.
                                if (!only.contents) {                                                                 
                                    result <- captionAdd(result, BTABLE, BENVIRONMENT, floating, caption, caption.placement, 
                                                        type, x, BCAPTION, ECAPTION, BLABEL, ELABEL, BSIZE, BTABULAR)               
                                }
                             
                                #update result with user defined (or default) column specification for inclusion of names, etc.
                                result <- colNames(include.colnames, result, BROW, BTH, include.rownames, type, x,
                                                  STH, sanitize.colnames.function, sanitize, rotate.colnames, ETH, EROW)                              
                                cols <- matrix("", nrow = nrow(x), ncol = ncol(x)+pos)

                                #update result with user defined column specification (inclusion, exclusion , etc.)
                                if (include.rownames) {
                                    cols[, 1] <- rowNames(sanitize.rownames.function, x, type, rotate.rownames,sanitize)   
                                }

                                #contentFormat is for formatting digits and sanitizing content
                                varying.digits <- is.matrix( attr( x, "digits", exact = TRUE ) )
                                cols <- contentFormat(x, pos, format.args, varying.digits, cols, sanitize, sanitizeNumbers, 
                                                     type, math.style.negative, sanitize.text.function, NA.string) 

                                #uses Params that seperate rows, values to create matrix used for constructing final result
                                full <- addSeparators(x, pos, BTD1, BTD2, BTD3, cols, ETD, EROW, lastcol, BROW)

                                if (type == "latex") full[, 2] <- ""
                                result <- result + lastcol[2] + paste(t(full), collapse = "")

                                #add final environments and make adjustments if they are longtable or floating
                                if (!only.contents) {
                                    if (tabular.environment == "longtable") {
                                        ltRes <- longtableFinal(booktabs, result, PHEADER, caption.placement, caption,
                                                                type, BCAPTION, ECAPTION, x, BLABEL, ELABEL)  
                                        result <- ltRes$result
                                        ETABULAR <- ltRes$ETABULAR
                                    }
                                    result <- result + ETABULAR
                                    result <- result + ESIZE
                                    if ( floating == TRUE ) {
                                        result <- floatingFinal(caption, type, caption.placement, result,
                                                                BCAPTION, ECAPTION, x, BLABEL, ELABEL)
                                    }
                                    result <- result + EENVIRONMENT
                                    result <- result + ETABLE
                                }
                                result <- sanitizeFinal(result, type)
                    })
    }




"+.string" <- function(x, y) {
    x$text <- paste(x$text, as.string(y)$text, sep = "")
    return(x)
}



print.string <- function(x, ...) {
    cat(x$text, file = x$file, append = x$append)
    return(invisible())
}

string <- function(text, file = "", append = FALSE) {
    x <- list(text = text, file = file, append = append)
    class(x) <- "string"
    return(x)
}

as.string <- function(x, file = "", append = FALSE) {
    if (is.null(attr(x, "class", exact = TRUE)))
        switch(data.class(x),
               character = return(string(x, file, append)),
               numeric = return(string(as.character(x), file, append)),
               stop("Cannot coerce argument to a string"))
    if (class(x) == "string")
        return(x)
    stop("Cannot coerce argument to a string")
}

is.string <- function(x) {
    return(class(x) == "string")
}

