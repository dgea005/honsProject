## If caption is length 2, treat the second value as the "short caption"
captionOrganise <- function(x){
    caption <- attr(x, "caption", exact = TRUE)
    short.caption <- NULL
    if (!is.null(caption) && length(caption) > 1){
    short.caption <- caption[2]
    caption <- caption[1]
    }
    return(list(caption = caption, short.caption = short.caption))
}


# assign appropriate LaTeX line rules
# output: PHEADER #this contains the line rules depending on if booktabs or not
lineRule <- function(x, hline.after, booktabs){
    # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 add.to.row
    if(!booktabs){
        PHEADER <- "\\hline\n"
    } else {
        if (is.null(hline.after)){
            PHEADER <- ""
        } else {
            hline.after <- sort(hline.after)
            PHEADER <- rep("\\midrule\n", length(hline.after))
            if (hline.after[1] == -1) {
                PHEADER[1] <- "\\toprule\n"
            }
            if (hline.after[length(hline.after)] == nrow(x)) {
                PHEADER[length(hline.after)] <- "\\bottomrule\n"
            }
        }  
    }
  return(PHEADER)
}


# insert extra lineRule locations into add.to.row
hlineLocations <- function(booktabs, add.to.row, npos, hline.after, PHEADER){
    if (!is.null(hline.after)) {
        if (!booktabs){
            add.to.row$pos[[npos+1]] <- hline.after
        } else {
            for(i in 1:length(hline.after)) {
                add.to.row$pos[[npos+i]] <- hline.after[i]
            }
        }
        add.to.row$command <- c(add.to.row$command, PHEADER)
    }
    return(add.to.row)
}


#assign add commands into variables with order to be used
assignAddCommands <- function(add.to.row, lastcol){
    for (i in 1:length(add.to.row$command)) {
        addpos <- add.to.row$pos[[i]]
        freq <- table(addpos)
        addpos <- unique(addpos)
        for (j in 1:length(addpos)) {
            lastcol[addpos[j]+2] <- paste(lastcol[addpos[j]+2],
                                        paste(rep(add.to.row$command[i],
                                                  freq[j]),
                                              sep = "", collapse = ""),
                                        sep = " ")
        }
  }
  return(list(addpos = addpos, freq = freq, lastcol = lastcol))
  #only lastcol is actually used again in print.xtable
}
