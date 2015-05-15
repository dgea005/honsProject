#function to print out the main content of the table
#has options to include hlines between each line
#function to convert special char to ticks

#checkmark requires package 
#\usepackage{bbding}
body <- function(x, lines, ticks){
  if (ticks == TRUE){
    rows <- dim(x)[1]
    cols <- dim(x)[2]
    for (i in 1:rows){
      for (j in 2:cols){
        x[i, j] <- gsub(pattern = "\\S+",replacement="\\\\CheckmarkBold", x=x[i,j])
      }
    }
  }
  tbl <- as.list(do.call(paste, c(x, sep=" & ")))
  if (lines==TRUE){
    tbl <- do.call(paste, c(tbl, sep="\\\\\n\\hline\n"))
    tbl <- paste(tbl, "\\\\\n", sep="")
  } else {
    tbl <- paste(tbl, "\\\\\n", sep="")
  }
  cat(tbl)
  invisible(return)
}
#function for top line of the table - column names
#function to rotate
hdr <- function(x, rotate){
  head <- t(cbind(names(x)))
  cols <- dim(head)[2]
  if (rotate == TRUE){
    for (i in 2:cols){
      head[1,i] <- paste("\\begin{rotate}{90} ", head[1,i], "\\end{rotate}")
    }
  }
  head <- as.list(head)
  head <- do.call(paste, c(head, sep=" & "))
  head <- paste0(head, "\\\\\n\\hline\n")
  cat(head)
}

#put header and body together along seperators
wrap <- function(x, 
                 lines = FALSE, 
                 vlines = FALSE, 
                 ticks = FALSE, 
                 rotate = FALSE, 
                 caption = NULL) {
  table <- "\\begin{table}[ht]\n"
  cent <- "\\centering\n"
  cols <- dim(tbl)[2]
  if (vlines == TRUE){
    tabular <- paste0("\\begin{tabular}{|*{",cols, "}{r|}}\n")
  } else {
    tabular <- paste0("\\hspace*{-1cm}\n\\scalebox{0.75}{\n\\begin{tabular}{*{",cols, "}{r}}\n")
  }
  sep <- "\\hline\n"
  if (rotate == TRUE){
    vs <- "\\vspace{50pt}"
  } else{
    vs <- ""
  }
  endtablr <- "\\end{tabular}\n}\n\\hspace*{-1cm}\n"
  endtab <- "\\end{table}\n"
  cat(table, vs, tabular)
  if (rotate == FALSE){
    cat(sep)
  }
  hdr(x, rotate)
  body(x, lines, ticks)
  cat(sep, endtablr)
  if (!is.null(caption)){
    cat(paste0("\\caption{",caption,"}"))
  }
  cat(endtab)
}
