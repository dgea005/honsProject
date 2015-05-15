## Based on contribution from Jonathan Swinton
## <jonathan@swintons.net> in e-mail dated Wednesday, January 17, 2007

sanitize <- function(str, type) {
    if(type == "latex"){
        result <- str
        result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
        result <- gsub("$", "\\$", result, fixed = TRUE)
        result <- gsub(">", "$>$", result, fixed = TRUE)
        result <- gsub("<", "$<$", result, fixed = TRUE)
        result <- gsub("|", "$|$", result, fixed = TRUE)
        result <- gsub("{", "\\{", result, fixed = TRUE)
        result <- gsub("}", "\\}", result, fixed = TRUE)
        result <- gsub("%", "\\%", result, fixed = TRUE)
        result <- gsub("&", "\\&", result, fixed = TRUE)
        result <- gsub("_", "\\_", result, fixed = TRUE)
        result <- gsub("#", "\\#", result, fixed = TRUE)
        result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
        result <- gsub("~", "\\~{}", result, fixed = TRUE)
        result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", result, fixed = TRUE)
        return(result)
    } else {
        result <- str
        ## Changed as suggested in bug report #2795
        ## That is replacement of "&" is "&amp;"
        ## instead of previous "&amp" etc
        ## result <- gsub("&", "&amp ", result, fixed = TRUE)
        ## result <- gsub(">", "&gt ", result, fixed = TRUE)
        ## result <- gsub("<", "&lt ", result, fixed = TRUE)
        result <- gsub("&", "&amp;", result, fixed = TRUE)
        result <- gsub(">", "&gt;", result, fixed = TRUE)
        result <- gsub("<", "&lt;", result, fixed = TRUE)
        ## Kurt Hornik <Kurt.Hornik@wu-wien.ac.at> on 2006/10/05
        ## recommended not escaping underscores.
        ## result <- gsub("_", "\\_", result, fixed=TRUE)
        return(result)
    }
}


sanitizeNumbers <- function(x, type, math.style.negative){
    if (type == "latex"){
        result <- x
        if ( math.style.negative ) {
        ## Jake Bowers <jwbowers@illinois.edu> in e-mail
        ## from 2008-08-20 suggested disabling this feature to avoid
        ## problems with LaTeX's dcolumn package.
        ## by Florian Wickelmaier <florian.wickelmaier@uni-tuebingen.de>
        ## in e-mail from 2008-10-03 requested the ability to use the
        ## old behavior.
            for(i in 1:length(x)) {
                result[i] <- gsub("-", "$-$", result[i], fixed = TRUE)
            }
        }
    return(result)
    } else {
        return(x)
    }
}


sanitizeFinal <- function(result, type){
    if (type == "latex"){
        return(result)
    } else {
        ## Suggested by Uwe Ligges <ligges@statistik.uni-dortmund.de>
        ## in e-mail dated 2005-07-30.
        result$text <- gsub("  *", " ",  result$text, fixed = TRUE)
        result$text <- gsub(' align="left"',  "", result$text,
                            fixed = TRUE)
        return(result)
    }
}