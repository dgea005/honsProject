#create CAPS variables parameters

latexParams <- function(type, tabular.environment, floating, floating.environment, table.placement, latex.environments,
                        x, include.rownames, width, caption.placement, short.caption, caption, lastcol, scalebox, size, caption.width)
{
        BCOMMENT <- "% "
        ECOMMENT <- "\n"
        ## See e-mail from "John S. Walker <jsw9c@uic.edu>" dated 5-19-2003
        ## regarding "texfloat"

        if ( floating == TRUE ) {
            # validate environments then create environment output
            fRes = floatingEnv(floating.environment, table.placement, latex.environments)
            BTABLE <- fRes$BTABLE
            ETABLE <- fRes$ETABLE
            BENVIRONMENT <- fRes$BENVIRONMENT
            EENVIRONMENT <- fRes$EENVIRONMENT
        } else {
            BTABLE <- ""
            ETABLE <- ""
            BENVIRONMENT <- ""
            EENVIRONMENT <- ""
        }

        tmp.index.start <- indexStart(include.rownames, x)

        # assign width argument and check it is valid
        WIDTH <- tbWidth(width, tabular.environment)
        # continue tabular env
        BTABULAR <- bTab(tabular.environment, WIDTH, tmp.index.start, x)

        if (tabular.environment == "longtable" && caption.placement == "top") {
            lcRes <- longCaptionTop(short.caption, caption, type, BTABULAR)
            BCAPTION <- lcRes$BCAPTION; ECAPTION <- lcRes$ECAPTION      #not sure if these are used again
            BTABULAR <- lcRes$BTABULAR
        }
        ## Claudio Agostinelli <claudio@unive.it> dated 2006-07-28
        ## add.to.row position -1

        BTABULAR <- paste(BTABULAR, lastcol[1], sep = "")
        ## the \hline at the end, if present, is set in full matrix
        ETABULAR <- paste("\\end{", tabular.environment, "}\n", sep = "")

        ## Add scalebox - CR, 7/2/12
        sbRes <- scaleBox(scalebox, BTABULAR, ETABULAR)
        BTABULAR <- sbRes$BTABULAR
        ETABULAR <- sbRes$ETABULAR

        # size
        sizeRes <- sizeLatex(size)
        BSIZE <- sizeRes$BSIZE
        ESIZE <- sizeRes$ESIZE

        BLABEL <- "\\label{"
        ELABEL <- "}\n"

        capRes <- captionLatex(caption.width, short.caption)
        BCAPTION <- capRes$BCAPTION
        ECAPTION <- capRes$ECAPTION

        BROW <- ""
        EROW <- " \\\\ \n"
        BTH <- ""
        ETH <- ""
        STH <- " & "
        BTD1 <- " & "
        BTD2 <- ""
        BTD3 <- ""
        ETD  <- ""
        params = list(BCOMMENT = BCOMMENT, ECOMMENT = ECOMMENT, BTABLE = BTABLE, ETABLE = ETABLE, BENVIRONMENT= BENVIRONMENT, 
                      EENVIRONMENT = EENVIRONMENT, BTABULAR = BTABULAR, BCAPTION = BCAPTION, ECAPTION = ECAPTION, ETABULAR = ETABULAR,
                      BSIZE = BSIZE, ESIZE = ESIZE, BLABEL = BLABEL, ELABEL = ELABEL, BROW = BROW,EROW = EROW, BTH = BTH, ETH = ETH,
                      STH = STH, BTD1 = BTD1, BTD2 = BTD2, BTD3 = BTD3, ETD = ETD)
        return(params)
} 


htmlParams <- function(html.table.attributes, caption.placement, x, pos){
        BCOMMENT <- "<!-- "
        ECOMMENT <- " -->\n"
        BTABLE <- paste("<TABLE ", html.table.attributes, ">\n", sep = "")
        ETABLE <- "</TABLE>\n"
        BENVIRONMENT <- ""
        EENVIRONMENT <- ""
        BTABULAR <- ""
        ETABULAR <- ""
        BSIZE <- ""
        ESIZE <- ""
        BLABEL <- "<A NAME="
        ELABEL <- "></A>\n"
        BCAPTION <- paste("<CAPTION ALIGN=\"", caption.placement, "\"> ",
                          sep = "")
        ECAPTION <- " </CAPTION>\n"
        BROW <- "<TR>"
        EROW <- " </TR>\n"
        BTH <- " <TH> "
        ETH <- " </TH> "
        STH <- " </TH> <TH> "
        BTD1 <- " <TD align=\""
        align.tmp <- attr(x, "align", exact = TRUE)
        align.tmp <- align.tmp[align.tmp!="|"]
        BTD2 <- matrix(align.tmp[(2-pos):(ncol(x)+1)],
                       nrow = nrow(x), ncol = ncol(x)+pos, byrow = TRUE)
        ## Based on contribution from Jonathan Swinton <jonathan@swintons.net>
        ## in e-mail dated Wednesday, January 17, 2007
        BTD2[regexpr("^p", BTD2)>0] <- "left"
        BTD2[BTD2 == "r"] <- "right"
        BTD2[BTD2 == "l"] <- "left"
        BTD2[BTD2 == "c"] <- "center"
        BTD3 <- "\"> "
        ETD  <- " </TD>"
		#list of all parameters created by assignments above (except WIDTH and tmp.index.start)
        params = list(BCOMMENT = BCOMMENT, ECOMMENT = ECOMMENT, BTABLE = BTABLE, ETABLE = ETABLE, BENVIRONMENT= BENVIRONMENT, 
                      EENVIRONMENT = EENVIRONMENT, BTABULAR = BTABULAR, BCAPTION = BCAPTION, ECAPTION = ECAPTION, ETABULAR = ETABULAR,
                      BSIZE = BSIZE, ESIZE = ESIZE, BLABEL = BLABEL, ELABEL = ELABEL, BROW = BROW,EROW = EROW, BTH = BTH, ETH = ETH,
                      STH = STH, BTD1 = BTD1, BTD2 = BTD2, BTD3 = BTD3, ETD = ETD)
        return(params)
}