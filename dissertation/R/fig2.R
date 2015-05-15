library(grid)

#can use for internal functions
funBox <- function(title, contents = NULL, x = .5, y = .5){
  nlabel <- length(contents) + length(title)
  tablevp <- 
    viewport(x = x, y = y,
             width = max(stringWidth(c(contents,title))) +
               unit(4, "mm"),
             height = unit(nlabel, "lines"))
  pushViewport(tablevp)
  grid.roundrect(gp = gpar(fill = "grey"))
  grid.clip(y = unit(nlabel-1, "lines"),
            just = "top")
  grid.roundrect(gp=gpar(fill = "white"))
  grid.clip()
  grid.text(title, x = unit(2, "mm"), y = unit(nlabel-.5, "lines"),
            just = "left")
  grid.text(contents, x = unit(2, "mm"), y = unit((nlabel-1):1 -.5, "lines"),
                               just = "left")
  
  popViewport()
}

#need a function that is the same as above but with grey result line
funcResBox <- function(title, contents = NULL, result = TRUE, x = .5, y = .5){
  nlabel <- length(contents) + length(title) + 1
  tablevp <- 
    viewport(x = x, y = y,
             width = max(stringWidth(c(contents,title))) +
               unit(4, "mm"),
             height = unit(nlabel, "lines"))
  pushViewport(tablevp)
  grid.roundrect(gp = gpar(fill = "grey"))
  grid.clip(y = unit(nlabel-1, "lines"),
            just = "top")
  grid.roundrect(gp=gpar(fill = "white"))
  grid.clip()
  grid.text(title, x = unit(2, "mm"), y = unit(nlabel-.5, "lines"),
            just = "left")
  grid.text(contents, x = unit(2, "mm"), y = unit((nlabel-1):2 -.5, "lines"),
            just = "left") 
  grid.lines(x = c(0, 1), y = unit(rep(nlabel-1,2),"lines"))
  grid.lines(x = c(0, 1), y = unit(rep(1,2), "lines"))
  grid.text("return result", x = unit(2, "mm"), y = unit(0.5, "lines"),
            just = "left") 
  popViewport()
}

funcResBox2 <- function(title, contents = NULL, result = TRUE, x = .5, y = .5){
  nlabel <- length(contents) + length(title) + 1
  tablevp <- 
    viewport(x = x, y = y,
             width = max(stringWidth(c(contents,title))) +
               unit(4, "mm"),
             height = unit(nlabel*2, "lines"))
  pushViewport(tablevp)
  grid.roundrect(gp = gpar(fill = "grey"))
  grid.clip(y = unit(nlabel + nlabel/2 + 1, "lines"),
            just = "top")
  grid.roundrect(gp=gpar(fill = "white"))
  grid.clip()
  grid.text(title, x = unit(2, "mm"), y = unit(nlabel+ 4, "lines"),
            just = "left")
  grid.text(contents, x = unit(2, "mm"), y = unit(seq(from = (nlabel+nlabel/2 + 0.5), 
                                                      to = (nlabel - 2), length.out = length(contents)), 
                                                  "lines"), just = "left") 
  grid.lines(x = c(0, 1), y = unit(rep(nlabel + nlabel/2 + 1,2),"lines"))
  grid.lines(x = c(0, 1), y = unit(rep(1,2), "lines"))
  grid.text("return result", x = unit(2, "mm"), y = unit(0.5, "lines"),
            just = "left") 
  popViewport()
}



#box1 <- funBox(x = 0.75, y = 0.25, title = "sanitize.R", contents = c("sanitize", "sanitize.numbers", "sanitize.final"))
#box2 <- funcResBox(x=0.4, y=0.5, title = "function.R", contents = c("pre-process","buildParams","makeTable"))

#not really dynamic -> but looks correct for now

#plot.new()
#plot.window(xlim = c(0, 1), ylim = c(0, 1))
#dev.off()
pushViewport(plotViewport(rep(1,4)))
box2 <- funcResBox2(x=0.15, y=0.5, title = "xtable.R", contents = c("pre-process","buildParams","makeTable"))
#grid.curve(x1 = 0.27,y1 =0.6 , x2 =.5 , y2 =0.75)


grid.curve(x1 = 0.225,y1 =0.625 , x2 =.665 , y2 =0.90, arrow = arrow(angle = 25, length = unit(0.1, "inches"), ends = "last", type = "open"),
           inflect = TRUE)
funBox(x = 0.8, y = 0.9, title = "validation.R", contents = c("envirValidate", "addToRowValidate"))
grid.curve(x1 = 0.225,y1 =0.6 , x2 =.665 , y2 =0.7, arrow = arrow(angle = 25, length = unit(0.1, "inches"), ends = "last", type = "open"),
           inflect = TRUE)
funBox(x = 0.8, y = 0.7, title = "preProcessInternal.R", contents = c("captionOrganise", "lineRule", "hlineLocations", "assignAddCommands"))

