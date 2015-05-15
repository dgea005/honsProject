#want to create a visual representation of the original xtable / short pseudocode
#see the modular section of the computer science text book images

#save default pars
.pardefault <- par(no.readonly = T)
par(mar = c(0, 0, 0, 0))


#might be better doing this as a function
outerlayer = c("assign captions", 
               "validation", 
               "create line rules (booktab dependent)", 
               "line rule locations",
               "validation of user inputs", 
               "create LaTeX 'components'", 
               "" ,
               "create HTML 'components'", 
               "", 
               "start recording table to 'result'", 
               "R.info and timestamp",
               "caption, labels, size, tabular added", 
               "include rownames, colnames", 
               "sanitize, rotation for rownames and colnames",
               "format digits", 
               "sanitize table content, apply NA string", 
               "create matrix to hold 'components'",
               "combine matrix of 'components' with results", 
               "final latex components, add captions, etc.,", 
               "final sanitize", 
               "return result"
               )
#these occur where the empty strings above are
innerlayer = c("LaTeX sanitize functions created",
               "HTML sanitize functions created")

lineNums = c("60-66",
             "85-118",
             "120-154",
             "156-187",
             "189-213",
             "215-402",
             "",
             "403-464",
             "",
             "466-467",
             "470-478",
             "480-496",
             "499-503",
             "504-532",
             "546-596",
             "600-612",
             "614-624",
             "627",
             "628-664",
             "665",
             "667-672")

sanNums = c("366-402","437-463")
ptsBtw = 3
plot.new()
plot.window(xlim = c(13,100), ylim = c(25, 95))
#polygon(x = c(25, 25, 97, 97), y = c(87, 92, 92, 87))
segments(x0 = 25, x1 = 25, y0 = 92, y1 = 87)

segments(x0 = 25, y0 = 87, x1 = 25, y1 = 89.5 - 7*(ptsBtw))
segments(x0 = 25, x1 = 27, y0 = 89.5 - 7*(ptsBtw), y1 = 89.5 - 7*(ptsBtw))
segments(x0 = 27, x1 = 27, y0 = 89.5 - 7*(ptsBtw), y1 = 86.5 - 7*(ptsBtw))
segments(x0 = 25, x1 = 27, y0 = 86.5 - 7*(ptsBtw), y1 = 86.5 - 7*(ptsBtw))
segments(x0 = 25, x1 = 25, y0 = 86.5-(7*ptsBtw), y1 = 89.5-(9*ptsBtw))
segments(x0 = 25, x1 = 27, y0 = 89.5 - 9*(ptsBtw), y1 = 89.5 - 9*(ptsBtw))
segments(x0 = 27, x1 = 27, y0 = 89.5 - 9*(ptsBtw), y1 = 86.5 - 9*(ptsBtw))
segments(x0 = 25, x1 = 27, y0 = 86.5 - 9*(ptsBtw), y1 = 86.5 - 9*(ptsBtw))
segments(x0 = 25, x1 = 25, y0 = 86.5-(9*ptsBtw), y1 = 23)
#segments(x0 = 25, x1 = 97, y0 = 23, y1 = 23)
#segments(x0 = 97, x1 = 97, y0 = 23, y1 = 87)
text("object and attributes", x = 24.5, y = 90.5, pos = 4, cex = 1.2,
     family = "mono")
text("(obtained from user arguments in", x = 24.5, y = 88.5, pos = 4, cex = 0.9,
     family = "mono")
text("xtable and print.xtable functions)", x = 24.5, y = 87, pos = 4, cex = 0.9,
     family = "mono")
text("print.xtable", x = 25, y = 95, pos = 4, family = "mono", font = 2, cex = 1.2)
text("line numbers", x = 13, y = 57, srt = 90, family = "mono", font = 3)
for (i in 1:length(outerlayer))
  text(outerlayer[i], x = 24.5, y = 88-(i*ptsBtw), pos = 4, cex = 1.2, family = "mono")
text("LaTeX sanitize functions created", x = 27, y = 88-(7*ptsBtw), pos = 4, cex = 1.2, family = "mono")
text("HTML sanitize functions created", x = 27, y = 88-(9*ptsBtw), pos = 4, cex = 1.2, family = "mono")
#nums
for (i in 1:length(lineNums))
  text(lineNums[i], x = 25.5, y = 88-(i*ptsBtw), pos = 2, cex = 1.2, family = "mono")
text(sanNums[1], x = 27.5, y = 88-(7*ptsBtw), pos = 2, cex = 1.2, family = "mono")
text(sanNums[2], x = 27.5, y = 88-(9*ptsBtw), pos = 2, cex = 1.2, family = "mono")
text("1-66", x = 25.5, y = 89, pos = 2, cex = 1.2, family = "mono")
