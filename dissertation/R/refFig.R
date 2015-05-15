# segments(x0 = 0.5, x1 = 0.7, y0 = 0.8, y1 = 0.8)
# segments(x0 = 0.7, x1 = 0.7, y0 = 0.8, y1 = 0.85)
# arrows(x0 = 0.7, x1 = 0.8, y0 = 0.85, y1 = 0.85, length = 0.1)
# 
# polygon(x = c(x0 = 0.81, 1.2, 1.2, 0.81), y = c(0.8, 0.8, 0.9, 0.9))
# text(x = 1, y = 0.85, labels = "validation")
# 
# segments(x0 = 0.5, x1 = 0.7, y0 = 0.7, y1 = 0.7)
# segments(x0 = 0.7, x1 = 0.7, y0 = 0.7, y1 = 0.65)
# arrows(x0 = 0.7, x1 = 0.8, y0 = 0.65, y1 = 0.65, length = 0.1)
# 
# polygon(x = c(x0 = 0.81, 1.2, 1.2, 0.81), y = c(0.6, 0.6, 0.7, 0.7))
# text(x = 1, y = 0.65, labels = "preprocessInternal")
# 
# par(op)

#how to make this adjustable -> would need to make the grid lines changeable
vlinesPx = c(0, 0.4)
cntr = (max(vlinesPx) - min(vlinesPx))/ 2

plot.new()
op = par(mar = rep(0.05, 4))
plot.window(xlim = c(0, 1.5), ylim = c(0, 1), xaxs = "i", yaxs = "i", asp = 1)


#main body
# this needs to be based off calculations that can change where the vertical and horizonal(secondary) lines are
polygon(x = c(vlinesPx[1],vlinesPx[1], vlinesPx[2], vlinesPx[2]), y = c(0, 1, 1, 0))
segments(x0 = vlinesPx[1], x1 = vlinesPx[2], y0 = 0.9, y1 = 0.9)
text(x = cntr, y = 0.95, labels = "print.xtable")

segments(x0 = vlinesPx[1], x1 = vlinesPx[2], y0 = 0.6, y1 = 0.6)
text(x = cntr, y = 0.75, labels = "preprocessing")
segments(x0 = vlinesPx[1], x1 = vlinesPx[2], y0 = 0.4, y1 = 0.4)
text(x = cntr, y = 0.5, labels = "build Params")
segments(x0 = vlinesPx[1], x1 = vlinesPx[2], y0 = 0.075, y1 = 0.075)
text(x = cntr, y = 0.25, labels = "make Table")
text(x = cntr, y = 0.0375, labels = "return table")


#arrows

mid = 0.7

segments(x0 = vlinesPx[2], x1 = mid, y0 = 0.8, y1 = 0.8)
segments(x0 = mid, x1 = mid, y0 = 0.8, y1 = 0.85)
arrows(x0 = mid, x1 = 0.8, y0 = 0.85, y1 = 0.85, length = 0.1)

par(op)





#rough outline of original





