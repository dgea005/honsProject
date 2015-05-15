#captions
context("caption errors")
test_that("Caption errors work",{
  data(tli)
  expect_that(xtable(tli[1:10,], caption = c("One", "Two", "Three")), 
              throws_error())
})


context("add to Row validation")
test_that("Check add to row fails",{
  data(tli)
  tli.table <- xtable(tli[1:10,])
  #pos not a list
  expect_that(print(tli.table, add.to.row = list(pos=10, command = c("&", "%^"))),
              throws_error())
  #command not a vector
  expect_that(print(tli.table, add.to.row = list(pos = list(10), command = list(" & "))),
              throws_error())
  #names of list elements wrong
  expect_that(print(tli.table, add.to.row = list(position = list(10), com = c(" & "))),
              throws_error())
  #pos and command different lengths
  expect_that(print(tli.table, add.to.row = list(pos = list(5, 7, 8), command = c(" & "))),
              throws_error())
  #pos numbers are higher than there are rows in data
  expect_that(print(tli.table, add.to.row= list(pos = list(14,15,16), command = c("%","#","^"))),
              throws_error())
  #pos negative
  expect_that(print(tli.table, add.to.row = list(pos = list(-4,4), command = c(" % ", "&"))),
              throws_error())
  #add.to.row argument not a list of length 2
  expect_that(print(tli.table, add.to.row = list(pos = list(3), command = c("&"), hline="s")),
              throws_error())
})



# test functions: addToRowValidate, hlineLocations, assignAddCommands
# from  http://stackoverflow.com/questions/19846796/adding-titles-to-xtable
# by Christopher Louden
test_that("Check add to row works", {
  Grade3 <- c("A","B","B","A","B","C","C","D","A","B","C","C","C","D","B","B","D","C","C","D")
  Grade6 <- c("A","A","A","B","B","B","B","B","C","C","A","C","C","C","D","D","D","D","D","D")
  Cohort <- table(Grade3,Grade6)
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 0
  addtorow$pos[[2]] <- 0
  addtorow$command <- c('& \\multicolumn{4}{c}{Grade 6} \\\\\n', "Grade 3 & A & B & C & D \\\\\n")
  expect_true({capture.output(print(xtable(Cohort, caption = 'My Title'), caption.placement = 'top', 
                                    add.to.row = addtorow, include.colnames = FALSE, comment = FALSE, timestamp = NULL)); TRUE})
})



context("hline after")
#test the hline after being input unsorted; tests hlineLocations
test_that("Check hline.after works and fails",{
  data(tli)
  tli.table <- xtable(tli[1:10,])
  #check that hline order doesn't change anything
  #this also checks that hline after works
  expect_that(capture.output(print(tli.table, hline.after = c(3,5,7))), 
              equals(capture.output(print(tli.table, hline.after = c(5, 3, 7)))))
  #check that negative out of range (-2 or lower) throws error
  expect_that(print(tli.table, hline.after = c(-2,3,5)),
              throws_error())
  #check that positive out of range (greater than max rows) throws error
  expect_that(print(tli.table, hline.after = c(3,5,20)),
              throws_error())
})


context("environment validation")

#tests envirValidate
test_that("Check that the envirValidate fails invalid inputs", {
  data(tli)
  tli.table <- xtable(tli[1:10,])
  #length of type greater than 1
  expect_that(print(tli.table, type = c("latex", "html")), throws_error())
  #type isn't anything meaningful
  expect_that(print(tli.table, type = "wrong type"), throws_error())
  #type is wrong type
  expect_that(print(tli.table, type = 1), throws_error())
  #margintable plus tableplacement
  expect_that(print(tli.table, floating.environment = "margintable", table.placement = "htb"), gives_warning())
  #table.placement wrong letters
  expect_that(print(tli.table, table.placement = "xyz"), throws_error())
  #caption placement wrong name
  tli.table <- xtable(tli[1:10,], caption = "stuff")
  expect_that(print(tli.table, caption.placement = c("side")), throws_error())
})



#tableWidth
test_that("Width warning works", {
  data(tli)
  tli.table <- xtable(tli[1:10,])
  #check that width plus longtable gives warning
  expect_that(print(tli.table, tabular.environment = "longtable", width = 3, floating = FALSE),
              gives_warning())
})

#longCheck
test_that("Long table warning works", {
  data(tli)
  tli.table <- xtable(tli[1:10,])
  #check floating + longtable gives warning
  expect_that(print(tli.table, tabular.environment = "longtable", floating = TRUE),
              gives_warning())
})