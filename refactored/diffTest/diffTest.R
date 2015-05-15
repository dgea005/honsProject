## if there is already a xtableGalleryOriginal.tex then firstsetup (commented out section) doesn't need to be run
### first setup
# setwd(#set to folder containing diff test)
# #need to remake the original tex file (only sweave/knitr included)
# install.packages("xtable_1.7-3.tar.gz", type="source",repos=NULL) ## version refactored from
# library(xtable)
# Sweave(file = "xtableGallery.snw")
## get rid of unrequired file
# file.remove("C:/Users/Daniel/Dropbox/Uni/Honours/Stats 781a - Project/week 17/NewVignette/xtableGallery-concordance.tex")
# file.rename("xtableGallery.tex", "xtableGalleryOriginal.tex")
# detach("package:xtable", unload = TRUE)



diffTest <- function(){
  setwd(#set to folder containing diff test)
  out <- capture.output(Sweave(file = "xtableGallery.snw"))
  out1 <- file.remove("xtableGallery-concordance.tex")
  out2 <- file.rename("xtableGallery.tex", "xtableGalleryNew.tex")
  ## need to locate diff utility; replace 'C:/cygwin...' if different
  system('C:/cygwin/bin/diff.exe xtableGalleryNew.tex xtableGalleryOriginal.tex')
}



diffTest()