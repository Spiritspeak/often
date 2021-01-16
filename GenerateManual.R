#Generate help files
devtools::document()
#Build manual
devtools::build_manual(path=".")
#Check package
devtools::check(args="--as-cran")
#devtools::check_win_devel()

#build package
#devtools::build()

flist<-list.files(all.files=T)
if(length(flist)>0){
  flist<- flist[grep("\\.Rd2.*",x=flist)]
  unlink(flist,recursive=T)
}

