compile<-function(filename,directory=getwd()){
  nettoie(directory)
  #if(.Platform$OS.type == "unix") {} else {}
  system(paste0("pdflatex '",file.path(directory,filename),".tex' -shell-escape -draftmode -interaction=nonstopmode;"),
         show.output.on.console = FALSE,wait=FALSE,ignore.stdout=TRUE, ignore.stderr=TRUE)
  system(paste0("bibtex -terse '",file.path(directory,filename),".aux'"),
         show.output.on.console = FALSE,wait=FALSE,ignore.stdout=TRUE, ignore.stderr=TRUE)
  system(paste0("pdflatex '",file.path(directory,filename),".tex' -interaction=nonstopmode;")
         show.output.on.console = FALSE,wait=FALSE,ignore.stdout=TRUE, ignore.stderr=TRUE)
  system(paste0("pdflatex '",file.path(directory,filename),".tex' -interaction=nonstopmode;"),
         show.output.on.console = FALSE,wait=FALSE,ignore.stdout=TRUE, ignore.stderr=TRUE)
  nettoie(directory)
}
