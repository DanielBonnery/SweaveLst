compile<-function(filename,directory=getwd()){
  nettoie(directory)
  #if(.Platform$OS.type == "unix") {} else {}
  system(command = paste0("
pdflatex '",file.path(directory,filename),".tex' -shell-escape -interaction=batchmode;
bibtex -terse '",file.path(directory,filename),".aux';
pdflatex '",file.path(directory,filename),".tex' -shell-escape -interaction=batchmode;
pdflatex '",file.path(directory,filename),".tex' -shell-escape -interaction=batchmode;"),
         intern=FALSE,wait=FALSE,
         ignore.stdout=TRUE, 
         ignore.stderr=TRUE)
  
  nettoie(directory)
}
