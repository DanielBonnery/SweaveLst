compile<-function(filename,directory=getwd()){
  nettoie(directory)
  #if(.Platform$OS.type == "unix") {} else {}
  system(command = paste0("
pdflatex '",file.path(directory,filename),".tex' -shell-escape -draftmode -interaction=batchmode;
bibtex -terse '",file.path(directory,filename),".aux';
pdflatex '",file.path(directory,filename),".tex' -shell-escape -interaction=batchmode;
pdflatex '",file.path(directory,filename),".tex' -shell-escape -interaction=batchmode;"),
         intern=FALSE,wait=FALSE,
         ignore.stdout=TRUE, 
         ignore.stderr=TRUE)
  
  system2("pdflatex -shell-escape -draftmode -interaction=nonstopmode ",
          args=
pdflatex '",file.path(directory,filename),".tex' -shell-escape -draftmode -interaction=nonstopmode;
bibtex -terse '",file.path(directory,filename),".aux';
pdflatex '",file.path(directory,filename),".tex' -interaction=nonstopmode;
pdflatex '",file.path(directory,filename),".tex' -interaction=nonstopmode;"),
         wait=FALSE,
         stdout=FALSE, 
         stderr=FALSE)
  
  nettoie(directory)
}
