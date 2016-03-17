compile<-function(filename,directory=getwd()){
  nettoie(directory)
  #if(.Platform$OS.type == "unix") {} else {}
    system(paste0(
      "cd '",directory,"';
      pdflatex '",filename,".tex' -shell-escape -draftmode -interaction=nonstopmode; 
      bibtex -terse '",filename,".aux';
      pdflatex '",filename,".tex' -interaction=nonstopmode;
      pdflatex '",filename,".tex' -interaction=nonstopmode;}  
    "),wait=FALSE,ignore.stdout=TRUE, ignore.stderr=TRUE)
  nettoie(directory)
}
