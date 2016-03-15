nettoie<-function(directory=getwd()){
  file.remove(file.path(directory,unique(unlist(sapply(c("\\.log$",
                       "\\.dvi$",
                       "\\.backup$",
                       "~$",
                       "\\.bbl$",
                       "\\.blg$",
                       "\\.toc$",
                       "\\.lot$",
                       "\\.aux$"),function(x){
                         grep(x,list.files(path = directory),value=TRUE)})))))}
  