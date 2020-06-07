#' get rid of all latex compilation files 
#' @param directory a character string indicating a file path.
nettoie<-function(directory=getwd()){
  file.remove(file.path(directory,unique(unlist(sapply(c("\\.log$",
                       "\\.dvi$",
                       "\\.backup$",
                       "~$",
                       "\\.bbl$",
                       "\\.nav$",
                       "\\.out$",
                       "\\.snm$",
                       "\\.vrb$",
                       "\\.blg$",
                       "\\.toc$",
                       "\\.lot$",
                       "\\.aux$"),function(x){
                         grep(x,list.files(path = directory),value=TRUE)})))))}
  