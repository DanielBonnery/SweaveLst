#'Prints a multidimensional array
#'@param XX
#'@param ...  additional arguments to pass to SweaveLst::stargazer2
#'@example
#' ## the output of stargazer2 is different from stargazer output:
#' stargazer2(cars,summary=FALSE)
#' stargazer(cars,summary=FALSE)
#' ## it can be called in the \code{\Sexpr} command in sweave
#' tempbasename<-tempfile()
#' temprnwfile=paste0(tempbasename,".rnw")
#' temptexfile=paste0(tempbasename,".tex")
#' temppdffile=paste0(tempbasename,".pdf")
#' file.create(temprnwfile);
#' sink(temprnwfile)
#' cat(
#' '\\documentclass[12pt]{article}
#' \\usepackage{SweaveLst}
#' \\usepackage{float}
#' \\usepackage{amsmath}
#' \\begin{document}
#' <<results=hide>>=
#' x=stargazer2(cars[1:10,],summary=FALSE)
#' y=stargazer2(cars[1:10,],summary=FALSE,rownames=FALSE,title="speed and distance",
#' label="fig:1")
#' @
#' 
#' \\Sexpr{x}
#' 
#' Table \\ref{fig:1} represents distance vs speed.
#' 
#' \\Sexpr{y}
#' \\end{document}')
#' sink()
#' SweaveLst::Sweavelst(fullpath = temprnwfile)
#' system(paste0("cd ",dirname(temptexfile),";
#'  lualatex '",basename(temptexfile),"';
#'  bibtex '",basename(gsub(".tex",".aux",temptexfile)),"';
#'  lualatex '",basename(temptexfile),"';"))
#' fs::file_show(temppdffile)
stargazer2 <-
  function(...){
    X=stargazer::stargazer(...,header=FALSE,table.placement = "H")
    YY<-list(...)
    x<-gsub("\\\\","\\\\\\\\",paste0(X[-1],collapse=" "))
    x<-gsub("\\\\textbackslash ","\\\\",x)
    replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
    x<-gsub("\\\\[$]",replacement,x)
    x<-gsub(paste0("\\\\",replacement),"$",x)
    replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
    x<-gsub("\\\\\\\\[_]",replacement,x)
    x<-gsub(replacement,"_",x)
    x<-gsub("\\\\[_]",replacement,x)
    x<-gsub(replacement,"_",x)
    replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
    x<-gsub("\\\\[{]",replacement,x)
    x<-gsub(paste0("\\\\",replacement),"{",x)
    replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
    x<-gsub("\\\\[}]",replacement,x)
    x<-gsub(paste0("\\\\",replacement),"}",x)
    replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
    x<-gsub("$\\\\hat[{]\\\\mkern6mu[]]}$",replacement,x)
    x<-gsub("[$]\\\\\\\\hat[{]\\\\\\\\mkern6mu[}][$]",replacement,x)
    x<-gsub(replacement,"^",x)
    
    if(!is.null(YY$out)){
      z<-gsub("\\\\",'\\',x,fixed=T)
      writeLines(text=z,YY$out)
     }
    x
  }
