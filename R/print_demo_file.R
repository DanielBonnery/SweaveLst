#' Gives the tex code to print a demo code
#'
#'@param topic
#'@param package
#'@examples
#' topic="nlm"
#' package="stats"
#' print_demo_file(topic,package)
#' ## it can be called in the \code{\Sexpr} command in SweaveLst
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
#' topic="nlm"
#' package="stats"
#' x=print_demo_file(topic,package)
#' @
#' 
#' \\Sexpr{x}
#' \\end{document}')
#' sink()
#' SweaveLst::Sweavelst(fullpath = temprnwfile)
#' system(paste0("cd ",dirname(temptexfile),";
#'  lualatex '",basename(temptexfile),"';
#'  bibtex '",basename(gsub(".tex",".aux",temptexfile)),"';"))
#' fs::file_show(temppdffile)

print_demo_file<-function(topic,package){
  paste0(
    "\\\\textbf{Rcode for ",package,"::",topic,"}\n",
    "\\\\begin{lstlisting}[style=Rinput,title={R demo file - package: ",package," - topic: ",topic,"},captionpos=tb]\n",
    paste(readLines(system.file("demo",paste0(topic,".R"),package=package)),
          collapse="\n"),
    "\\\\end{lstlisting}\n")}

