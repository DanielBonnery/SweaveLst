#' Gives the tex code to print a demo code
#'
#'@param topic
#'@param package
#'@example 
#'topic="nlm"
#'package="stats"
#'print_demo_file(topic,package)


print_demo_file<-function(topic,package){
  paste0(
    "\\\\textbf{Rcode for ",package,"::",topic,"}\n",
    "\\\\begin{lstlisting}[style=Rinput,title={demo: ",topic," - package: ",package,"}]\n",
    paste(readLines(system.file("demo",paste0(topic,".R"),package=package)),
          collapse="\n"),
    "\\\\end{lstlisting}\n")}

