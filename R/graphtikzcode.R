#' Reads the output file of the tikz command into an R character string. 
#' @param texte file containing tikz code
#' @param widthe a numeric
#' @param heighte a numeric
#' @param scale=c(1,1) a two parameters scale to apply to the graph
#' @param yxratio=c(1,1), 
#' @param caption a character string.
#' @param label a character string.
#' @param addfigureenv a boolean
#' @param sanitize a booleaan
#' @param standalone a booleaan
#' @param usepackages a character string
#' @param modify a function that takes a character string as a parameter and returns a character string
#' @param ... additional parameters to pass to  tikzDevice::tikz
#' @description 
#' Based on tikzDevice::tikz.
#' @examples
#' ## First example: we generate the tikz code for a graph.
#' library(ggplot2)
#' texte="print(ggplot(data=cars,aes(x=speed,y=dist))+geom_point())"
#' graphtikzcode("print(ggplot(data=cars,aes(x=speed,y=dist))+geom_point())")
#' ## Second example, we create a rnw file 
#' ## This rnw file will be interpretated by Sweave and will print the 
#' ## tikz dode of the plot into the corresponding tex file. 
#' 
#' figonlyrnwfile<-tempfile(fileext = ".rnw")
#' file.create(figonlyrnwfile);
#' sink(figonlyrnwfile)
#' cat(
#' '\\Sexpr{graphtikzcode("print(ggplot(data=cars,aes(x=speed,y=dist))+geom_point())")}
#' ')
#' sink()
#' SweaveLst::Sweavelst(fullpath = figonlyrnwfile)
#' readLines(gsub(".rnw",".tex",figonlyrnwfile))
#' 
#' library(ggplot2)
#' figureX<-function(){
#' figureXX<-ggplot(data=cars,aes(x=speed,y=dist))+geom_point()
#' x=graphtikzcode("print(figureXX)")
#' graph2texfile("print(figureXX)",file.path(tempdir(),"figureX.tex"))
#' graph2pdffile("print(figureXX)",file.path(tempdir(),"figureX.pdf"))
#' }
#' figureX()
#' fs::file_show(file.path(tempdir(),"figureX.pdf"))
graphtikzcode <-
function(texte,
         widthe=7,
         heighte=7,
         scale=c(1,1),
         yxratio=c(1,1),
         caption=NULL,
         label=NULL,
         addfigureenv=FALSE,
         sanitize=FALSE,
         modify=NULL,
         addtopreamble=character(0),
         standalone=FALSE,
         ...){
  par()
  if(!require("tikzDevice")){devtools::install_github("yihui/tikzDevice")}
  require("tikzDevice")
  documentDeclaration<-1
  getDocumentPointsize<-function(a){12}
  tmpfile=paste0(tempfile(),".tex")
  tikz(file=tmpfile, standAlone = standalone,sanitize=sanitize,width=widthe,height=heighte, footer = getOption("tikzFooter"),
       packages=
         c(options()$tikzLatexPackages,
           "\\usepackage{amsfonts}",
           "\\usepackage{amssymb}",
           "\\usepackage{amsthm,amsmath}",
           "\\usepackage{mathrsfs}"       ,           
           "\\usepackage{amssymb}" ,
           "\\usepackage{MnSymbol}",
           "\\usepackage{dsfont}",
           if(standalone){c("\\usepackage{float}",
             "\\usepackage{SweaveLst}")}else{character(0)},
           addtopreamble),...)  
  xx=try(eval.parent(parse(text=texte),n=1))
  if(is.element("try-error",class(xx))){
    xx=try(eval.parent(parse(text=texte),n=2))
  }
  if(is.element("try-error",class(xx))){
    xx=try(eval.parent(parse(text=texte),n=3))
  }
  dev.off()  
  return(removetikzfile(tmpfile,modify=modify,scale=scale,yxratio=yxratio,caption=caption,label=label,addfigureenv=addfigureenv))}

#' Modifies the output of the  tikz command and copies it to a tex file.
#' @param texte file containing tikz code
#' @param widthe a numeric
#' @param heighte a numeric
#' @param scale=c(1,1) a two parameters scale to apply to the graph
#' @param yxratio=c(1,1), 
#' @param caption a character string.
#' @param label a character string.
#' @param addfigureenv a boolean
#' @param sanitize a booleaan
#' @param standalone a booleaan
#' @param usepackages a character string
#' @param modify a function that takes a character string as a parameter and returns a character string
#' @param ... additional parameters to pass to  tikzDevice::tikz
#' @description 
#' Based on tikzDevice::tikz.
#' @examples
#' ## First example: we generate the tikz code for a graph. 
#' 
#' outputtexfile<-tempfile(fileext = ".tex")
#' graph2texfile(
#' "print(ggplot2::ggplot(data=cars,ggplot2::aes(x=speed,y=dist))+
#'        ggplot2::geom_point())",
#'   output=outputtexfile)
#' readLines(outputtexfile)
#' graph2texfile(
#' "print(ggplot2::ggplot(data=cars,ggplot2::aes(x=speed,y=dist))+
#'        ggplot2::geom_point())",
#'   standalone=TRUE,
#'   output=outputtexfile,
#'   modify=function(y){
#' gsub("dist","$\\\\\\\\left(1-\\\\\\\\exp\\\\\\\\left(-\\\\\\\\mathrm(x)^2\\\\\\\\right)\left(\\\\\\\\sin(\\\\\\\\mathrm{x})+\\\\\\\\mathds{1}_{\\\\\\\\{0\\\\\\\\}}(\\\\\\\\mathrm{x})\\\\\\\\right)$",y)})
#' readLines(outputtexfile)
#' system(paste0("cd ",dirname(outputtexfile),"; lualatex '",basename(outputtexfile),"';"))
#' fs::file_show(gsub(".tex",".pdf",outputtexfile))
graph2texfile <-
  function(texte,
           output=tempfile(fileext = ".tex"),
           modify=NULL,
           widthe=7,
           heighte=7,
           caption=NULL,
           label=NULL,
           addfigureenv=FALSE,
           sanitize=FALSE,
           standalone=FALSE,
           addtopreamble=NULL,
           ...){
    
    yy<-graphtikzcode(texte=texte,
               widthe=widthe,
               heighte=heighte,
               caption=caption,
               label=label,
               modify=modify,
               addfigureenv=addfigureenv,
               sanitize=sanitize,
               standalone=standalone,
               addtopreamble=addtopreamble,
               ...)
    
    tempbasename<-tempfile()
    temprdafile=paste0(tempbasename,".rda")
    save(yy,file=temprdafile)
    tempbasename<-tempfile()
    temprnwfile=paste0(tempbasename,".rnw")
    temptexfile=paste0(tempbasename,".tex")
      
      file.create(temprnwfile);
      sink(temprnwfile)
      cat(
        paste0(
'<<echo=FALSE,results=hide,split=FALSE>>=
load("',temprdafile,'")
@

\\Sexpr{yy}'))
      sink()
      SweaveLst::Sweavelst(fullpath = temprnwfile)
      file.copy(from=temptexfile, to=output,overwrite = TRUE)
        return(output)}

#' Creates a pdf file by converting a graph to tikz and lualatexing the output
#' @param texte file containing tikz code
#' @param output output fill path (will be overwritten if existing with no warning) 
#' @param widthe a numeric
#' @param heighte a numeric
#' @param caption a character string.
#' @param label a character string.
#' @param addfigureenv a boolean
#' @param sanitize a booleaan
#' @param usepackages a character string
#' @param modify a function that takes a character string as a parameter and returns a character string
#' @param ... additional parameters to pass to  tikzDevice::tikz
#' @description 
#' Based on tikzDevice::tikz.
#' @examples
#' ## First example: we generate the tikz code for a graph. 
#' 
#' outputpdffile<-tempfile(fileext = ".pdf")
#' command="print(ggplot2::ggplot(data=cars,ggplot2::aes(x=speed,y=dist))+ggplot2::geom_point())"
#' graph2pdffile(command,output=outputpdffile)
#' readLines(outputpdffile)
#' fs::file_show(outputpdffile)
#' graph2pdffile(command,output=outputpdffile,widthe=7,heighte=3)
#' fs::file_show(outputpdffile)
#' command="print(ggplot2::ggplot(data=cars,ggplot2::aes(x=speed,y=dist,color=dist))+
#' ggplot2::geom_point())"
#' fs::file_show(graph2pdffile(command,widthe=7,heighte=3,modify=function(y){
#' gsub("dist","$\\frac{1-\\exp\\left(-\\mathrm(x)^2\\right)}{\\sin(\\mathrm{x})+\\mathds{1}_{\\{0\\}}(\\mathrm{x})}$",y)}))

graph2pdffile <-
  function(texte,
           output=tempfile(fileext = ".pdf"),
           widthe=7,
           heighte=7,
           caption=NULL,
           label=NULL,
           addfigureenv=FALSE,
           sanitize=FALSE,
           modify=NULL,
           addtopreamble=NULL,
           ...){
    tempbasename<-tempfile()
    temptexfile<-paste0(tempbasename,".tex")
    temppdffile<-paste0(tempbasename,".pdf")
    graph2texfile(texte,
               output=temptexfile,
               modify=modify,
               widthe=widthe,
               heighte=heighte,
               caption=caption,
               label=label,
               addfigureenv=addfigureenv,
               sanitize=sanitize,
               standalone=TRUE,
               addtopreamble=addtopreamble,
               ...)
    try(file.copy(system.file("extdata/SweaveLst.sty",package="SweaveLst"),dirname(temptexfile)))
    system(paste0("cd ",dirname(temptexfile),"; lualatex '",basename(temptexfile),"';"))
    try(file.remove(file.path(dirname(temptexfile),"SweaveLst.sty")))
    file.copy(from=temppdffile, to=output,overwrite = TRUE)
    return(output)}





#' Creates a png file by converting a graph to tikz and lualatexing the output
#' @param texte file containing tikz code
#' @param output output fill path (will be overwritten if existing with no warning) 
#' @param widthe a numeric
#' @param heighte a numeric
#' @param caption a character string.
#' @param label a character string.
#' @param addfigureenv a boolean
#' @param sanitize a booleaan
#' @param usepackages a character string
#' @param modify a function that takes a character string as a parameter and returns a character string
#' @param ... additional parameters to pass to  tikzDevice::tikz
#' @description 
#' Based on tikzDevice::tikz.
#' @examples
#' ## First example: we generate the tikz code for a graph. 
#' 
#' outputpngfile<-tempfile(fileext = ".png")
#' command="print(ggplot2::ggplot(data=cars,ggplot2::aes(x=speed,y=dist))+ggplot2::geom_point())"
#' graph2pngfile(command,output=outputpngfile)
#' readLines(outputpngfile)
#' fs::file_show(outputpngfile)
#' graph2pngfile(command,output=outputpngfile,widthe=7,heighte=3)
#' fs::file_show(outputpngfile)
#' command="print(ggplot2::ggplot(data=cars,ggplot2::aes(x=speed,y=dist,color=dist))+
#' ggplot2::geom_point())"
#' fs::file_show(graph2pngfile(command,widthe=7,heighte=3,modify=function(y){
#' gsub("dist","$\\frac{1-\\exp\\left(-\\mathrm(x)^2\\right)}{\\sin(\\mathrm{x})+\\mathds{1}_{\\{0\\}}(\\mathrm{x})}$",y)}))
#' convert a print (graph) expression to a png file.
graph2pngfile <-
  function(texte,
           output=tempfile(fileext = ".png"),
           widthe=7,
           heighte=7,
           caption=NULL,
           label=NULL,
           addfigureenv=FALSE,
           sanitize=FALSE,
           modify=NULL,
           addtopreamble=NULL,
           ...){
    tempbasename<-tempfile()
    temppdffile<-paste0(tempbasename,".pdf")
    temppngfile<-paste0(tempbasename,".png")
    graph2pdffile(texte,
                  output=temppdffile,
                  modify=modify,
                  widthe=widthe,
                  heighte=heighte,
                  caption=caption,
                  label=label,
                  addfigureenv=addfigureenv,
                  sanitize=sanitize,
                  addtopreamble=addtopreamble,
                  ...)
    pdftools::pdf_convert(temppdffile,"png",dpi = 1200,filenames = temppngfile)
    file.copy(from=temppngfile, to=output,overwrite = TRUE)
    return(output)}
