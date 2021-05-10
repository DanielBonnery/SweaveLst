#' @param tmpfile file containing tikz code
#' @param scale=c(1,1),
#' @param yxratio=c(1,1),
#' @param caption=NULL,
#' @param label=NULL,
#' @param addfigureenv=FALSE,
#' @param modify=NULL 
#' @examples
#' tmpfile=tempfile()
#' library(tikzDevice);library(ggplot2)
#' tikz(file=tmpfile)
#' print(ggplot(data=cars,aes(x=speed,y=dist))+geom_point())
#' dev.off()  
#' removetikzfile(tmpfile)
#' modify=function(y){
#' gsub("dist","$\\\\\\\\frac{1-\\\\\\\\exp\\\\\\\\left(-\\\\\\\\mathrm(x)^2\\\\\\\\right)}{\\\\\\\\sin(\\\\\\\\mathrm{x})+\\\\\\\\mathds{1}_{\\\\\\\\{0\\\\\\\\}}(\\\\\\\\mathrm{x})}$",y)}
#' grep("dist",removetikzfile(tmpfile,modify=modify))
#' 

removetikzfile <-function(tmpfile,
                          scale=c(1,1),
                          yxratio=c(1,1),
                          caption=NULL,
                          label=NULL,
                          addfigureenv=FALSE,
                          modify=NULL){
  if(length(scale)==1){scale=rep(scale,2)}
  if(length(yxratio)==1){yxratio=rep(yxratio,2)}
  ww<-readLines(file(tmpfile))
  ww<-ww[!sapply(ww, function(x){any(grep("%",x)==1)})]
  xx<-paste(ww,collapse="\n")
  if(any(yxratio!=1)){
  xx<-paste0("\\redeftikzyxratio{",yxratio[1],"}{",yxratio[2],"}
             ",xx,"\\redeftikzyxratio{1}{1}")}
  if(any(scale!=1)){
    xx<-paste0("\\rescale{",scale[1],"}{",scale[2],"}
             ",xx,"\\rescale{1}{1}")}
  yy<-try((function(xx){
    yy<-gsub('\\','\\\\', xx, fixed=TRUE)
    yy<-gsub('x=1pt,y=1pt',paste0('x=1pt,y=1pt,scale=\\\\tikzscale'), yy, fixed=TRUE)
    yy<-gsub('x=1pt,y=1pt','x=1pt,y=\\\\tikzyxratio pt', yy, fixed=TRUE)
    yy<-gsub(';\n', '; ', yy, fixed=TRUE)
    yy<-gsub('--\n', '-- ', yy, fixed=TRUE) 
    yy<-gsub('\n\n', '\n', yy, fixed=TRUE)
    if(addfigureenv|!is.null(caption)|!is.null(label)){
      yy<-paste0("\\\\begin{figure}[H]",
                 if(!is.null(caption)){paste0("\\\\caption{",caption,"}")}else{character(0)},
                 if(!is.null(label)){paste0("\\\\label{",label,"}")}else{character(0)},
                 yy,
                 "\\\\end{figure}")}
    if(!is.null(modify)){
      yy<-modify(yy)}
    yy})(xx))
  if(is.element("try-error",class(yy))){yy<-paste0(xx,yy,Sys.getlocale())}
  return(yy)}







