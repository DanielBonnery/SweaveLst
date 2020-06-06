#' @param tmpfile file containing tikz code
#' @param scale=c(1,1),
#' @param yxratio=c(1,1),
#' @param caption=NULL,
#' @param label=NULL,
#' @param addfigureenv=FALSE,
#' @param createtexfileinto=NULL,
#' @param createstandalonetexfileinto=NULL,
#' @param createpdffileinto=NULL,
#' @param usepackages=NULL,
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
                          usepackages=NULL,
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
  return(yy)}








#file.remove(tmpfile)

if(!is.null(createstandalonetexfileinto)|
   !is.null(createpdffileinto)|
   !is.null(createtexfileinto)){
  
  yyrda=tempfile()
  save(yy,file=yyrda)}

if(!is.null(createtexfileinto)){
  figonly<-tempfile()
  figonlyrnwfile=paste0(figonly,".rnw")
  figonlytexfile=paste0(figonly,".tex")
  
  file.create(figonlyrnwfile);
  sink(figonlyrnwfile)
  cat(
    paste0('<<echo=FALSE,results=hide>>=
           load("',yyrda,'")
           @
           \\Sexpr{yy}'))
  sink()
  SweaveLst::Sweavelst(fullpath = figonlyrnwfile)
  try(file.remove(createtexfileinto))
  if(!is.null(modify)){
    x<-readLines(figonlytexfile)
    x<-modify(x)
    write(x,figonlytexfile)}
  file.copy(from=figonlytexfile, to=createtexfileinto,overwrite = TRUE)}


if(!is.null(createstandalonetexfileinto)|
   !is.null(createpdffileinto)){
  standalone=tempfile()
  standalonernwfile=paste0(standalone,".rnw")
  standalonetexfile=paste0(standalone,".tex")
  standalonepdffile=paste0(standalone,".pdf")
  
  file.create(standalonernwfile);
  sink(standalonernwfile)
  cat(
    paste0(
      '\\documentclass[10pt]{article}
\\usepackage{tikz}
\\usepackage{SweaveLst}
\\usepackage[active,tightpage,psfixbb]{preview}',
      if(!is.null(usepackages)){usepackages}else{character(0)},
      '
\\PreviewEnvironment{pgfpicture}
\\setlength\\PreviewBorder{0pt}
\\begin{document}
<<echo=FALSE,results=hide>>=
load("',yyrda,'")
@
\\Sexpr{yy}
\\end{document}'))
  sink()
  SweaveLst::Sweavelst(fullpath=standalonernwfile)
  if(!is.null(createstandalonetexfileinto)){
    try(file.remove(createstandalonetexfileinto))
    file.copy(from=standalonetexfile, to=createstandalonetexfileinto,overwrite = TRUE)
  }
  if(!is.null(createpdffileinto)){   
    zz=list.files(tempdir())
    zz<-zz[grep("_ras",zz)]
    for (zzz in zz){file.rename(file.path(tempdir(),zzz),file.path(tempdir(),gsub("_ras","ras",zzz)))}
    x<-readLines(standalonetexfile)
    x<-gsub("_ras","ras",x)
    if(!is.null(modify)){x<-modify(x)}
    write(x,standalonetexfile)
    
    system(paste0("pdflatex ",standalonetexfile))
    try(file.remove(createpdffileinto))
    file.copy(from=standalonepdffile, to=createpdffileinto,overwrite = TRUE)}
}



























