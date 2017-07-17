removetikzfile <-function(tmpfile,scale=c(1,1),yxratio=c(1,1),caption=NULL,label=NULL,addfigureenv=FALSE,createtexfileinto=NULL){
  if(length(scale)==1){scale=rep(scale,2)}
  if(length(yxratio)==1){yxratio=rep(yxratio,2)}
  ww<-readLines(file(tmpfile))
  ww<-ww[!sapply(ww, function(x){any(grep("%",x)==1)})]
  xx<-paste(ww,collapse="\n")  
  xx<-paste0("\\redeftikzyxratio{",yxratio[1],"}{",yxratio[2],"}
             ",xx,"\\redeftikzyxratio{1}{1}")
  xx<-paste0("\\rescale{",scale[1],"}{",scale[2],"}
             ",xx,"\\rescale{1}{1}")
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
  file.remove(tmpfile)
  if(!is.null(createtexfileinto)){
    try(file.remove(paste0(createtexfileinto,".rnw")))
    file.create(paste0(createtexfileinto,".rnw"));
    yyrda=tempfile()
    save(yy,file=yyrda)
    sink(paste0(createtexfileinto,".rnw"))
    cat(
paste0('<<echo=FALSE,results=hide>>
load("',yyrda,'")
@
\\Sexpr{yy}'))
    sink()
    SweaveLst::Sweavelst(paste0(createtexfileinto,".rnw"))
  }
  return(yy)}
