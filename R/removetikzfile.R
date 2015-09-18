removetikzfile <-function(tmpfile,scale=""){
  ww<-readLines(file(tmpfile))
  ww<-ww[!sapply(ww, function(x){any(grep("%",x)==1)})]
  xx<-paste(ww,collapse="\n")
  xx<-paste0("\\rescale{",scale,"}
             ",xx,"\\rescale{1}")
  yy<-gsub('\\','\\\\', xx, fixed=TRUE)
  yy<-gsub('x=1pt,y=1pt',paste0('x=1pt,y=1pt,scale=\\\\tikzscale'), yy, fixed=TRUE)
  file.remove(tmpfile)
  return(yy)}
