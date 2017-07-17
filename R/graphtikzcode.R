graphtikzcode <-
function(texte,widthe=7,heighte=7,scale=c(1,1),yxratio=c(1,1),caption=NULL,label=NULL,addfigureenv=FALSE,sanitize=FALSE,createtexfileinto=NULL,...){
  par()
  if(!require("tikzDevice")){devtools::install_github("yihui/tikzDevice")}
  require("tikzDevice")
  documentDeclaration<-1
  getDocumentPointsize<-function(a){12}
  tmpfile=paste0(tempfile(),".tex")
  tikz(file=tmpfile, standAlone = FALSE,sanitize=sanitize,width=widthe,height=heighte, footer = getOption("tikzFooter"),
       packages=
         c(options()$tikzLatexPackages,"\\usepackage{amsfonts}","\\usepackage{amssymb}"),...)  
  eval.parent(parse(text=texte),n=1)
  dev.off()  
  return(removetikzfile(tmpfile,scale=scale,yxratio=yxratio,caption=caption,label=label,addfigureenv=addfigureenv,createtexfileinto=createtexfileinto))}
