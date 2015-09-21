graphtikzcode <-
function(texte,widthe=7,heighte=7,scale=c(1,1),yxratio=c(1,1),...){
  par()
  if(!require("tikzDevice")){devtools::install_github("yihui/tikzDevice")}
  require("tikzDevice")
  documentDeclaration<-1
  getDocumentPointsize<-function(a){12}
  tmpfile=paste0(tempfile(),".tex")
  tikz(file=tmpfile, standAlone = FALSE,sanitize=FALSE,width=widthe,height=heighte, footer = getOption("tikzFooter"),
       packages=
         c(options()$tikzLatexPackages,"\\usepackage{amsfonts}","\\usepackage{amssymb}"),...)  
  eval.parent(parse(text=texte),n=1)
  dev.off()  
  return(removetikzfile(tmpfile,scale=scale,yxratio=yxratio))}
