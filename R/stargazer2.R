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
