stargazer2 <-
function(...){
  X=stargazer(...,header=FALSE,table.placement = "H")
  x<-gsub("\\\\","\\\\\\\\",paste0(X[-1],collapse=" "))
  x<-gsub("\\\\textbackslash ","\\\\",x)
  replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
  x<-gsub("\\\\[$]",replacement,x)
  x<-gsub(paste0("\\\\",replacement),"$",x)
  replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
  x<-gsub("\\\\[_]",replacement,x)
  x<-gsub(replacement,"_",x)
  replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
  x<-gsub("\\\\[{]",replacement,x)
  x<-gsub(paste0("\\\\",replacement),"{",x)
  replacement<-paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse",sample(letters,20,replace=TRUE)),collapse="")
  x<-gsub("\\\\[}]",replacement,x)
  x<-gsub(paste0("\\\\",replacement),"}",x)
  x
}
