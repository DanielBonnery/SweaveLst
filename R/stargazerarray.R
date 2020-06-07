#'Prints a multidimensional array
#'@param XX
#'@param ...  additional arguments to pass to SweaveLst::stargazer2
stargazerarray <-
function(XX,...){
  tables<-c(apply(XX,3:length(dim(XX)),function(X){stargazer2(X,...)}))
  titles<-""
for (i in 3:length(dim(XX))){  titles=outer(titles, dimnames(XX)[[i]],paste,sep=" ")}
  paste(paste0(c(titles)," \\\\\\\ ",tables),collapse=" \\\\\\\\ ")
}
