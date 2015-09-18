dropboxfolder<-function(){
  if(file.exists("~/Dropbox/")){"~/Dropbox/"}
  else{if(file.exists("~/../Dropbox/")){"~/../Dropbox/"}}
}