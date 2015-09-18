compT <-
function(file,path="./"){
  system(paste0("pdflatex -interaction=nonstopmode ",file.path(path,file)))
}
