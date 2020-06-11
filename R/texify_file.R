#' sanitise latex
#' @param fullpath a path to a file
#' @examples
#' temp.file=tempfile() 
#' sink(temp.file)
#' cat("$x=1$
#' ")
#' sink()
#' readLines(temp.file)
#' fullpath=temp.file
texify_file <-
  function(fullpath) {
    folder<-dirname(fullpath)
    file.name<-basename(fullpath)
    x<-readLines(fullpath)
    x<-readtext::readtext(fullpath)
  }
