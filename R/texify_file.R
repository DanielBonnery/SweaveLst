#' sanitise latex
#' @param fullpath a path to a file
#' @examples
#' temp.file=tempfile() 
#' sink(temp.file)
#' cat("$x=1$
#' ```{r}
#' x=list(x=1)
#' x$x
#' ```
#' `r x$x`
#' ")
#' sink()
#' readLines(temp.file)
#' fullpath=temp.file
texify_mdfile <-
  function(input,output) {
    folder<-dirname(input)
    file.name<-basename(output)
    file.copy(input,tempdir())
    tempoutput<-tempfile()
    system(paste0(
"cd ",tempdir(),";
python3 -m readme2tex --output ",tempoutput," ",input,";
sed -i 's+https://rawgit.com/in	git@github.com:DanielBonnery/CompositeRegressionEstimation/None/++g' ",tempoutput))}