Sweavelst <-
function(file=NULL,
                    path=getwd(),
                    fullpath=NULL,
                    out.width=10,
                    width=50,
                    height=10,
                    prompte="  "){
  options(continue="   ",
          out.width=out.width,
          width=width,
          height=height,
          prompt=prompte)
  if(is.null(fullpath)){fullpath<-paste(path,file,sep="/")}
  outfile=gsub(".rnw",".tex",fullpath)
  Sweave(fullpath,out.width=10,width=50,keep.source=TRUE,encoding="utf8",out=outfile)
  yarep <- readLines(outfile)
  if(length(yarep)>0){yarep<-yarep[!is.element(seq_len(length(yarep)),grep("Schunk",yarep))]}
  if(length(yarep)>0){
    vide<-sapply(yarep,function(l){all(is.element(unique(strsplit(paste0(l," "),"")),c(" ")))})
    yarep[vide]<-""
    suppr<-seq_len(length(yarep))[vide&c(FALSE,vide[-length(yarep)])]
    if(length(suppr)>0){yarep<-yarep[-suppr]}
    Sput<-grep("Sinput|Soutput",yarep)
    vide<-seq_len(length(yarep))[yarep==""]
    suppr<-union((Sput+1)[is.element(Sput+1,vide)],
                 (Sput-1)[is.element(Sput-1,vide)])
    if(length(suppr)>0){yarep<-yarep[-suppr]}
    Sinput<-grep("Sinput",yarep)
    suppr<-Sinput[is.element(Sinput,Sinput+1)|is.element(Sinput,Sinput-1)]
    if(length(suppr)>0){yarep<-yarep[-suppr]}
    Soutput<-grep("Soutput",yarep)
    suppr<-Soutput[is.element(Soutput,Soutput+1)|is.element(Soutput,Soutput-1)]
    if(length(suppr)>0){yarep<-yarep[-suppr]}
    yarep <- gsub( "\\\\usepackage\\{Sweave\\}", "", yarep )
    yarep <- gsub( "begin\\{Sinput\\}", "begin{lstlisting}[style=Rinput]", yarep )
    yarep <- gsub( "begin\\{Soutput\\}", "begin{lstlisting}[style=Routput]", yarep )
    yarep <- gsub( "\\{Sinput", "{lstlisting", yarep )
    yarep <- gsub( "\\{Soutput", "{lstlisting", yarep )
    if(FALSE){
      yarep <- gsub( "0.00,0.00,0.00", "remlacecatoutusuitee", yarep )
      yarep <- gsub( "1.00,1.00,1.00", "0.00,0.00,0.00", yarep )
      yarep <- gsub( "remlacecatoutusuitee","1.00,1.00,1.00", yarep )}
    yarep <- gsub( "‘", "'", yarep )
    yarep <- gsub( "‘", "'", yarep )
    yarep <- gsub( "’", "'", yarep )
    yarep[yarep=="\\end{lstlisting}"]<-"\\end{lstlisting}\n"
   # yarep <- gsub( "3.3333333333333333", "\\textwidth", yarep )
    cat(yarep, file=outfile, sep="\n")}}
