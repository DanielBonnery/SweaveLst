\name{stargazerarray}
\alias{stargazerarray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{stargazerarray
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stargazerarray(XX, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
%%     ~~Describe \code{XX} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (XX, ...) 
{
    tables <- c(apply(XX, 3:length(dim(XX)), function(X) {
        stargazer2(X, ...)
    }))
    titles <- ""
    for (i in 3:length(dim(XX))) {
        titles = outer(titles, dimnames(XX)[[i]], paste, sep = " ")
    }
    paste(paste0(c(titles), " \\\\\\ ", tables), collapse = " \\\\\\\\ ")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
