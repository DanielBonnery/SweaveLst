\name{stargazer2}
\alias{stargazer2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{stargazer2
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stargazer2(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (...) 
{
    X = stargazer(..., header = FALSE, table.placement = "H")
    x <- gsub("\\\\", "\\\\\\\\", paste0(X[-1], collapse = " "))
    x <- gsub("\\\\textbackslash ", "\\\\", x)
    replacement <- paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse", 
        sample(letters, 20, replace = TRUE)), collapse = "")
    x <- gsub("\\\\[$]", replacement, x)
    x <- gsub(paste0("\\\\", replacement), "$", x)
    replacement <- paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse", 
        sample(letters, 20, replace = TRUE)), collapse = "")
    x <- gsub("\\\\[_]", replacement, x)
    x <- gsub(replacement, "_", x)
    replacement <- paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse", 
        sample(letters, 20, replace = TRUE)), collapse = "")
    x <- gsub("\\\\[{]", replacement, x)
    x <- gsub(paste0("\\\\", replacement), "{", x)
    replacement <- paste(c("oooowoieghoihoihwoeghohoihwoeihgohsdfsdfse", 
        sample(letters, 20, replace = TRUE)), collapse = "")
    x <- gsub("\\\\[}]", replacement, x)
    x <- gsub(paste0("\\\\", replacement), "}", x)
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
