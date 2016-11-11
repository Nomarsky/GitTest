\name{es.divisible}
\alias{es.divisible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Es Dividible
%%  ~~function to do ... ~~
}
\description{Comprueba si dos números son divisibles
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
es.divisible(numero, divisor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numero}{
%%     ~~Describe \code{numero} here~~
}
  \item{divisor}{
%%     ~~Describe \code{divisor} here~~
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
function (numero, divisor) 
{
    if (numero\%\%divisor == 0) 
        return("Es divisible")
    else return("No es divisible")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
