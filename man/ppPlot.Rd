\name{ppPlot}
\alias{ppPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
P-P Plot for One Continuous Variable
}
\description{
Function will take in a data frame and return a P-P plot of the cumulative probability distribution (y) on the hypothetical cumulative probability distribution (x)
}
\usage{
ppPlot(df, name.var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
     data.frame(no NAs in selected variable)
}
  \item{name.var}{
     c("selected column name as character string")
}
}
\details{
  If necessary, more details than the description above
}
\value{
Returns a P-P Plot, for the same purpose as P-P plot in SPSS.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
 ~put references to the literature/web site here ~
}
\author{
  Paul E. Plonski
}
\note{
  First Created: 12 February 2021
  Modified: 12 February 2021
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
function (df, name.var)
{
    df <- data.frame(rnorm(100)^2)
    name.var <- c("rnorm.100..2")
    n = nrow(df)
    p <- (1:n)/n - 0.5/n
    pp <- ggplot(df) + geom_point(aes(x = p, y = sort(pnorm(.data[[name.var]],
        mean(.data[[name.var]]), sd(.data[[name.var]]))))) +
        labs(x = "Hypothetical Probability", y = "Probability",
            title = name.var) + geom_abline() + coord_cartesian(xlim = c(0,
        1), ylim = c(0, 1)) + theme_tufte(base_size = 14, base_family = "sans",
        ticks = FALSE)
    pp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ ~P-P }
\keyword{ ~Probability }
\keyword{ ~Plot }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
