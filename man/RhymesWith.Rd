\name{RhymesWith}
\alias{RhymesWith}

\title{
Takes two lines and decides if they rhyme or not.
}
\description{
Takes two lines and decides if they rhyme or not. If desired, can also determine eye rhymes by setting the \code{eyeRhyme} option to \code{TRUE}.
}
\usage{
RhymesWith(firstLine, secondLine, eyeRhyme = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{firstLine}{
Object of type character.
}
  \item{secondLine}{
Object of type character.
}
  \item{eyeRhyme}{
Logical specifying whether eye rhymes should also be considered rhymes. Defaults to \code{FALSE}.
}
}
\value{
Logical denoting whether the two lines rhyme.
}

\author{
Jireh Tan
}

\seealso{
\code{\link{GetPronunciations}}
}
\examples{
RhymesWith("Bubble, bubble, toil and trouble,", "Fire burn, and cauldron bubble.")
RhymesWith("old", "gold")
RhymesWith("Cool it with a baboon's blood,", "Then the charm is firm and good")
RhymesWith("Cool it with a baboon's blood,", "Then the charm is firm and good", eyeRhyme = TRUE)

## The function is currently defined as
function(firstLine, secondLine, eyeRhyme = FALSE) {
  firstLine <- gsub("[\".;!&(),:?]", "", firstLine)
  firstLine <- gsub("-", " ", firstLine)
  secondLine <- gsub("[\".;!&(),:?]", "", secondLine)
  secondLine <- gsub("-", " ", secondLine)
  firstWords <- unlist(strsplit(firstLine, split = " "))
  secondWords <- unlist(strsplit(secondLine, split = " "))
  firstPronunciations <- lapply(firstWords, function(x) GetPronunciations(x))
  secondPronunciations <- lapply(secondWords, function(x) GetPronunciations(x))
  firstVariants <- expand.grid(firstPronunciations)
  secondVariants <- expand.grid(secondPronunciations)
  firstVariants <- lapply(apply(firstVariants, 1, identity), function(x) unlist(x, use.names = FALSE))
  secondVariants <- lapply(apply(secondVariants, 1, identity), function(x) unlist(x, use.names = FALSE))
  for (x in firstVariants) {
    for (y in secondVariants) {
      if (ARPABETRhyme(x,y))
        return(TRUE)
    }
  }
  if (eyeRhyme) {
    lastWordOfFirst <- firstWords[length(firstWords)]
    lastWordOfSecond <- secondWords[length(secondWords)]
    if (EyeRhyme(lastWordOfFirst,lastWordOfSecond))
      return(TRUE)
  }
  return(FALSE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ function }