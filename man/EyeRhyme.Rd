\name{EyeRhyme}
\alias{EyeRhyme}
\title{
Decides if two words are an eye rhyme or not.
}
\description{
Takes two surface encodings and decides whether they are eye rhymes (also known as visual rhymes).
}
\usage{
EyeRhyme(firstWord,secondWord)
}
\arguments{
  \item{firstWord}{
One surface encoding
}
  \item{secondWord}{
Another surface encoding
}
}
\details{
  Just simple end-of-string matching. It's assumed that words shorter than 3 letters cannot eye rhyme.
}
\value{
Logical specifying whether two surface encodings are eye rhymes.
}
\author{
Jireh Tan
}

\seealso{
\code{\link{MatchMeter}}
}
\examples{
EyeRhyme("old", "gold")

## The function is currently defined as
function(firstWord, secondWord) {
  firstLetters <- unlist(strsplit(firstWord, ""))
  secondLetters <- unlist(strsplit(secondWord, ""))
  firstLength <- length(firstLetters)
  secondLength <- length(secondLetters)
  shorter <- min(firstLength,secondLength)
  if (shorter <= 2)
    return(FALSE)
  if (all(firstLetters[(firstLength-2):firstLength] == secondLetters[(secondLength-2):secondLength]))
    return(TRUE)
  return(FALSE) 
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ function }