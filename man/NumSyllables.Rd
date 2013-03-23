\name{NumSyllables}
\alias{NumSyllables}
\title{
Finds the number of number of syllables in a line.
}
\description{
Takes a line and returns the number of syllables in it.
}
\usage{
NumSyllables(line)
}
\arguments{
  \item{line}{
An character object.
}
}
\value{
Integer representing the number of syllables in the line.
}

\author{
Jireh Tan
}

\seealso{
 \code{\link{GetPronunciations}}
}
\examples{
NumSyllables("But soft? What light through yonder window breaks?")
NumSyllables("To be or not to be? That is the question.")

## The function is currently defined as
function(line) {
  vowels <- c('IH2', 'UW2', 'IH0', 'IH1', 'AH2', 'AH0', 'AH1',
             'UH2', 'UH0', 'UH1', 'UW1', 'EH2', 'EH0', 'EH1', 
             'IY0', 'EY1', 'EY0', 'EY2', 'IY1', 'AY1', 'AY0', 
             'AY2', 'UW0', 'IY2', 'AW2', 'AW1', 'AW0', 'AO2', 
             'AO1', 'AO0', 'OW1', 'OW0', 'OW2', 'AE1', 'AE0', 
             'AE2', 'ER0', 'ER1', 'ER2', 'OY2', 'OY1', 'OY0', 
             'AA1', 'AA0', 'AA2')
  line <- gsub("[\".;!&(),:?]", "", line)
  line <- gsub("-", " ", line)
  words <- unlist(strsplit(line, " "))
  pronunciation <- unlist(lapply(words, function(x) GetPronunciations(x)[[1]]))
  length(pronunciation[pronunciation \%in\% vowels])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ function }