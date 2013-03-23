\name{StressPattern}
\alias{StressPattern}
\title{
Takes a line and returns its stress pattern, a sequence of "0"s (unstressed syllables) and "1"s
}
\description{
Takes a line and returns its stress pattern, a sequence of "0"s (unstressed syllables) and "1"s
}
\usage{
StressPattern(line)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{line}{
Object of type character.
}
}

\value{
Object of type character consisting of "0"s and "1"s.
}\author{
Jireh Tan
}
\seealso{
\code{\link{GetPronunciations}}
}
\examples{
StressPattern("To be or not to be, that is the question.")
StressPattern("I wandered lonely as a cloud")

## The function is currently defined as
function(line) {
  vowels <- c('IH2', 'UW2', 'IH0', 'IH1', 'AH2', 'AH0', 'AH1',
             'UH2', 'UH0', 'UH1', 'UW1', 'EH2', 'EH0', 'EH1', 
             'IY0', 'EY1', 'EY0', 'EY2', 'IY1', 'AY1', 'AY0', 
             'AY2', 'UW0', 'IY2', 'AW2', 'AW1', 'AW0', 'AO2', 
             'AO1', 'AO0', 'OW1', 'OW0', 'OW2', 'AE1', 'AE0', 
             'AE2', 'ER0', 'ER1', 'ER2', 'OY2', 'OY1', 'OY0', 
             'AA1', 'AA0', 'AA2')
  stressed_vowels <- c('IH2', 'UW2', 'UW1', 'IH1',
                      'AH2', 'AH1', 'UH2', 'UH1', 
                      'EH2', 'EH1', 'EY1', 'EY2', 
                      'IY1', 'AY1', 'AY2', 'IY2', 
                      'AW2', 'AW1', 'AO2', 'AO1', 
                      'OW1', 'OW2', 'AE1', 'AE2', 
                      'ER1', 'ER2', 'OY2', 'OY1', 
                      'AA1', 'AA2')
  line <- gsub("[\".;!&(),:?]", "", line)
  line <- gsub("-", " ", line)
  words <- unlist(strsplit(line, " "))
  pronunciation <- unlist(lapply(words, function(x) GetPronunciations(x)[[1]]))
  pronunciation <- pronunciation[pronunciation \%in\% vowels]
  paste(unlist(lapply(pronunciation, function(x) ifelse(x \%in\% stressed_vowels, 1, 0))), collapse = "")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ function }