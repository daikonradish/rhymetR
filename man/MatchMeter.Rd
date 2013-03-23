\name{MatchMeter}
\alias{MatchMeter}
\title{
Measures how far a line is from a specified meter.
}
\description{
A meter consists of a metrical pattern and the number of feet, e.g. the prevalent iambic pentameter consists of the iamb (unstressed, stressed) repeated five times. Unstressed syllables are represented with 0, stressed syllables are represented with 1, so the iambic pentameter is represented as "0101010101". Given a meter and a line, this function returns the minimum edit distance required to convert that line into the meter. The smaller the returned value, the closer it is to the line. On average, each line of Shakespeare's sonnets is about 4 edits away from iambic pentameter.
}
\usage{
MatchMeter(line, numfeet = 5, meter = c("iamb", "trochee", "amphibrach", "dactyl", "anapest", "spondee", "pyrrhic"))
}
\arguments{
  \item{line}{
Character, the line to be analyzed.
}
  \item{numfeet}{
The number of times the metrical pattern is repeated: 1 for monometer, 2 for dimeter, 3 for trimeter, 4 for tetrameter, 5 for pentameter, 6 for sexameter and so on and so forth.
}
  \item{meter}{
Character, one of "iamb", "trochee", "amphibrach", "dactyl", "anapest", "spondee", "pyrrhic". Currently only these metrical patterns are supported.
}
}

\value{
Integer denoting the edit distance to a particular meter.
}

\author{
Jireh Tan
}


\seealso{
\code{\link{StressPattern}}
}
\examples{
MatchMeter("But soft! What light through yonder window breaks?", 5, "iamb")
MatchMeter("It is the east, and Juliet is the sun.", 5, "iamb")

## The function is currently defined as
function (line, numfeet = 5, meter = c("iamb", "trochee", "amphibrach", 
    "dactyl", "anapest", "spondee", "pyrrhic")) 
{
    patterns <- list(iamb = "01", trochee = "10", dactyl = "100", 
        anapest = "001", spondee = "11", pyrrhic = "00", amphibrach = "010")
    vowels <- c("IH2", "UW2", "IH0", "IH1", "AH2", "AH0", "AH1", 
        "UH2", "UH0", "UH1", "UW1", "EH2", "EH0", "EH1", "IY0", 
        "EY1", "EY0", "EY2", "IY1", "AY1", "AY0", "AY2", "UW0", 
        "IY2", "AW2", "AW1", "AW0", "AO2", "AO1", "AO0", "OW1", 
        "OW0", "OW2", "AE1", "AE0", "AE2", "ER0", "ER1", "ER2", 
        "OY2", "OY1", "OY0", "AA1", "AA0", "AA2")
    stressed_vowels <- c("IH2", "UW2", "UW1", "IH1", "AH2", "AH1", 
        "UH2", "UH1", "EH2", "EH1", "EY1", "EY2", "IY1", "AY1", 
        "AY2", "IY2", "AW2", "AW1", "AO2", "AO1", "OW1", "OW2", 
        "AE1", "AE2", "ER1", "ER2", "OY2", "OY1", "AA1", "AA2")
    meter <- match.arg(meter)
    idealPattern <- paste(rep(patterns[[meter]], numfeet), collapse = "")
    actualPattern <- StressPattern(line)
    adist(idealPattern, actualPattern, costs = list(insertions = 2, 
        deletions = 2, substitutions = 1))[1, 1]
  }
}

\keyword{ function }