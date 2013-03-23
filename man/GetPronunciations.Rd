\name{GetPronunciations}
\alias{GetPronunciations}
\title{
Takes the surface encoding of a word and returns its ARPABET encoding(s).
}
\description{
Given the surface encoding (spelling) of a word, does a lookup in the CMU Pronouncing Dictionary and returns a list of the pronunciation(s) in ARPABET format. Note that there can be several pronunciations for a given word. If the dictionary doesn't contain the word, some prefix and suffix matching is performed. If nothing is found, produces an error. 
}
\usage{
GetPronunciations(surface)
}
\arguments{
  \item{surface}{
One word and one word only. Should be an object of type character, no spaces.
}
}
\value{
List of ARPABET encodings.
}
\references{
For information on the CMU Pronouncing Dictionary, please refer to http://www.speech.cs.cmu.edu/cgi-bin/cmudict
}
\author{
Jireh Tan
}
\examples{
GetPronunciations("bass")
GetPronunciations("dandruff")
## The function is currently defined as
function (surface) 
{
    surface <- toupper(surface)
    pronunciations <- CMUDict[[surface]]
    if (!is.null(pronunciations)) {
        return(pronunciations)
    }
    else {
        if (substr(surface, 1, 2) == "UN") {
            substr <- substring(surface, 3, nchar(surface))
            pronunciations <- GetPronunciations(substr)
            for (i in seq_along(pronunciations)) {
                pronunciations[[i]] <- c(c("AH0", "N"), pronunciations[[i]])
            }
            return(pronunciations)
        }
        else if (substr(surface, 1, 3) == "DIS") {
            substr <- substring(surface, 4, nchar(surface))
            pronunciations <- GetPronunciations(substr)
            for (i in seq_along(pronunciations)) {
                pronunciations[[i]] <- c(c("D", "IH0", "S"), 
                  pronunciations[[i]])
            }
            return(pronunciations)
        }
        else if (substr(surface, 1, 3) == "MIS") {
            substr <- substring(surface, 4, nchar(surface))
            pronunciations <- GetPronunciations(substr)
            for (i in seq_along(pronunciations)) {
                pronunciations[[i]] <- c(c("M", "IH0", "S"), 
                  pronunciations[[i]])
            }
            return(pronunciations)
        }
        else if (substr(surface, 1, 3) == "PRE") {
            substr <- substring(surface, 4, nchar(surface))
            pronunciations <- GetPronunciations(substr)
            for (i in seq_along(pronunciations)) {
                pronunciations[[i]] <- c(c("P", "R", "IY0"), 
                  pronunciations[[i]])
            }
            return(pronunciations)
        }
        else if (substr(surface, nchar(surface) - 1, nchar(surface)) \%in\% 
            c("'S", "'S")) {
            substr <- substring(surface, 1, nchar(surface) - 
                2)
            pronunciations <- GetPronunciations(substr)
            for (i in seq_along(pronunciations)) {
                pronunciations[[i]] <- c(pronunciations[[i]], 
                  c("S"))
            }
            return(pronunciations)
        }
        else print(toupper(surface))
        stop("Pronunciation unknown. Please ensure standardized orthography and try again.")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ function }