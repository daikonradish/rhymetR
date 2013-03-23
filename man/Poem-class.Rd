\name{Poem-class}
\Rdversion{1.1}
\docType{class}
\alias{Poem}
\alias{Poem-class}
\title{Class \code{Poem}}

\description{
Reference class that describes a poem. A poem consists of text that are separated into lines. We can analyze a poem, line by line, obtaining rhyme scheme and meter.
}

\section{Fields}{
\describe{
  \item{\code{Lines}:}{A vector of characters. Each element of the vector represents a line in the poem. Each line is stripped of natural language punctuation that the pronunciation can be retrieved.}
  \item{\code{Text}:}{A string that represents the raw text of the poem. This allows text to be printed easily.}
  \item{\code{NumLines}:}{An integer denoting the number of lines in the poem}
  \item{\code{RhymeScheme}:}{A vector of characters denoting the rhyme scheme of the poem. The length of the vector is equal to the number of lines.}
  \item{\code{Meter}:}{A list of denoting the meter of the poem. The list contains two vectors, the first denoting the metrical pattern for each line, and the second denoting the number of feet for each line.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{new(string, fromFile = FALSE)}}{Instantiates a new \code{Poem} object. If \code{fromFile = TRUE}, assumes that \code{string} represents a legal filename of a file that contains the poem that has been correctly typesetted. Otherwise, \code{string} is assumed to be a sequence of characters separated by linebreaks.}
    \item{\code{printText}}{Prints out the text of the poem}
    \item{\code{findRhymeScheme}}{Automatically finds the rhyme scheme of a given poem and updates the \code{RhymeScheme} field}
    \item{\code{findMeter}}{Automatically finds the meter of a given poem and updates the \code{Meter} field}
  }
}

\seealso{
  \code{\link{RhymesWith}}, \code{\link{MatchMeter}}
}

\examples{
  \dontrun{
    EdnaStVincentMillay <- Poem$new("What lips my lips have kissed, and where, and why,
    I have forgotten, and what arms have lain
    Under my head till morning; but the rain
    Is full of ghosts tonight, that tap and sigh
    Upon the glass and listen for reply;
    And in my heart there stirs a quiet pain
    For unremembered lads that not again
    Will turn to me at midnight with a cry.
    Thus in the winter stands a lonely tree,
    Nor knows what birds have vanished one by one,
    Yet know its boughs more silent than before:
    I cannot say what loves have come and gone;
    I only know that summer sang in me
    A little while, that in me sings no more.")
    EdnaStVincentMillay $findRhymeScheme()
    EdnaStVincentMillay $findMeter()
	}
}

