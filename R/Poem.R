Poem <- setRefClass(
    "Poem", 
    fields = list(
      Lines = "vector",
      Text = "character",
      NumLines = "integer",
      RhymeScheme = "vector",
      Meter = "list"
      ),
    methods = list(

      ## Instanstiates a new poem, from a text file or from
      ## raw text.
      initialize = function(string, fromFile = FALSE) {
        if (fromFile) {
          tmp <- readLines(string)
          Text <<- paste(tmp, collapse = "\n")
        } else {
          Text <<- string
          tmp <- unlist(strsplit(string, "\n"))
        }  
        # Must preprocess to remove natural language punctuation.
        # Since punctuation does not change the pronunciation of
        # the words, except for the apostrophe, which can be used
        # to signify possessives, e.g. "the man's cat"
        for (i in seq_along(tmp)) {
          tmp[i] <- gsub("[\".;!&(),:?]", "", tmp[i])
          tmp[i] <- gsub("-", " ", tmp[i])}
        tmp <- tmp[tmp != ""]  #rm empty lines, e.g. stanza breaks
        Lines <<- tmp
      NumLines <<- length(Lines)
      },
     
      ## Prints out the raw text.
      printText = function() {
        cat(.self$Text)
      },

      findRhymeScheme = function(eyeRhyme = FALSE) {
        schema <- rep(NA, length(.self$Lines))
        # Start with the first line.
        # The first line has the rhyme scheme "A"
        currentLine <- 1
        pointer <- 1
        while (TRUE) {
          # Get all the lines that rhyme with the current line.
          linesThatRhyme <- which(unlist(
                  lapply(.self$Lines, 
                  function(x) RhymesWith(x, .self$Lines[currentLine], eyeRhyme = eyeRhyme))))
          schema[linesThatRhyme] <- toupper(letters[pointer])
          # Break out of loop if the entire rhyme scheme has been populated.
          if (all(!is.na(schema))) 
            break
          # Move on to the next unrhymed line.
          currentLine <- min(which(is.na(schema)))
          pointer <- pointer+1
        }
        RhymeScheme <<- schema
      },

      findMeter = function() {
        patterns <- list("iamb" = 2, "trochee" = 2, "dactyl" = 3,
                      "anapest" = 3, "spondee" = 3, "pyrrhic" = 3,
                      "amphibrach" = 3)
        meters <- character(0)
        numfeets <- integer(0)
        for (i in seq_along(.self$Lines)) {
          currentLine <- .self$Lines[i]
          numSyll <- NumSyllables(currentLine)
          distances <- list("iamb" = integer(0), "trochee" = integer(0), 
                        "dactyl" = integer(0), "anapest" = integer(0), 
                        "spondee" = integer(0), "pyrrhic" = integer(0),
                        "amphibrach" = integer(0))
          for (i in seq_along(patterns)) {
            meter <- names(patterns)[i]
            numfeet <- numSyll %/% patterns[[i]]
            distances[[meter]] <- MatchMeter(currentLine, numfeet = numfeet, meter = meter)
          }
          bestDist <- min(unlist(distances, use.names = FALSE))
          if ((bestDist) > (1/2) * numSyll) {
            bestGuess <- NA
            bestSyll <- NA
          } else {
            bestGuess <- names(patterns)[which(distances == bestDist)][1]
            bestSyll <- numSyll %/% patterns[[bestGuess]]
          }
          meters <- c(meters, bestGuess)
          numfeets <- c(numfeets, bestSyll)
        }
        Meter <<- list(meters,numfeets)
      }      

    )
)