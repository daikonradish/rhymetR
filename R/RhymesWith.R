RhymesWith <- 
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