EyeRhyme <-
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
