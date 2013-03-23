GetPronunciations <-
function(surface) {
  surface <- toupper(surface)
  pronunciations <- CMUDict[[surface]]
  if (!is.null(pronunciations)) {
    return(pronunciations)} 
  else {
    if (substr(surface, 1,2) == "UN") {
      substr <- substring(surface, 3, nchar(surface))
      pronunciations <- GetPronunciations(substr)
      for (i in seq_along(pronunciations)) {
        pronunciations[[i]] <- c(c("AH0", "N"), pronunciations[[i]])}
      return(pronunciations)}
    else if (substr(surface, 1,3) == "DIS") {
      substr <- substring(surface, 4, nchar(surface))
      pronunciations <- GetPronunciations(substr)
      for (i in seq_along(pronunciations)) {
        pronunciations[[i]] <- c(c("D",  "IH0", "S"), pronunciations[[i]])}
      return(pronunciations)}
    else if (substr(surface, 1,3) == "MIS") {
      substr <- substring(surface, 4, nchar(surface))
      pronunciations <- GetPronunciations(substr)
      for (i in seq_along(pronunciations)) {
        pronunciations[[i]] <- c(c("M",  "IH0", "S"), pronunciations[[i]])}
      return(pronunciations)}
    else if (substr(surface, 1,3) == "PRE") {
      substr <- substring(surface, 4, nchar(surface))
      pronunciations <- GetPronunciations(substr)
      for (i in seq_along(pronunciations)) {
        pronunciations[[i]] <- c(c("P", "R", "IY0"), pronunciations[[i]])}
      return(pronunciations)}
    else if (substr(surface, nchar(surface)-1,nchar(surface)) %in% c("\'S", "'S")) {
      substr <- substring(surface, 1, nchar(surface)-2)
      pronunciations <- GetPronunciations(substr)
      for (i in seq_along(pronunciations)) {
        pronunciations[[i]] <- c(pronunciations[[i]], c("S"))}
      return(pronunciations)}
    else 
      print(toupper(surface))
      stop("Pronunciation unknown. Please ensure standardized orthography and try again.")
  } 
}
