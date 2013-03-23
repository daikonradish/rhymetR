ARPABETRhyme <-
function(x,y){

  stressed_vowels <- c('IH2', 'UW2', 'UW1', 'IH1',
                      'AH2', 'AH1', 'UH2', 'UH1', 
                      'EH2', 'EH1', 'EY1', 'EY2', 
                      'IY1', 'AY1', 'AY2', 'IY2', 
                      'AW2', 'AW1', 'AO2', 'AO1', 
                      'OW1', 'OW2', 'AE1', 'AE2', 
                      'ER1', 'ER2', 'OY2', 'OY1', 
                      'AA1', 'AA2')

  shorter <- min(length(x), length(y))
  for (i in 1:shorter) {
    pointer <- i - 1
    if (x[length(x) - pointer] != y[length(y) - pointer])
      return(FALSE)
    else {
      if (x[length(x) - pointer] %in% stressed_vowels)
        return(TRUE)
      else
         next
    }
  }
}