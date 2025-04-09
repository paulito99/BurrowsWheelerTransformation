# Direktzuweisung der Eingabe
seq_input <- "ananas$"

# Sicherstellen, dass die Sequenz mit einem eindeutigen Terminierungszeichen endet
if (substr(seq_input, nchar(seq_input), nchar(seq_input)) != "$") {
  seq_input <- paste0(seq_input, "$")
}

# Funktion zur Berechnung der Burrows-Wheeler-Transformation
bwt <- function(text) {
  n <- nchar(text)
  # Alle zyklischen Rotationen erzeugen
  rotations <- sapply(0:(n - 1), function(i) {
    paste0(substr(text, i + 1, n), substr(text, 1, i))
  })
  # Lexikographisches Sortieren der Rotationen
  rotations_sorted <- sort(rotations)
  # Die letzte Spalte extrahieren
  last_column <- substr(rotations_sorted, n, n)
  # Die letzte Zeichen zu einem String zusammenfügen
  result <- paste(last_column, collapse = "")
  
  return(result)
}

# Berechnung der Burrows-Wheeler-Transformation
result <- bwt(seq_input)

# Ausgabe des Ergebnisses
cat("Die Burrows-Wheeler-Transformation der Eingabesequenz lautet:\n", result, "\n")