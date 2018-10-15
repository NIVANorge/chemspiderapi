write_mol <- function(mol, file) {
  mol <- mol[[1]]
  if (nchar(mol) < 1) {
    warning("Input file (\"mol\") was likely not a valid .mol file; returning \"NA\".\nCarefully check the input to this function.", call. = FALSE)
    return(NA_character_)
  }
  writeLines(text = mol, con = file(file), sep = "\n")
}
