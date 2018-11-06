write_mol <- function(mol, file) {
  mol <- unname(mol)
  if (nchar(mol) < 1) {
    warning("Input is not a valid .mol/.sdf file; returning \"NA\".\nCarefully check the input to this function.", call. = FALSE)
    return(NA_character_)
  }
  writeLines(text = mol, con = file(file), sep = "\n")
}
