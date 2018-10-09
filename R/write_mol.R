write_mol <- function(mol, file) {
  mol <- mol[[1]]
  if (nchar(mol) < 1) {
    stop("Input file (\"mol\") was likely not a valid .mol file.\nCarefully check the input to this function.")
  }
  writeLines(text = mol, con = file(file), sep = "\n")
}
