.check_elements <- function(includeElements, excludeElements) {
  
  if (is.null(includeElements)) {
    stop("No \"includeElements\" provided.", 
         call. = FALSE)
  }
  
  if (is.null(excludeElements)) {
    stop("No \"excludeElements\" provided.", 
         call. = FALSE)
  }
  
  if (!is.character(includeElements)) {
    stop("\"includeElements\" should consist of character codes for elements.", 
         call. = FALSE)
  }
  
  if (!is.character(excludeElements)) {
    stop("\"excludeElements\" should consist of character codes for elements.", 
         call. = FALSE)
  }
  
  if (sum(tolower(includeElements) %in% 
          tolower(c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og"))) != length(includeElements)) {
    stop("One or more provided inputs to \"includeElements\" are not part of the periodic table", 
         call. = FALSE)
  }
  
  if (sum(tolower(excludeElements) %in% 
          tolower(c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og"))) != length(excludeElements)) {
    stop("One or more provided inputs to \"excludeElements\" are not part of the periodic table", 
         call. = FALSE)
  }
  
  if (length(includeElements) > 15) {
    stop("ChemSpider only supports up to 15 entries in \"includeElements\".", 
         call. = FALSE)
  }
  
  if (length(excludeElements) > 100) {
    stop("ChemSpider only supports up to 100 entries in \"excludeElements\".", 
         call. = FALSE)
  }
  
}
