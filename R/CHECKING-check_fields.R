.check_fields <- function(fields) {
  
  if (!is.character(fields)) {
    stop("The provided \"fields\" are not a character vector.", 
         call. = FALSE)
  }
  
  if (!any(fields %in% "all") && !any(tolower(fields) %in% 
                                      c("smiles", "formula", "inchi", "inchikey", "stdinchi", "stdinchikey", "averagemass", "molecularweight", "monoisotopicmass", "nominalmass", "commonname", "referencecount", "datasourcecount", "pubmedcount", "rsccount", "mol2d", "mol3d"))) {
    stop("One or more \"fields\" are not valid.", 
         call. = FALSE)
  }
  
}
