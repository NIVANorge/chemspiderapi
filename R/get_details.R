get_details <- function(recordId, fields = "all", apikey, id = TRUE) {
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.integer(recordId)) == TRUE) {
    warning("Please provide a valid (integer) \"recordId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(fields) < 1) {
    warning("Please specify which \"fields\" to look up; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (fields != "all" && sum(tolower(fields) %in% c("smiles", "formula", "averagemass", "molecularweight", "monoisotopicmass", "nominalmass", "commonname", "referencecount", "datasourcecount", "pubmedcount", "rsccount", "mol2d", "mol3d")) < length(fields)) {
    warning("One or more \"fields\" are not valid; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(fields) == 1 && fields == "all") {
      fields <- "SMILES,Formula,AverageMass,MolecularWeight,MonoisotopicMass,NominalMass,CommonName,ReferenceCount,DataSourceCount,PubMedCount,RSCCount,Mol2D,Mol3D"
  }
  fields <- paste(fields, collapse = ",")
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/details?fields=", fields)
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"recordId\", the spelling of the \"fields\", and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (id == FALSE) {
    result$id <- NULL
  }
  if (ncol(result) == 1) {
    result <- unlist(result)
  }
  return(result)
}
