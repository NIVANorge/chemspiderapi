post_batch <- function(recordIds, fields = "all", apikey, id = TRUE) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(recordIds) == 1) {
    warning("This function is meant for handling multiple \"queryId\" entries; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(recordIds) > 100) {
    warning("Only up to 100 \"recordIds\" are allowed; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(fields) == 1 && fields == "all") {
    fields <- c("SMILES", "Formula", "AverageMass", "MolecularWeight", "MonoisotopicMass", "NominalMass", "CommonName", "ReferenceCount", "DataSourceCount", "PubMedCount", "RSCCount", "Mol2D", "Mol3D")
    }
  if (length(fields) == 1 && fields != "all") {
    fields <- I(fields)
  }
  curlData <- list("recordIds" = recordIds, "fields" = fields)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list("Content-Type" = "", "apikey" = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/records/batch"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST")
  curl::handle_setopt(curlHandle, postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"name\", \"orderBy\" and \"orderDirection\" (if applicable), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result[[1]], stringsAsFactors = FALSE)
  if (id == FALSE) {
    result$id <- NULL
  }
  return(result)
}
