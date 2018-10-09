get_details <- function(recordId, fields, apikey, id = TRUE) {
  # if (!requireNamespace("httr", quietly = TRUE)) {
  #   stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (is.na(as.integer(recordId))) {
  #   stop(simpleError("The \"recordId\" must be a valid integer value."))
  # }
  if (length(recordId) > 1) {
    warning("This function can only handle individual ChemSpider IDs (\"recordId\"); returning NA.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (length(fields) == 1) {
    if (fields == "all") {
      fields <- "SMILES,Formula,AverageMass,MolecularWeight,MonoisotopicMass,NominalMass,CommonName,ReferenceCount,DataSourceCount,PubMedCount,RSCCount,Mol2D,Mol3D"
      }
    field <- tolower(fields)
  }
  else {
    fields <- tolower(fields)
    fields <- paste(fields, collapse = ",")
  }
  url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/details")
  result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey), query = list(fields = fields))
  if (result$status_code != 200) {
    warning("No valid information was retrieved, returning NA.\nCarfully check the ChemSpider ID (integer \"recordId\"), the spelling of the \"fields\" names, and the validity of the 32-character API key (\"apikey\").", call. = FALSE)
    return(NA_character_)
  }
  else {
    result <- httr::content(result, type = "application/json")
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    if (id == FALSE) {
      result$id <- NULL
    }
    return(result)
  }
}
