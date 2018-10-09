write_image <- function(image, file) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package \"png\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (length(image) > 1) {
    stop("This function can only handle individual (\"image\") files.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  if (is.character(image[[1]])) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
    }
    image <- jsonlite::base64_dec(image[[1]])
    image <- png::readPNG(image)
    png::writePNG(image = image, target = file)
  }
  else {
    png::writePNG(image = image, target = file)
  }
}
