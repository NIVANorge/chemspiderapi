write_image <- function(image, file) {
  if (length(image) > 1) {
    warning("This function can only handle individual \"image\" files; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  if (is.character(image[[1]])) {
    image <- jsonlite::base64_dec(image[[1]])
    image <- png::readPNG(image)
    png::writePNG(image = image, target = file)
  }
  else {
    png::writePNG(image = image, target = file)
  }
}
