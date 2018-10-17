write_image <- function(image, file) {
  if (length(image) > 1) {
    warning("This function can only handle a single \"image\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  if (typeof(image) == "character") {
    image <- jsonlite::base64_dec(image)
    image <- png::readPNG(image)
  }
  png::writePNG(image = image, target = file)
}
