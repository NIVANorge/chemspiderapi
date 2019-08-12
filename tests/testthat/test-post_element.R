library(chemspiderapi)

context("post_element")

test_that("check_elements() fails if no input is provided.", {
  expect_error(
    post_element()
  )
})

test_that("post_element() fails if NULL is provided as input.", {
  expect_error(
    post_element(includeElements = NULL,
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if NULL is provided as input.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = NULL)
  )
})

test_that("post_element() fails if no input for includeElements is provided.", {
  expect_error(
    post_element(excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if no input for excludeElements is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"))
  )
})

test_that("post_element() fails if a non-character input for includeElements is provided.", {
  expect_error(
    post_element(includeElements = c(1, 2, 3),
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if a non-character input for excludeElements is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = c(1, 2, 3))
  )
})

test_that("post_element() fails if includeElements contains over 15 entries.", {
  expect_error(
    post_element(includeElements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if includeElements contains over 15 entries.", {
  expect_error(
    post_element(includeElements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if excludeElements contains over 100 entries.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md"))
  )
})

test_that("post_element() fails if includeElements contains a non-periodic table element symbol.", {
  expect_error(
    post_element(includeElements = "J",
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if excludeElements contains a non-periodic table element symbol.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = "J")
  )
})

test_that("post_element() fails if more than one complexity is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                     excludeElements = c("Na", "K", "Fe"),
                     complexity = c("any", "single", "multiple"))
  )
})

test_that("post_element() fails if a wrong character string is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                     excludeElements = c("Na", "K", "Fe"),
                     complexity = "something")
  )
})

test_that("post_element() fails if a numeric value is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                     excludeElements = c("Na", "K", "Fe"),
                     complexity = 123)
  )
})


test_that("post_element() fails if a logical is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                     excludeElements = c("Na", "K", "Fe"),
                     complexity = TRUE)
  )
})

test_that("post_element() fails if NULL is provided as isotopic.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = NULL)
  )
})

test_that("post_element() fails if multiple isotopic are provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = c("all", "labeled"))
  )
})

test_that("post_element() fails if a non-character isotopic is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = 123)
  )
})

test_that("post_element() fails if a wrong isotopic is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                   excludeElements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = "something")
  )
})

test_that("post_element() fails if more than one orderBy is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                excludeElements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                orderBy = c("recordid", "massdefect"), orderDirection = NULL)
  )
})

test_that("post_element() fails if a false orderBy is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                excludeElements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                orderBy = "thewrongthing", orderDirection = NULL)
  )
})

test_that("post_element() fails if a non-character orderBy is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                excludeElements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                orderBy = 123, orderDirection = NULL)
  )
})

test_that("post_element() fails if more than one orderDirection is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                excludeElements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                orderBy = NULL, orderDirection = c("ascending", "descending"))
  )
})

test_that("post_element() fails if a non-character orderDirection is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                excludeElements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                orderBy = NULL, orderDirection = 123)
  )
})

test_that("post_element() fails if a false orderDirection is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                excludeElements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                orderBy = NULL, orderDirection = "thewrongthing")
  )
})

test_that("post_element() fails if no API key is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                 excludeElements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 orderBy = "recordId", orderDirection = "ascending")
  )
})

test_that("post_element() fails if NULL is provided as API key.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                 excludeElements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = NULL)
  )
})

test_that("post_element() fails if more than one API key is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                 excludeElements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = c("API key one", "API key two"))
  )
})

test_that("post_element() fails if a numeric API key is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                 excludeElements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = 1234567890)
  )
})

test_that("post_element() fails if a logical API key is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                 excludeElements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = TRUE)
  )
})

test_that("post_element() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_element(includeElements = c("C", "H", "O"),
                 excludeElements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})
