library(chemspiderapi)

context("check_elements")

test_that("check_elements() fails if no input is provided.", {
  expect_error(
    check_elements()
    )
})

test_that("check_elements() fails if NULL is provided as input.", {
  expect_error(
    check_elements(includeElements = NULL,
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if NULL is provided as input.", {
  expect_error(
    check_elements(includeElements = c("C", "H", "O"),
                   excludeElements = NULL)
  )
})

test_that("check_elements() fails if no input for includeElements is provided.", {
  expect_error(
    check_elements(excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if no input for excludeElements is provided.", {
  expect_error(
    check_elements(includeElements = c("C", "H", "O"))
  )
})

test_that("check_elements() fails if a non-character input for includeElements is provided.", {
  expect_error(
    check_elements(includeElements = c(1, 2, 3),
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if a non-character input for excludeElements is provided.", {
  expect_error(
    check_elements(includeElements = c("C", "H", "O"),
                   excludeElements = c(1, 2, 3))
  )
})

test_that("check_elements() fails if includeElements contains over 15 entries.", {
  expect_error(
    check_elements(includeElements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if includeElements contains over 15 entries.", {
  expect_error(
    check_elements(includeElements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if excludeElements contains over 100 entries.", {
  expect_error(
    check_elements(includeElements = c("C", "H", "O"),
                   excludeElements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md"))
  )
})

test_that("check_elements() fails if includeElements contains a non-periodic table element symbol.", {
  expect_error(
    check_elements(includeElements = "J",
                   excludeElements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if excludeElements contains a non-periodic table element symbol.", {
  expect_error(
    check_elements(includeElements = c("C", "H", "O"),
                   excludeElements = "J")
  )
})

test_that("check_elements() remains silent when correct inputs are provided", {
  expect_silent(
    check_elements(includeElements = c("C", "H", "O"),
                   excludeElements = c("Na", "K", "Fe"))
  )
})
