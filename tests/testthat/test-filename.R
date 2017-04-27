context("filename")

test_that("filename is a character string", {
  fname <- make_filename(2013)
  expect_that(fname, is_a("character"))
})
