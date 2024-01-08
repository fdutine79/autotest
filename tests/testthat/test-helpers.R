# Tests for function pstars -----------------------------------------------

test_that("p stars are reported correctly", {
  expect_equal(pstars(""), "")
  expect_equal(pstars(NaN), "")
  expect_equal(pstars(NA), "")
  expect_equal(pstars(NULL), "")
  expect_equal(pstars("a"), "")
  expect_equal(pstars(0.03), "*")
  expect_equal(pstars(0.003), "**")
  expect_equal(pstars(0.0003), "***")
  expect_equal(pstars(0.03, ls = TRUE), " *")
  expect_equal(pstars(0.03, ts = TRUE), "* ")
  expect_equal(pstars(0.03, ls = TRUE, ts = TRUE), " * ")
})


# Tests for function reportp ----------------------------------------------

test_that("The p-value is formatted correctly", {
  expect_equal(reportp(""), "p = NA")
  expect_equal(reportp(NaN), "p = NA")
  expect_equal(reportp(NA), "p = NA")
  expect_equal(reportp(NULL), "p = NA")
  expect_equal(reportp("a"), "p = NA")
  expect_equal(reportp(0.99), "p = .99")
  expect_equal(reportp(0.999), "p = 1")
  expect_equal(reportp(0.03), "p = .03")
  expect_equal(reportp(0.003), "p = .003")
  expect_equal(reportp(0.0003), "p < .001")
})


# Tests for function calc_space -------------------------------------------

test_that("Spaces are calculated correctly", {
  expect_equal(calc_space("", min = 10), 10)
  expect_equal(calc_space(NaN, min = 10), 10)
  expect_equal(calc_space(NA, min = 10), 10)
  expect_equal(calc_space(NULL, params = c("1234")), 4)
  expect_equal(calc_space("123", min = 10), 7)
  expect_equal(calc_space("123456789", min = 3), 0)
  expect_equal(calc_space("123", params = c("1234"), min = 10), 7)
  expect_equal(calc_space("123", params = c("123456789", "12345678901"), min = 5), 8)
  expect_equal(calc_space("12345678901", params = c("123", "456")), 0)
})


# Test for function firstup -----------------------------------------------

test_that("First letter is correctly capitalised", {
  expect_equal(firstup(""), "")
  expect_equal(firstup(NaN), "NaN")
  expect_equal(firstup(NA), "NA")
  expect_equal(firstup(NULL), "")
  expect_equal(firstup(123), "123")
  expect_equal(firstup("mytest"), "Mytest")
  expect_equal(firstup("Mytest"), "Mytest")
  expect_equal(firstup("hello world"), "Hello world")
})
