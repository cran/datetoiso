test_that("clean_date trims whitespace", {
  expect_equal(clean_date("   2024-01-01   "), "2024-01-01")
})


test_that("clean_date handles single T and keeps left side", {
  expect_equal(
    clean_date("2024-01-01T12:00"),
    "2024-01-01"
  )
})

test_that("clean_date keeps only part before T when T occurs once", {
  expect_equal(
    clean_date("20250110T999"),
    "20250110"
  )
})

test_that("clean_date ignores T inside 'August' or 'October'", {
  expect_equal(
    clean_date("2024 August 10"),
    "2024 Aug 10")

  expect_equal(
    clean_date("2024 October 05"),
    "2024 Oct 05"
  )
})

test_that("clean_date handles multiple T occurrences via helper", {
  # Stub expected behavior assuming helper removes after first T
  expect_equal(
    clean_date("2024T01Textra"),
    remove_unnecessary_part_of_date("2024T01Textra")
  )
})

test_that("clean_date extracts first token if >4 characters", {
  expect_equal(
    clean_date("ABCDEFG something"),
    "ABCDEFG"
  )
})

test_that("clean_date respects 12-character limit", {
  expect_true(nchar(clean_date("20240101123456789")) <= 12)
})

test_that("clean_date abbreviates months", {
  expect_equal(
    clean_date("2024 January 10"),
    "2024 Jan 10"
  )
})

test_that("clean_date removes non-date characters", {
  expect_equal(
    clean_date("2024-01-01###"),
    "2024-01-01"
  )
})

test_that("clean_date processes vector input", {
  input <- c("2024-01-01T10:00", "2025 FEB 20")
  expected <- c("2024-01-01", "2025 FEB 20")
  expect_equal(clean_date(input), expected)
})

