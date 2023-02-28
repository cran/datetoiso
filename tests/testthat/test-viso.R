test_that("does function correctly transfer YMD data to ISO standard sep by T", {
  test_data<-c("2022-02-20T22:02:02","2022-02-02T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by T", {
  test_data<-c("2022-20-02T22:02:02","2022-02-02T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by T", {
  test_data<-c("20-02-2022T22:02:02","02-02-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by T", {
  test_data<-c("20-2022-02T22:02:02","02-2022-02T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by T", {
  test_data<-c("02-20-2022T22:02:02","02-02-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})



test_that("does function correctly transfer YMD data to ISO standard sep by /", {
  test_data<-c("2022/02/20","2022/02/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by /", {
  test_data<-c("2022/20/02","2022/02/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by /", {
  test_data<-c("20/02/2022","02/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by /", {
  test_data<-c("20/2022/02","02/2022/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by /", {
  test_data<-c("02/20/2022","02/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer YMD data to ISO standard sep by /", {
  test_data<-c("2022/02/20","2022/02/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by /", {
  test_data<-c("2022/20/02","2022/02/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by /", {
  test_data<-c("20/02/2022","02/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by /", {
  test_data<-c("20/2022/02","02/2022/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by /", {
  test_data<-c("02/20/2022","02/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YMD data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("2022-Feb-20T22:02:02","2022-Feb-02T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("2022-20-FebT22:02:02","2022-02-FebT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("20-Feb-2022T22:02:02","02-Feb-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("20-2022-FebT22:02:02","02-2022-FebT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("Feb-20-2022T22:02:02","Feb-02-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer YMD data to ISO standard sep by / month as character 'Feb' ", {
  test_data<-c("2022/Feb/20","2022/Feb/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("2022/20/Feb","2022/02/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/Feb/2022","02/Feb/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/2022/Feb","02/2022/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("Feb/20/2022","Feb/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer YMD data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("2022/Feb/20","2022/Feb/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("2022/20/Feb","2022/02/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/Feb/2022","02/Feb/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/2022/Feb","02/2022/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("Feb/20/2022","Feb/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

