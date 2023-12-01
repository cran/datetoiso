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

test_that("does function correctly transfer YMD data to ISO standard sep by T months as character 'Feb' and 'Sep'", {
  test_data<-c("2022-Feb-20T22:02:02","2022-Sep-02T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-09-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YMD data to ISO standard sep by T months as character 'February' and 'SEPTEMBER'", {
  test_data<-c("2022-February-20T22:02:02","2022-SEPTEMBER-02T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-09-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("2022-20-FebT22:02:02","2022-02-FebT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by T months as character 'Feb' and 'Oct'", {
  test_data<-c("2022-20-FebT22:02:02","2022-02-OctT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by T months as character 'February' and 'OCTOBER'", {
  test_data<-c("2022-20-FebruaryT22:02:02","2022-02-OCTOBERT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("20-Feb-2022T22:02:02","02-Feb-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by T months as character 'Feb' and 'Oct'", {
  test_data<-c("20-Feb-2022T22:02:02","02-Oct-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by T months as character 'February' and 'OCTOBER'", {
  test_data<-c("20-February-2022T22:02:02","02-OCTOBER-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("20-2022-FebT22:02:02","02-2022-FebT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by T months as character 'Feb' and 'OCT'", {
  test_data<-c("20-2022-FebT22:02:02","02-2022-OCTT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by T months as character 'February' and 'OCTOBER'", {
  test_data<-c("20-2022-FebruaryT22:02:02","02-2022-OCTOBERT22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by T month as character 'Feb'", {
  test_data<-c("Feb-20-2022T22:02:02","Feb-02-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer MDY data to ISO standard sep by T months as character 'Feb' and 'Oct'", {
  test_data<-c("Feb-20-2022T22:02:02","Oct-02-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by T months as character 'February' and 'October'", {
  test_data<-c("February-20-2022T22:02:02","October-02-2022T22:02:02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YMD data to ISO standard sep by / month as character 'Feb' ", {
  test_data<-c("2022/Feb/20","2022/Feb/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YMD data to ISO standard sep by / months as character 'Feb' and 'Oct' ", {
  test_data<-c("2022/Feb/20","2022/Oct/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YMD data to ISO standard sep by / months as character 'February' and 'October' ", {
  test_data<-c("2022/February/20","2022/October/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("2022/20/Feb","2022/02/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("2022/20/Feb","2022/02/Oct")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / months as character 'February' and 'OCTOBER'", {
  test_data<-c("2022/20/February","2022/02/OCTOBER")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/Feb/2022","02/Feb/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("20/Feb/2022","02/Oct/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by / months as character 'February' and 'OCTOBER'", {
  test_data<-c("20/February/2022","02/OCTOBER/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/2022/Feb","02/2022/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("20/2022/Feb","02/2022/Oct")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / months as character 'February' and 'October'", {
  test_data<-c("20/2022/February","02/2022/October")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("Feb/20/2022","Feb/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("Feb/20/2022","Oct/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / months as character 'February' and 'OCTOBER'", {
  test_data<-c("February/20/2022","OCTOBER/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer YMD data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("2022/Feb/20","2022/Feb/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YMD data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("2022/Feb/20","2022/Oct/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YMD data to ISO standard sep by / months as character 'February' and 'SEPTEMBER'", {
  test_data<-c("2022/February/20","2022/SEPTEMBER/02")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-09-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("2022/20/Feb","2022/02/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("2022/20/Feb","2022/02/Oct")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer YDM data to ISO standard sep by / months as character 'FEBRUARY' and 'october'", {
  test_data<-c("2022/20/FEBRUARY","2022/02/october")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})


test_that("does function correctly transfer DMY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/Feb/2022","02/Feb/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("20/Feb/2022","02/Oct/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by / months as character 'February' and 'SEPTEMBER'", {
  test_data<-c("20/February/2022","02/SEPTEMBER/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-09-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("20/2022/Feb","02/2022/Feb")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("20/2022/Feb","02/2022/Oct")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DYM data to ISO standard sep by / months as character 'February' and 'OCTOBER'", {
  test_data<-c("20/2022/February","02/2022/OCTOBER")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / month as character 'Feb'", {
  test_data<-c("Feb/20/2022","Feb/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-02-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / months as character 'Feb' and 'Oct'", {
  test_data<-c("Feb/20/2022","Oct/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer MDY data to ISO standard sep by / months as character 'February' and 'October'", {
  test_data<-c("February/20/2022","October/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-02-20","2022-10-02")
  expect_equal(actual,test_data)
})

test_that("function doesn't process unknown date/month date", {
  test_data<-c("UNK-UN-2022","Feb-UNK-2022","10-UN-2021")
  test_data<-viso(test_data)
  actual<-c("UNK-UN-2022","Feb-UNK-2022","10-UN-2021")
  expect_equal(actual,test_data)
})

test_that("does function correctly transfer DMY data to ISO standard sep by ' ' month as character 'OCT'", {
  test_data<-c("OCT/20/2022","OCT/02/2022")
  test_data<-viso(test_data)
  actual<-c("2022-10-20","2022-10-02")
  expect_equal(actual,test_data)
})

