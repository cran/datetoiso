test_that("check if impute_date works for parameter min_max = 'min'", {
  ymd_day_un <- data.frame(
    day_un_month_numeric_hyphen = c(
      "2023-05-UN",
      "2021-12-UN",
      "2022-07-UN"
    ),
    day_un_month_character_hyphen = c(
      "2023-May-UN",
      "2021-Dec-UN",
      "2022-Jul-UN"
    ),
    day_un_month_character_blank = c(
      "2023 May UN",
      "2021 Dec UN",
      "2022 Jul UN"),
    day_un_month_numeric_slash = c(
      "2023/05/UN",
      "2021/12/UN",
      "2022/07/UN"
    ),
    day_un_month_character_slash = c(
      "2023/May/UN",
      "2021/Dec/UN",
      "2022/Jul/UN"
    )
  )

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'ymd', separator =  "-", day = "UN")
  actual_f <- actual[["day_un_month_numeric_hyphen_DTF"]]
  actual <- actual[["day_un_month_numeric_hyphen_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_character_hyphen",
                        suffix = "_DT", date_format = 'ymd', separator =  "-", day = "UN")
  actual_f <- actual[["day_un_month_character_hyphen_DTF"]]
  actual <- actual[["day_un_month_character_hyphen_DT"]]
  expected <- c("2023-05-01", "2021-12-01","2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_character_blank",
                        suffix = "_DT", date_format = 'ymd', separator =  " ", day = "UN")
  actual_f <- actual[["day_un_month_character_blank_DTF"]]
  actual <- actual[["day_un_month_character_blank_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_numeric_slash",
                        suffix = "_DT", date_format = 'ymd', separator =  "/", day = "UN")
  actual_f <- actual[["day_un_month_numeric_slash_DTF"]]
  actual <- actual[["day_un_month_numeric_slash_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_character_slash",
                        suffix = "_DT", date_format = 'ymd', separator =  "/", day = "UN")
  actual_f <- actual[["day_un_month_character_slash_DTF"]]
  actual <- actual[["day_un_month_character_slash_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  ymd_month_un <- data.frame(
    month_un_month_numeric_hyphen = c(
      "2023-UN-10",
      "2021-UN-10",
      "2022-UN-10"
    ),
    month_un_month_numeric_blank = c(
      "2023 UN 15",
      "2021 UN 15",
      "2022 UN 15"),
    month_un_month_numeric_slash = c(
      "2023/UN/15",
      "2021/UN/15",
      "2022/UN/15"
    )
  )

  actual <- impute_date(data_frame = ymd_month_un, column_name = "month_un_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'ymd', separator =  "-", month = "UN")
  actual_f <- actual[["month_un_month_numeric_hyphen_DTF"]]
  actual <- actual[["month_un_month_numeric_hyphen_DT"]]
  expected <- c("2023-01-10","2021-01-10","2022-01-10")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)


  actual <- impute_date(data_frame = ymd_month_un, column_name = "month_un_month_numeric_blank",
                        suffix = "_DT", date_format = 'ymd', separator =  " ", month = "UN")
  actual_f <- actual[["month_un_month_numeric_blank_DTF"]]
  actual <- actual[["month_un_month_numeric_blank_DT"]]
  expected <- c("2023-01-15","2021-01-15","2022-01-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_month_un, column_name = "month_un_month_numeric_slash",
                        suffix = "_DT", date_format = 'ymd', separator =  "/", month = "UN")
  actual_f <- actual[["month_un_month_numeric_slash_DTF"]]
  actual <- actual[["month_un_month_numeric_slash_DT"]]
  expected <- c("2023-01-15","2021-01-15","2022-01-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  dmy_month_un <- data.frame(
    month_unk_month_numeric_hyphen = c(
      "10-UNK-2023",
      "10-UNK-2021",
      "10-UNK-2022"
    ),
    month_unk_month_numeric_blank = c(
      "15 UNK 2023",
      "15 UNK 2021",
      "15 UNK 2022"),
    month_unk_month_numeric_slash = c(
      "15/UNK/2023",
      "15/UNK/2021",
      "15/UNK/2022"
    )
  )

  actual <- impute_date(data_frame = dmy_month_un, column_name = "month_unk_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'dmy', separator =  "-", month = "UNK")
  actual_f <- actual[["month_unk_month_numeric_hyphen_DTF"]]
  actual <- actual[["month_unk_month_numeric_hyphen_DT"]]
  expected <- c("2023-01-10","2021-01-10","2022-01-10")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)


  actual <- impute_date(data_frame = dmy_month_un, column_name = "month_unk_month_numeric_blank",
                        suffix = "_DT", date_format = 'dmy', separator =  " ", month = "UNK")
  actual_f <- actual[["month_unk_month_numeric_blank_DTF"]]
  actual <- actual[["month_unk_month_numeric_blank_DT"]]
  expected <- c("2023-01-15","2021-01-15","2022-01-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_month_un, column_name = "month_unk_month_numeric_slash",
                        suffix = "_DT", date_format = 'dmy', separator =  "/", month = "UNK")
  actual_f <- actual[["month_unk_month_numeric_slash_DTF"]]
  actual <- actual[["month_unk_month_numeric_slash_DT"]]
  expected <- c("2023-01-15","2021-01-15","2022-01-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  ymd_day_month_un <- data.frame(
    day_un_month_un_hyphen = c(
      "2023-UN-UN",
      "2020-UN-UN",
      "2019-UN-UN"
    ),
    day_un_month_un_blank = c(
      "2023 UN UN",
      "2020 UN UN",
      "2019 UN UN"
    ),
    day_un_month_unk_hyphen = c(
      "2023-UNK-UN",
      "2020-UNK-UN",
      "2019-UNK-UN"
    ),
    day_un_month_unk_blank = c(
      "2023 UNK UN",
      "2020 UNK UN",
      "2019 UNK UN")
  )
  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_un_hyphen",
                        separator = "-", month = "UN", suffix = "_DT")
  actual_f <- actual[["day_un_month_un_hyphen_DTF"]]
  actual <- actual[["day_un_month_un_hyphen_DT"]]
  expected <- c("2023-01-01","2020-01-01","2019-01-01")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_un_blank",
                        separator = " ", month = "UN", suffix = "_DT")
  actual_f <- actual[["day_un_month_un_blank_DTF"]]
  actual <- actual[["day_un_month_un_blank_DT"]]
  expected <- c("2023-01-01", "2020-01-01", "2019-01-01")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_unk_hyphen",
                        separator = "-", month = "UNK", suffix = "_DT")
  actual_f <- actual[["day_un_month_unk_hyphen_DTF"]]
  actual <- actual[["day_un_month_unk_hyphen_DT"]]
  expected <- c("2023-01-01", "2020-01-01", "2019-01-01")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_unk_blank",
                        separator = " ", month = "UNK", suffix = "_DT")
  actual_f <- actual[["day_un_month_unk_blank_DTF"]]
  actual <- actual[["day_un_month_unk_blank_DT"]]
  expected <- c("2023-01-01", "2020-01-01", "2019-01-01")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)


  dmy_day_un <- data.frame(
    day_un_month_numeric_hyphen = c(
      "UN-05-2023",
      "UN-12-2021",
      "UN-07-2022"
    ),
    day_un_month_character_hyphen = c(
      "UN-May-2023",
      "UN-Dec-2021",
      "UN-Jul-2022"
    ),
    day_un_month_character_blank = c(
      "UN May 2023",
      "UN Dec 2021",
      "UN Jul 2022"),
    day_un_month_numeric_slash = c(
      "UN/05/2023",
      "UN/12/2021",
      "UN/07/2022"
    ),
    day_un_month_character_slash = c(
      "UN/MAY/2023",
      "UN/DEC/2021",
      "UN/JUL/2022"
    )
  )
  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'dmy', separator =  "-", day = "UN")
  actual_f <- actual[["day_un_month_numeric_hyphen_DTF"]]
  actual <- actual[["day_un_month_numeric_hyphen_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_character_hyphen",
                        suffix = "_DT", date_format = 'dmy', separator =  "-", day = "UN")
  actual_f <- actual[["day_un_month_character_hyphen_DTF"]]
  actual <- actual[["day_un_month_character_hyphen_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_character_blank",
                        suffix = "_DT", date_format = 'dmy', separator =  " ", day = "UN")
  actual_f <- actual[["day_un_month_character_blank_DTF"]]
  actual <- actual[["day_un_month_character_blank_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_numeric_slash",
                        suffix = "_DT", date_format = 'dmy', separator =  "/", day = "UN")
  actual_f <- actual[["day_un_month_numeric_slash_DTF"]]
  actual <- actual[["day_un_month_numeric_slash_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_character_slash",
                        suffix = "_DT", date_format = 'dmy', separator =  "/", day = "UN")
  actual_f <- actual[["day_un_month_character_slash_DTF"]]
  actual <- actual[["day_un_month_character_slash_DT"]]
  expected <- c("2023-05-01", "2021-12-01", "2022-07-01")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)



  dmy_day_un_month_unk_year_unkn <- data.frame(
    day_un_month_unk_year_unkn_hyphen = c(
      "UN-UNK-UNKN",
      "UN-UNK-UNKN",
      "UN-UNK-UNKN"
    ),
    day_un_month_unk_year_unkn_blank = c(
      "UN UNK UNKN",
      "UN UNK UNKN",
      "UN UNK UNKN"
    )
  )

  actual <- impute_date(data_frame = dmy_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_hyphen",
                        separator = "-", suffix = "_DT")
  actual_f <- actual[["day_un_month_unk_year_unkn_hyphen_DTF"]]
  actual <- actual[["day_un_month_unk_year_unkn_hyphen_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_blank",
                        separator = " ", suffix = "_DT")
  actual_f <- actual[["day_un_month_unk_year_unkn_blank_DTF"]]
  actual <- actual[["day_un_month_unk_year_unkn_blank_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)



  ymd_day_un_month_unk_year_unkn <- data.frame(
    day_un_month_unk_year_unkn_hyphen = c(
      "UNKN-UNK-UN",
      "UNKN-UNK-UN",
      "UNKN-UNK-UN"
    ),
    day_un_month_unk_year_unkn_blank = c(
      "UNKN UNK UN",
      "UNKN UNK UN",
      "UNKN UNK UN"
    )
  )
  actual <- impute_date(data_frame = ymd_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_hyphen",
                        separator = "-", suffix = "_DT")
  actual_f <- actual[["day_un_month_unk_year_unkn_hyphen_DTF"]]
  actual <- actual[["day_un_month_unk_year_unkn_hyphen_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_blank",
                        separator = " ", suffix = "_DT")
  actual_f <- actual[["day_un_month_unk_year_unkn_blank_DTF"]]
  actual <- actual[["day_un_month_unk_year_unkn_blank_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)


  ymd_full_date <- data.frame(
    day_month_year_slash = c(
      "2023/May/04",
      "2021/Dec/02",
      "2022/Jul/03"
    ),
    day_month_year_hyphen = c(
      "2023-05-04",
      "2021-12-02",
      "2022-07-03"
    ),
    day_month_year_blank = c(
      "2023 05 04",
      "2021 12 02",
      "2022 07 03"
    )
  )

  actual <- impute_date(data_frame = ymd_full_date, date_format = "ymd", column_name = "day_month_year_slash",
                        separator = "/", suffix = "_DT")
  actual_f <- actual[["day_month_year_slash_DTF"]]
  actual <- actual[["day_month_year_slash_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_full_date, date_format = "ymd", column_name = "day_month_year_hyphen",
                        separator = "-", suffix = "_DT")
  actual_f <- actual[["day_month_year_hyphen_DTF"]]
  actual <- actual[["day_month_year_hyphen_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_full_date, date_format = "ymd", column_name = "day_month_year_blank",
                        separator = " ", suffix = "_DT")
  actual_f <- actual[["day_month_year_blank_DTF"]]
  actual <- actual[["day_month_year_blank_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)


  dmy_full_date <- data.frame(
    day_month_year_slash = c(
      "04/May/2023",
      "02/Dec/2021",
      "03/Jul/2022"
    ),
    day_month_year_hyphen = c(
      "04-05-2023",
      "02-12-2021",
      "03-07-2022"
    ),
    day_month_year_blank = c(
      "04 05 2023",
      "02 12 2021",
      "03 07 2022"
    )
  )

  actual <- impute_date(data_frame = dmy_full_date, date_format = "dmy", column_name = "day_month_year_slash",
                        separator = "/", suffix = "_DT")
  actual_f <- actual[["day_month_year_slash_DTF"]]
  actual <- actual[["day_month_year_slash_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_full_date, date_format = "dmy", column_name = "day_month_year_hyphen",
                        separator = "-", suffix = "_DT")
  actual_f <- actual[["day_month_year_hyphen_DTF"]]
  actual <- actual[["day_month_year_hyphen_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_full_date, date_format = "dmy", column_name = "day_month_year_blank",
                        separator = " ", suffix = "_DT")
  actual_f <- actual[["day_month_year_blank_DTF"]]
  actual <- actual[["day_month_year_blank_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <-c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)
})





test_that("check if impute_date works for parameter min_max = 'max'", {
  ymd_day_un <- data.frame(
    day_un_month_numeric_hyphen = c(
      "2023-05-UN",
      "2021-12-UN",
      "2022-07-UN"
    ),
    day_un_month_character_hyphen = c(
      "2023-May-UN",
      "2021-Dec-UN",
      "2022-Jul-UN"
    ),
    day_un_month_character_blank = c(
      "2023 May UN",
      "2021 Dec UN",
      "2022 Jul UN"),
    day_un_month_numeric_slash = c(
      "2023/05/UN",
      "2021/12/UN",
      "2022/07/UN"
    ),
    day_un_month_character_slash = c(
      "2023/May/UN",
      "2021/Dec/UN",
      "2022/Jul/UN"
    )
  )

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'ymd', separator =  "-", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_numeric_hyphen_DTF"]]
  actual <- actual[["day_un_month_numeric_hyphen_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_character_hyphen",
                        suffix = "_DT", date_format = 'ymd', separator =  "-", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_character_hyphen_DTF"]]
  actual <- actual[["day_un_month_character_hyphen_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_character_blank",
                        suffix = "_DT", date_format = 'ymd', separator =  " ", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_character_blank_DTF"]]
  actual <- actual[["day_un_month_character_blank_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_numeric_slash",
                        suffix = "_DT", date_format = 'ymd', separator =  "/", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_numeric_slash_DTF"]]
  actual <- actual[["day_un_month_numeric_slash_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un, column_name = "day_un_month_character_slash",
                        suffix = "_DT", date_format = 'ymd', separator =  "/", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_character_slash_DTF"]]
  actual <- actual[["day_un_month_character_slash_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  ymd_month_un <- data.frame(
    month_un_month_numeric_hyphen = c(
      "2023-UN-10",
      "2021-UN-10",
      "2022-UN-10"
    ),
    month_un_month_numeric_blank = c(
      "2023 UN 15",
      "2021 UN 15",
      "2022 UN 15"),
    month_un_month_numeric_slash = c(
      "2023/UN/15",
      "2021/UN/15",
      "2022/UN/15"
    )
  )

  actual <- impute_date(data_frame = ymd_month_un, column_name = "month_un_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'ymd', separator =  "-", month = "UN", min_max = 'max')
  actual_f <- actual[["month_un_month_numeric_hyphen_DTF"]]
  actual <- actual[["month_un_month_numeric_hyphen_DT"]]
  expected <- c("2023-12-10","2021-12-10","2022-12-10")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)


  actual <- impute_date(data_frame = ymd_month_un, column_name = "month_un_month_numeric_blank",
                        suffix = "_DT", date_format = 'ymd', separator =  " ", month = "UN", min_max = 'max')
  actual_f <- actual[["month_un_month_numeric_blank_DTF"]]
  actual <- actual[["month_un_month_numeric_blank_DT"]]
  expected <- c("2023-12-15","2021-12-15","2022-12-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_month_un, column_name = "month_un_month_numeric_slash",
                        suffix = "_DT", date_format = 'ymd', separator =  "/", month = "UN", min_max = 'max')
  actual_f <- actual[["month_un_month_numeric_slash_DTF"]]
  actual <- actual[["month_un_month_numeric_slash_DT"]]
  expected <- c("2023-12-15","2021-12-15","2022-12-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  dmy_month_un <- data.frame(
    month_unk_month_numeric_hyphen = c(
      "10-UNK-2023",
      "10-UNK-2021",
      "10-UNK-2022"
    ),
    month_unk_month_numeric_blank = c(
      "15 UNK 2023",
      "15 UNK 2021",
      "15 UNK 2022"),
    month_unk_month_numeric_slash = c(
      "15/UNK/2023",
      "15/UNK/2021",
      "15/UNK/2022"
    )
  )

  actual <- impute_date(data_frame = dmy_month_un, column_name = "month_unk_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'dmy', separator =  "-", month = "UNK", min_max = 'max')
  actual_f <- actual[["month_unk_month_numeric_hyphen_DTF"]]
  actual <- actual[["month_unk_month_numeric_hyphen_DT"]]
  expected <- c("2023-12-10","2021-12-10","2022-12-10")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)


  actual <- impute_date(data_frame = dmy_month_un, column_name = "month_unk_month_numeric_blank",
                        suffix = "_DT", date_format = 'dmy', separator =  " ", month = "UNK", min_max = 'max')
  actual_f <- actual[["month_unk_month_numeric_blank_DTF"]]
  actual <- actual[["month_unk_month_numeric_blank_DT"]]
  expected <- c("2023-12-15","2021-12-15","2022-12-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_month_un, column_name = "month_unk_month_numeric_slash",
                        suffix = "_DT", date_format = 'dmy', separator =  "/", month = "UNK", min_max = 'max')
  actual_f <- actual[["month_unk_month_numeric_slash_DTF"]]
  actual <- actual[["month_unk_month_numeric_slash_DT"]]
  expected <- c("2023-12-15","2021-12-15","2022-12-15")
  expect_equal(actual, expected)
  expected_f <- c("M", "M", "M")
  expect_equal(actual_f, expected_f)

  ymd_day_month_un <- data.frame(
    day_un_month_un_hyphen = c(
      "2023-UN-UN",
      "2020-UN-UN",
      "2019-UN-UN"
    ),
    day_un_month_un_blank = c(
      "2023 UN UN",
      "2020 UN UN",
      "2019 UN UN"
    ),
    day_un_month_unk_hyphen = c(
      "2023-UNK-UN",
      "2020-UNK-UN",
      "2019-UNK-UN"
    ),
    day_un_month_unk_blank = c(
      "2023 UNK UN",
      "2020 UNK UN",
      "2019 UNK UN")
  )
  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_un_hyphen",
                        separator = "-", month = "UN", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_un_hyphen_DTF"]]
  actual <- actual[["day_un_month_un_hyphen_DT"]]
  expected <- c("2023-12-31","2020-12-31","2019-12-31")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_un_blank",
                        separator = " ", month = "UN", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_un_blank_DTF"]]
  actual <- actual[["day_un_month_un_blank_DT"]]
  expected <- c("2023-12-31","2020-12-31","2019-12-31")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_unk_hyphen",
                        separator = "-", month = "UNK", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_unk_hyphen_DTF"]]
  actual <- actual[["day_un_month_unk_hyphen_DT"]]
  expected <- c("2023-12-31","2020-12-31","2019-12-31")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_month_un, column_name = "day_un_month_unk_blank",
                        separator = " ", month = "UNK", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_unk_blank_DTF"]]
  actual <- actual[["day_un_month_unk_blank_DT"]]
  expected <- c("2023-12-31","2020-12-31","2019-12-31")
  expect_equal(actual, expected)
  expected_f <- c("D, M", "D, M", "D, M")
  expect_equal(actual_f, expected_f)


  dmy_day_un <- data.frame(
    day_un_month_numeric_hyphen = c(
      "UN-05-2023",
      "UN-12-2021",
      "UN-07-2022"
    ),
    day_un_month_character_hyphen = c(
      "UN-May-2023",
      "UN-Dec-2021",
      "UN-Jul-2022"
    ),
    day_un_month_character_blank = c(
      "UN May 2023",
      "UN Dec 2021",
      "UN Jul 2022"),
    day_un_month_numeric_slash = c(
      "UN/05/2023",
      "UN/12/2021",
      "UN/07/2022"
    ),
    day_un_month_character_slash = c(
      "UN/MAY/2023",
      "UN/DEC/2021",
      "UN/JUL/2022"
    )
  )
  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_numeric_hyphen",
                        suffix = "_DT", date_format = 'dmy', separator =  "-", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_numeric_hyphen_DTF"]]
  actual <- actual[["day_un_month_numeric_hyphen_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_character_hyphen",
                        suffix = "_DT", date_format = 'dmy', separator =  "-", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_character_hyphen_DTF"]]
  actual <- actual[["day_un_month_character_hyphen_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_character_blank",
                        suffix = "_DT", date_format = 'dmy', separator =  " ", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_character_blank_DTF"]]
  actual <- actual[["day_un_month_character_blank_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_numeric_slash",
                        suffix = "_DT", date_format = 'dmy', separator =  "/", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_numeric_slash_DTF"]]
  actual <- actual[["day_un_month_numeric_slash_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un, column_name = "day_un_month_character_slash",
                        suffix = "_DT", date_format = 'dmy', separator =  "/", day = "UN", min_max = 'max')
  actual_f <- actual[["day_un_month_character_slash_DTF"]]
  actual <- actual[["day_un_month_character_slash_DT"]]
  expected <- c("2023-05-31", "2021-12-31", "2022-07-31")
  expect_equal(actual, expected)
  expected_f <- c("D", "D", "D")
  expect_equal(actual_f, expected_f)



  dmy_day_un_month_unk_year_unkn <- data.frame(
    day_un_month_unk_year_unkn_hyphen = c(
      "UN-UNK-UNKN",
      "UN-UNK-UNKN",
      "UN-UNK-UNKN"
    ),
    day_un_month_unk_year_unkn_blank = c(
      "UN UNK UNKN",
      "UN UNK UNKN",
      "UN UNK UNKN"
    )
  )

  actual <- impute_date(data_frame = dmy_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_hyphen",
                        separator = "-", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_unk_year_unkn_hyphen_DT"]]
  actual <- actual[["day_un_month_unk_year_unkn_hyphen_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_blank",
                        separator = " ", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_unk_year_unkn_blank_DTF"]]
  actual <- actual[["day_un_month_unk_year_unkn_blank_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)



  ymd_day_un_month_unk_year_unkn <- data.frame(
    day_un_month_unk_year_unkn_hyphen = c(
      "UNKN-UNK-UN",
      "UNKN-UNK-UN",
      "UNKN-UNK-UN"
    ),
    day_un_month_unk_year_unkn_blank = c(
      "UNKN UNK UN",
      "UNKN UNK UN",
      "UNKN UNK UN"
    )
  )
  actual <- impute_date(data_frame = ymd_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_hyphen",
                        separator = "-", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_unk_year_unkn_hyphen_DTF"]]
  actual <- actual[["day_un_month_unk_year_unkn_hyphen_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_day_un_month_unk_year_unkn, date_format = "dmy", column_name = "day_un_month_unk_year_unkn_blank",
                        separator = " ", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_un_month_unk_year_unkn_blank_DTF"]]
  actual <- actual[["day_un_month_unk_year_unkn_blank_DT"]]
  expect_true(all(is.na(actual)))
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)


  ymd_full_date <- data.frame(
    day_month_year_slash = c(
      "2023/May/04",
      "2021/Dec/02",
      "2022/Jul/03"
    ),
    day_month_year_hyphen = c(
      "2023-05-04",
      "2021-12-02",
      "2022-07-03"
    ),
    day_month_year_blank = c(
      "2023 05 04",
      "2021 12 02",
      "2022 07 03"
    )
  )

  actual <- impute_date(data_frame = ymd_full_date, date_format = "ymd", column_name = "day_month_year_slash",
                        separator = "/", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_month_year_slash_DTF"]]
  actual <- actual[["day_month_year_slash_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_full_date, date_format = "ymd", column_name = "day_month_year_hyphen",
                        separator = "-", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_month_year_hyphen_DTF"]]
  actual <- actual[["day_month_year_hyphen_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = ymd_full_date, date_format = "ymd", column_name = "day_month_year_blank",
                        separator = " ", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_month_year_blank_DTF"]]
  actual <- actual[["day_month_year_blank_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)


  dmy_full_date <- data.frame(
    day_month_year_slash = c(
      "04/May/2023",
      "02/Dec/2021",
      "03/Jul/2022"
    ),
    day_month_year_hyphen = c(
      "04-05-2023",
      "02-12-2021",
      "03-07-2022"
    ),
    day_month_year_blank = c(
      "04 05 2023",
      "02 12 2021",
      "03 07 2022"
    )
  )

  actual <- impute_date(data_frame = dmy_full_date, date_format = "dmy", column_name = "day_month_year_slash",
                        separator = "/", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_month_year_slash_DTF"]]
  actual <- actual[["day_month_year_slash_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_full_date, date_format = "dmy", column_name = "day_month_year_hyphen",
                        separator = "-", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_month_year_hyphen_DTF"]]
  actual <- actual[["day_month_year_hyphen_DT"]]
  expected <- c("2023-05-04", "2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)

  actual <- impute_date(data_frame = dmy_full_date, date_format = "dmy", column_name = "day_month_year_blank",
                        separator = " ", suffix = "_DT", min_max = 'max')
  actual_f <- actual[["day_month_year_blank_DTF"]]
  actual <- actual[["day_month_year_blank_DT"]]
  expected <- c("2023-05-04","2021-12-02", "2022-07-03")
  expect_equal(actual, expected)
  expected_f <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(actual_f, expected_f)
})
