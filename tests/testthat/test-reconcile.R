test_that("test reconcile works - number of rows, columns", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  expected <- names(new_df)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(all(expected %in% names(actual)))
  expect_true(nrow(new_df) <= nrow(actual))
  expect_true(length(expected) == length(names(actual)) - 4)
})


test_that("test reconcile works - check record_status", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "DELETED",]) == (nrow(actual) - nrow(new_df)))
  expect_true(nrow(actual[actual$record_status == "NEW",]) == (nrow(actual) - nrow(old_df)))

  expect_true(nrow(new_df[new_df[, index] %in% old_df[, index],]) == nrow(actual[actual$record_status %in% c("UPDATED", "UNCHANGED"), ]))
})


test_that("test reconcile works - check record_status", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED", "NEW"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED"),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status == "UPDATED", "expected_status"]
  actual <- actual[actual$record_status == "UPDATED", "record_status"]

  expect_identical(actual, expected)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected_records <- actual[actual$record_status == "UNCHANGED", "recordid"]

  new_unchanhed <- new_df[new_df$recordid %in% expected_records,]
  old_unchanhed <- old_df[old_df$recordid %in% expected_records,]

  expect_identical(new_unchanhed, old_unchanhed)

  expected_new_records <- actual[actual$record_status == "NEW", "recordid"]
  new_unchanhed <- new_df[new_df$recordid %in% expected_new_records,]
  old_unchanhed <- old_df[old_df$recordid %in% expected_new_records,]
  expect_true(nrow(new_unchanhed) > 0)
  expect_true(nrow(old_unchanhed) == 0)
})


test_that("test reconcile works - check changed_cols", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED", "NEW"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", "", "ALL"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", ""),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status != "UNCHANGED", "expected_change_cols"]
  actual <- actual[actual$record_status != "UNCHANGED", "changed_cols"]

  expect_identical(actual, expected)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status == "UNCHANGED", "expected_change_cols"]
  actual <- actual[actual$record_status == "UNCHANGED", "changed_cols"]

  expect_identical(actual, expected)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status == "ALL", "expected_change_cols"]
  actual <- actual[actual$record_status == "ALL", "changed_cols"]

  expect_identical(actual, expected)
})


test_that("test reconcile works - check change_details", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED", "NEW"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", "", "ALL"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", ""),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "UPDATED" & actual$changed_cols == "numeric_", c("change_details")]
  expected_new <- new_df[new_df$expected_status == "UPDATED" & new_df$expected_change_cols == "numeric_", c("expected_change_cols", "numeric_")]
  expected_old <- old_df[old_df$expected_status == "UPDATED" & old_df$expected_change_cols == "numeric_", c("numeric_")]
  expected <- cbind(expected_new, expected_old)
  expected$expected_change_details <- paste0(expected$expected_change_cols,": ", expected$expected_old,  " -> ", expected$numeric_)

  expect_identical(expected$expected_change_details, actual)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "UPDATED" & actual$changed_cols == "character_", c("change_details")]
  expected_new <- new_df[new_df$expected_status == "UPDATED" & new_df$expected_change_cols == "character_", c("expected_change_cols", "character_")]
  expected_old <- old_df[old_df$expected_status == "UPDATED" & old_df$expected_change_cols == "character_", c("character_")]
  expected <- cbind(expected_new, expected_old)
  expected$expected_change_details <- paste0(expected$expected_change_cols,": ", expected$expected_old,  " -> ", expected$character_)

  expect_identical(expected$expected_change_details, actual)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "UPDATED" & actual$changed_cols == "character_, numeric_", c("change_details")]
  expected_new <- new_df[new_df$expected_status == "UPDATED" & new_df$expected_change_cols == "character_, numeric_", c("expected_change_cols", "character_", "numeric_")]
  expected_old <- old_df[old_df$expected_status == "UPDATED" & old_df$expected_change_cols == "character_, numeric_", c("character_", "numeric_")]
  expected <- cbind(expected_new, expected_old)
  expected$expected_change_details <- paste0("character_: ", expected_old[["character_"]],  " -> ", expected_new[["character_"]],", ",
                                             "numeric_: ", expected_old[["numeric_"]],  " -> ", expected_new[["numeric_"]])

  expect_identical(expected$expected_change_details, actual)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "NEW", "change_details"]

  expect_identical(actual, "new record created")
})


test_that("test reconcile works - check lookup_columns parameter", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = c("character_", "numeric_"))
  expected <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = NA)

  expect_identical(actual, expected)

  lookup_columns = c("numeric_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% lookup_columns))

  lookup_columns = c("character_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% lookup_columns))

  lookup_columns = c("character_", "numeric_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% c(lookup_columns, paste(lookup_columns, collapse = ", "))))

  lookup_columns = c("numeric_", "character_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% c(lookup_columns, paste(lookup_columns, collapse = ", "))))
})


test_that("test reconcile works - new_df has one row deleted", {
  new_df <- data.frame(
    character_   = c("A", "B", "D"),
    numeric_   = c(101, 102, 104),
    recordid  = c(1, 2, 4),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "UNCHANGED",]) == 3)
  expect_true(nrow(actual[actual$record_status == "DELETED",]) == 1)
  expect_true(actual[actual$record_status == "DELETED", "changed_cols"] == "ALL")
  expect_true(actual[actual$record_status == "DELETED", "change_details"] == "record deleted")
})


test_that("test reconcile works - new_df has one row added", {
  new_df <- data.frame(
    character_   = c("A", "B", "C", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "UNCHANGED",]) == 4)
  expect_true(nrow(actual[actual$record_status == "NEW",]) == 1)
  expect_true(actual[actual$record_status == "NEW", "changed_cols"] == "ALL")
  expect_true(actual[actual$record_status == "NEW", "change_details"] == "new record created")
})


test_that("test reconcile works - check comment removed variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(length(names(new_df)) < length(names(old_df)) & all(grepl("removed variable", actual$comment)))
})


test_that("test reconcile works - check comment added variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(length(names(new_df)) > length(names(old_df)) & all(grepl("added variable", actual$comment)))
})


test_that("test reconcile works - check comment added variable and removed variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    numeric_   = c(101, 102, 103, 104),
    added   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- "recordid"

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(all(grepl("removed variable", actual$comment)) & all(grepl("added variable", actual$comment)))
})


test_that("test reconcile works - number of rows, columns", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA

  expected <- names(new_df)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(all(expected %in% names(actual)))
  expect_true(nrow(new_df) <= nrow(actual))
  expect_true(length(expected) == length(names(actual)) - 4)
})


test_that("test reconcile works - check record_status", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA
  expected <- names(new_df)
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "NEW or CHANGED",]) > 0 & (nrow(new_df) > nrow(old_df)))
  actual_record_id <- actual[actual$record_status == "UNCHANGED", "recordid"]
  expect_true(nrow(actual[actual$record_status == "UNCHANGED", expected]) == nrow(new_df[new_df$recordid == actual_record_id, expected]) &
              nrow(actual[actual$record_status == "UNCHANGED", expected]) == nrow(old_df[old_df$recordid == actual_record_id, expected]))
})


test_that("test reconcile works - check changed_cols", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    expected_status = c("NEW or CHANGED", "NEW or CHANGED", "NEW or CHANGED", "UNCHANGED", "NEW or CHANGED"),
    expected_change_cols = c("UNK", "UNK", "UNK", "", "UNK"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    expected_status = c("NEW or CHANGED", "NEW or CHANGED", "NEW or CHANGED", "UNCHANGED"),
    expected_change_cols = c("UNK", "UNK", "UNK", ""),
    stringsAsFactors = FALSE
  )

  index <- NA

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status != "UNCHANGED", "expected_change_cols"]
  actual <- actual[actual$record_status != "UNCHANGED", "changed_cols"]

  expect_identical(actual, expected)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status == "UNCHANGED", "expected_change_cols"]
  actual <- actual[actual$record_status == "UNCHANGED", "changed_cols"]

  expect_identical(actual, expected)
})


test_that("test reconcile works - check lookup_columns parameter", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = c("character_", "numeric_"))
  expected <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = NA)

  expect_identical(actual, expected)

  lookup_columns = c("numeric_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "NEW or CHANGED", "changed_cols"]
  expect_true(all(actual == "UNK"))

  lookup_columns = c("character_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "NEW or CHANGED", "changed_cols"]
  expect_true(all(actual == "UNK"))

  lookup_columns = c("character_", "numeric_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "NEW or CHANGED", "changed_cols"]
  expect_true(all(actual == "UNK"))

  lookup_columns = c("numeric_", "character_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "NEW or CHANGED", "changed_cols"]
  expect_true(all(actual == "UNK"))
})


test_that("test reconcile works - new_df has one row deleted - not detected since index not provided", {
  new_df <- data.frame(
    character_   = c("A", "B", "D"),
    numeric_   = c(101, 102, 104),
    recordid  = c(1, 2, 4),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "UNCHANGED",]) == nrow(new_df))
  expect_true(all(actual[actual$record_status == "UNCHANGED", "changed_cols"] == ("")))
})


test_that("test reconcile works - new_df has one row added", {
  new_df <- data.frame(
    character_   = c("A", "B", "C", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "UNCHANGED",]) == 4)
  expect_true(nrow(actual[actual$record_status == "NEW or CHANGED",]) == 1)
  expect_true(actual[actual$record_status == "NEW or CHANGED", "changed_cols"] == "UNK")
  expect_true(actual[actual$record_status == "NEW or CHANGED", "change_details"] == "")
})


test_that("test reconcile works - check comment removed variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(length(names(new_df)) < length(names(old_df)) & all(grepl("removed variable", actual$comment)))
})


test_that("test reconcile works - check comment added variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(length(names(new_df)) > length(names(old_df)) & all(grepl("added variable", actual$comment)))

})


test_that("test reconcile works - check comment added variable and removed variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    numeric_   = c(101, 102, 103, 104),
    added   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  index <- NA

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(all(grepl("removed variable", actual$comment)) & all(grepl("added variable", actual$comment)))
})


test_that("test reconcile works - number of rows, columns", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 1, 3, 4, 5),
    formrepeatkey = c(1, 2, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  expected <- names(new_df)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(all(expected %in% names(actual)))
  expect_true(nrow(new_df) <= nrow(actual))
  expect_true(length(expected) == length(names(actual)) - 4)
})


test_that("test reconcile works - check record_status", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 1, 3, 4, 5),
    formrepeatkey = c(1, 2, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "DELETED",]) == (nrow(actual) - nrow(new_df)))
  expect_true(nrow(actual[actual$record_status == "NEW",]) == (nrow(actual) - nrow(old_df)))

  new_keys <- unique(new_df[index])
  old_keys <- unique(old_df[index])
  common_keys <- merge(new_keys, old_keys, by = index)
  expected <- nrow(common_keys)

  expect_true(expected == nrow(actual[actual$record_status %in% c("UPDATED", "UNCHANGED"), ]))
})


test_that("test reconcile works - check record_status", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 4),
    formrepeatkey = c(1, 1, 1, 1, 2),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED", "NEW"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED"),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  expect_new <- new_df %>% dplyr::anti_join(old_df, by = index) %>% nrow()

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- nrow(actual[actual$record_status == "NEW", ])
  expect_equal(expect_new, actual)

  expect_deleted <- old_df %>% dplyr::anti_join(new_df, by = index) %>% nrow()

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- nrow(actual[actual$record_status == "DELETED", ])
  expect_equal(expect_deleted, actual)

  expect_unchanged <- new_df %>% dplyr::inner_join(old_df, by = c("character_", "numeric_", "recordid", "formrepeatkey")) %>% nrow()

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- nrow(actual[actual$record_status == "UNCHANGED", ])
  expect_equal(expect_unchanged, actual)
})


test_that("test reconcile works - check changed_cols", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 4),
    formrepeatkey = c(1, 1, 1, 1, 2),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED", "NEW"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", "", "ALL"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", ""),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status != "UNCHANGED", "expected_change_cols"]
  actual <- actual[actual$record_status != "UNCHANGED", "changed_cols"]

  expect_identical(actual, expected)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status == "UNCHANGED", "expected_change_cols"]
  actual <- actual[actual$record_status == "UNCHANGED", "changed_cols"]

  expect_identical(actual, expected)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expected <- actual[actual$record_status == "ALL", "expected_change_cols"]
  actual <- actual[actual$record_status == "ALL", "changed_cols"]

  expect_identical(actual, expected)
})


test_that("test reconcile works - check change_details", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 4),
    formrepeatkey = c(1, 1, 1, 1, 2),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED", "NEW"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", "", "ALL"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    expected_status = c("UPDATED", "UPDATED", "UPDATED", "UNCHANGED"),
    expected_change_cols = c("character_, numeric_", "numeric_", "character_", ""),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "UPDATED" & actual$changed_cols == "numeric_", c("change_details")]
  expected_new <- new_df[new_df$expected_status == "UPDATED" & new_df$expected_change_cols == "numeric_", c("expected_change_cols", "numeric_")]
  expected_old <- old_df[old_df$expected_status == "UPDATED" & old_df$expected_change_cols == "numeric_", c("numeric_")]
  expected <- cbind(expected_new, expected_old)
  expected$expected_change_details <- paste0(expected$expected_change_cols,": ", expected$expected_old,  " -> ", expected$numeric_)

  expect_identical(expected$expected_change_details, actual)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "UPDATED" & actual$changed_cols == "character_", c("change_details")]
  expected_new <- new_df[new_df$expected_status == "UPDATED" & new_df$expected_change_cols == "character_", c("expected_change_cols", "character_")]
  expected_old <- old_df[old_df$expected_status == "UPDATED" & old_df$expected_change_cols == "character_", c("character_")]
  expected <- cbind(expected_new, expected_old)
  expected$expected_change_details <- paste0(expected$expected_change_cols,": ", expected$expected_old,  " -> ", expected$character_)

  expect_identical(expected$expected_change_details, actual)

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "UPDATED" & actual$changed_cols == "character_, numeric_", c("change_details")]
  expected_new <- new_df[new_df$expected_status == "UPDATED" & new_df$expected_change_cols == "character_, numeric_", c("expected_change_cols", "character_", "numeric_")]
  expected_old <- old_df[old_df$expected_status == "UPDATED" & old_df$expected_change_cols == "character_, numeric_", c("character_", "numeric_")]
  expected <- cbind(expected_new, expected_old)
  expected$expected_change_details <- paste0("character_: ", expected_old[["character_"]],  " -> ", expected_new[["character_"]],", ",
                                             "numeric_: ", expected_old[["numeric_"]],  " -> ", expected_new[["numeric_"]])

  expect_identical(expected$expected_change_details, actual)


  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  actual <- actual[actual$record_status == "NEW", "change_details"]

  expect_identical(actual, "new record created")
})


test_that("test reconcile works - check lookup_columns parameter", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(1010, 1020, 103, 104, 101),
    recordid  = c(1, 2, 3, 4, 5),
    formrepeatkey = c(1, 1, 1, 1, 2),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = c("character_", "numeric_"))
  expected <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = NA)

  expect_identical(actual, expected)

  lookup_columns = c("numeric_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% lookup_columns))

  lookup_columns = c("character_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% lookup_columns))

  lookup_columns = c("character_", "numeric_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% c(lookup_columns, paste(lookup_columns, collapse = ", "))))

  lookup_columns = c("numeric_", "character_")
  actual <- reconcile(new_df = new_df, old_df = old_df, index = index, lookup_columns = lookup_columns)
  actual <- actual[actual$record_status == "UPDATED", "changed_cols"]
  expect_true(all(actual %in% c(lookup_columns, paste(lookup_columns, collapse = ", "))))
})


test_that("test reconcile works - new_df has one row deleted", {
  new_df <- data.frame(
    character_   = c("A", "B", "D"),
    numeric_   = c(101, 102, 104),
    recordid  = c(1, 2, 4),
    formrepeatkey = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 2, 4),
    formrepeatkey = c(1, 1, 2, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "UNCHANGED",]) == 3)
  expect_true(nrow(actual[actual$record_status == "DELETED",]) == 1)
  expect_true(actual[actual$record_status == "DELETED", "changed_cols"] == "ALL")
  expect_true(actual[actual$record_status == "DELETED", "change_details"] == "record deleted")
})


test_that("test reconcile works - new_df has one row added", {
  new_df <- data.frame(
    character_   = c("A", "B", "C", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 4),
    formrepeatkey = c(1, 1, 1, 1, 2),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)

  expect_true(nrow(actual[actual$record_status == "UNCHANGED",]) == 4)
  expect_true(nrow(actual[actual$record_status == "NEW",]) == 1)
  expect_true(actual[actual$record_status == "NEW", "changed_cols"] == "ALL")
  expect_true(actual[actual$record_status == "NEW", "change_details"] == "new record created")
})


test_that("test reconcile works - check comment removed variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    recordid  = c(1, 2, 3, 4, 4),
    formrepeatkey = c(1, 1, 1, 1, 2),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    character_   = c("A", "B", "C", "D"),
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(length(names(new_df)) < length(names(old_df)) & all(grepl("removed variable", actual$comment)))
})


test_that("test reconcile works - check comment added variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 4),
    formrepeatkey = c(1, 1, 1, 1, 2),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    numeric_   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(length(names(new_df)) > length(names(old_df)) & all(grepl("added variable", actual$comment)))
})


test_that("test reconcile works - check comment added variable and removed variable", {
  new_df <- data.frame(
    character_   = c("AA", "B", "new", "D", "E"),
    numeric_   = c(101, 102, 103, 104, 105),
    recordid  = c(1, 2, 3, 4, 4),
    formrepeatkey = c(1, 1, 1, 1, 2),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    numeric_   = c(101, 102, 103, 104),
    added   = c(101, 102, 103, 104),
    recordid  = c(1, 2, 3, 4),
    formrepeatkey = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  index <- c("recordid", "formrepeatkey")

  actual <- reconcile(new_df = new_df, old_df = old_df, index = index)
  expect_true(all(grepl("removed variable", actual$comment)) & all(grepl("added variable", actual$comment)))
})
