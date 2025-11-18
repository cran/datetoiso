#' Impute Missing Components in Partial Date Strings
#'
#' This function imputes missing **month** and/or **day** components in partial date strings where the **year** is known.
#' It assumes input dates are provided in the *ymd* format (year-month-day) and does not process datetime values or
#' strings containing time components or non-date characters.
#'
#' If the **year** is missing or explicitly marked as unknown (e.g., `"UNKN"`), the function returns `NA`.
#' When the **month** is missing, the function imputes **January (01)** as the default month.
#' When the **day** is missing, it imputes the **first day of the month (01)**.
#'
#' Any datetime strings (e.g., `"2025-01-NAT11:10:00"`) must be preprocessed to remove the time component
#' before applying this function (e.g., convert to `"2025-01-NA"`).
#'
#' In addition to imputing the date, the function creates an accompanying **flag variable** named as:
#' `"<source_variable>_<suffix>F"`.
#' This flag variable indicates the type of imputation performed:
#' \itemize{
#'   \item `NA` — No imputation was performed (the original date was complete or missing year).
#'   \item `"D"` — The **day** component was imputed. The **month** component was imputed.
#'   \item `"M"` — The **month** component were imputed.
#'   \item `"D, M"` — Both **month** and **day** components were imputed.
#' }
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @param year by default "UNKN" - the format of unknown year
#' @param month by default "UNK" - the format of unknown month
#' @param day by default "UN" - the format of unknown day
#' @param min_max by default "min". controlling imputation direction."min" → Impute the earliest possible date
#' "max"` → Impute the latest possible date
#' @param suffix by default "_DT" - new imputed date is named as source variable with suffix
#' @author Lukasz Andrzejewski
#'
#' @return A data frame identical to the input, with an additional column representing the imputed values.
#' The imputed column name is constructed by appending the suffix "_imputed" to the source variable name.
#' @export
#'
#' @examples impute_date_ymd(data_frame = data.frame(K = c('2025/11/UN', '2025/11/23')),
#' column_name = "K", separator = "/")
impute_date_ymd <- function(data_frame, column_name, separator = "-", year = "UNKN", month = "UNK", day = "UN", min_max = "min", suffix = "_DT") {

  dates <- data_frame[[column_name]]


  check_unknown_year <- paste0(separator, year, "|", year, separator)
  check_unknown_month <- paste0(separator, month, separator)
  check_unknown_day <- paste0(separator, day, "$")

  if(min_max == "min") {
  imputed <-
    dplyr::case_when(
      check_if_year_month_day_entered(data_frame = data_frame, column_name = column_name,
                                      separator = separator, year = year, month = month, day = day) ~
        suppressWarnings(dplyr::coalesce(as.character(lubridate::ymd(dates)), datetoiso::viso(dates))),

      grepl(check_unknown_year, dates) ~
        NA_character_,

      check_if_only_year_entered(data_frame = data_frame, column_name = column_name,
                                 separator = separator, month = month, day = day) ~
        sapply(strsplit(dates, separator), function(x) paste0(x[1], "-01-01")),

      grepl(check_unknown_month, dates) &
        !grepl(check_unknown_day, dates) ~
        sapply(strsplit(dates, separator), function(x) paste0(x[1], "-01-", x[3])),

      check_if_year_month_entered(data_frame = data_frame, column_name = column_name, separator = separator) ~
        sapply(strsplit(dates, separator), function(x) paste(x[1], x[2], "01", sep = "-")),

      grepl(check_unknown_day, dates) |
        (!is.na(check_day_correctly_entered_ymd(data_frame = data_frame, column_name = column_name, separator = separator)) &
        check_day_correctly_entered_ymd(data_frame = data_frame, column_name = column_name, separator = separator)) ~
        sapply(strsplit(dates, separator), function(x) paste(x[1], x[2], "01", sep = "-")),

      TRUE ~ NA_character_
    )
  } else {
      imputed <-
        dplyr::case_when(
          check_if_year_month_day_entered(data_frame = data_frame, column_name = column_name,
                                          separator = separator, year = year, month = month, day = day) ~
            suppressWarnings(dplyr::coalesce(as.character(lubridate::ymd(dates)), datetoiso::viso(dates))),

          grepl(check_unknown_year, dates) ~
            NA_character_,

          check_if_only_year_entered(data_frame = data_frame, column_name = column_name,
                                     separator = separator, month = month, day = day) ~
            sapply(strsplit(dates, separator), function(x) paste0(x[1], "-12-31")),

          grepl(check_unknown_month, dates) &
            !grepl(check_unknown_day, dates) ~
            sapply(strsplit(dates, separator), function(x) paste0(x[1], "-12-", x[3])),

          check_if_year_month_entered(data_frame = data_frame, column_name = column_name, separator = separator) ~
            sapply(strsplit(dates, separator), function(x) paste(x[1], x[2],
            suppressWarnings(lubridate::days_in_month(lubridate::ymd(paste(x[1], x[2], '01', sep = "-")))),
                                                                 sep = "-")),

          grepl(check_unknown_day, dates) |
            (!is.na(check_day_correctly_entered_ymd(data_frame = data_frame, column_name = column_name, separator = separator)) &
            check_day_correctly_entered_ymd(data_frame = data_frame, column_name = column_name, separator = separator)) ~
            sapply(strsplit(dates, separator), function(x) paste(x[1], x[2],
            suppressWarnings(lubridate::days_in_month(lubridate::ymd(paste(x[1], x[2], '01', sep = "-")))),
                                                                 sep = "-")),

          TRUE ~ NA_character_
        )
  }


  imputation_flag <-
    dplyr::case_when(
      check_if_year_month_day_entered(data_frame = data_frame, column_name = column_name,
                                      separator = separator, year = year, month = month, day = day) ~
        NA_character_,

      grepl(check_unknown_year, dates) ~
        NA_character_,

      check_if_only_year_entered(data_frame = data_frame, column_name = column_name,
                                 separator = separator, month = month, day = day) ~
        "D, M",

      grepl(check_unknown_month, dates) &
        !grepl(check_unknown_day, dates) ~
        "M",

      check_if_year_month_entered(data_frame = data_frame, column_name = column_name, separator = separator) ~
        "D",

      grepl(check_unknown_day, dates) |
      (!is.na(check_day_correctly_entered_ymd(data_frame = data_frame, column_name = column_name, separator = separator)) &
      check_day_correctly_entered_ymd(data_frame = data_frame, column_name = column_name, separator = separator)) ~
        "D",

      TRUE ~ NA_character_
    )


  data_frame[[paste0(column_name, suffix)]] <- suppressWarnings(dplyr::coalesce(as.character(lubridate::ymd(imputed)), datetoiso::viso(imputed)))
  data_frame[[paste0(column_name, suffix, "F")]] <- imputation_flag

  return(data_frame)

}



#' Check if a Vector Contains only Year
#'
#' This function determines whether the elements of a vector contain only **year**
#' It returns a logical vector indicating which elements meet this criterion.
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @param month by default "UNK" - the format of unknown month
#' @param day by default "UN" - the format of unknown day
#' @author Lukasz Andrzejewski
#'
#' @return A logical vector
check_if_only_year_entered <- function(data_frame, column_name, separator = "-", month = "UNK", day = "UN") {

  dates <- data_frame[[column_name]]


  check_unknown_month <- paste0(separator, month, separator)
  check_unknown_day <- paste0(separator, day, "$")


  only_year_entered <- (grepl(check_unknown_month, dates) &
      grepl(check_unknown_day, dates)) |
    (nchar(dates) == 4 & !is.na(suppressWarnings(as.numeric(dates))))


  return(only_year_entered)
}



#' Check if Day Component is Valid in ymd date type
#'
#' This function checks whether the day component in a vector of date strings is valid,
#' i.e., not exceeding the maximum number of days for the given month and year.
#' It returns a logical vector indicating which elements have a correctly specified day.
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @author Lukasz Andrzejewski
#'
#' @return A logical vector
check_day_correctly_entered_ymd <- function(data_frame, column_name, separator = "-") {

  dates <- clean_date(data_frame[[column_name]])

  months_with_31_days <- c(1, 3, 5, 7, 8, 10, 12)
  months_with_30_days <- c(4, 6, 9, 11)
  months_with_28_29 <- 2

  n_keep <- sapply(strsplit(x = dates, split = separator), function(x) length(x))
  day_from_date <- ifelse(n_keep == 3,
                          suppressWarnings(as.integer(data.table::tstrsplit(dates, split = separator, keep = n_keep)[[1]])),
                          NA)
  middle <- ifelse(n_keep == 3,
                   data.table::tstrsplit(dates, split = separator, keep = 2)[[1]],
                   ifelse(n_keep == 2,
                   data.table::tstrsplit(dates, split = separator, keep = 2)[[1]],
                   NA))
  # middle <- data.table::tstrsplit(dates, split = separator, keep = 2)[[1]]
  month_from_date <- suppressWarnings(as.integer(middle))
  month_from_date[is.na(month_from_date)] <- match(toupper(middle[is.na(month_from_date)]), toupper(month.abb))

  check_day_of_month <- (month_from_date %in% months_with_31_days & day_from_date > 31) |
    (month_from_date %in% months_with_30_days & day_from_date > 30) |
    (month_from_date %in% months_with_28_29   & day_from_date > 29)

  check_day_of_month <- ifelse(is.na(month_from_date), TRUE, check_day_of_month)

  return(check_day_of_month)
}



#' Check if Day Component is Valid in dmy date type
#'
#' This function checks whether the day component in a vector of date strings is valid,
#' i.e., not exceeding the maximum number of days for the given month and year.
#' It returns a logical vector indicating which elements have a correctly specified day.
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @author Lukasz Andrzejewski
#'
#' @return A logical vector
check_day_correctly_entered_dmy <- function(data_frame, column_name, separator = "-") {

  dates <- clean_date(data_frame[[column_name]])

  months_with_31_days <- c(1, 3, 5, 7, 8, 10, 12)
  months_with_30_days <- c(4, 6, 9, 11)
  months_with_28_29 <- 2

  n_keep <- sapply(strsplit(x = dates, split = separator), function(x) length(x))
  day_from_date <- ifelse(n_keep == 3,
                          suppressWarnings(as.integer(data.table::tstrsplit(dates, split = separator, keep = 1)[[1]])),
                          NA)

  middle <- ifelse(n_keep == 3,
                   data.table::tstrsplit(dates, split = separator, keep = 2)[[1]],
            ifelse(n_keep == 2,
                   data.table::tstrsplit(dates, split = separator, keep = 1)[[1]],
                   NA))

  month_from_date <- suppressWarnings(as.integer(middle))
  month_from_date[is.na(month_from_date)] <- match(toupper(middle[is.na(month_from_date)]), toupper(month.abb))

  check_day_of_month <- (month_from_date %in% months_with_31_days & day_from_date > 31) |
    (month_from_date %in% months_with_30_days & day_from_date > 30) |
    (month_from_date %in% months_with_28_29   & day_from_date > 29)

  check_day_of_month <- ifelse(is.na(month_from_date), TRUE, check_day_of_month)

  return(check_day_of_month)
}




#' Check if a vector contains a complete date
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @param year by default "UNKN" - the format of unknown year
#' @param month by default "UNK" - the format of unknown month
#' @param day by default "UN" - the format of unknown day
#' @param date_format by default "ymd". choose between ymd (if first year, then month then day) and dmy (if first day, then month then year)
#' @author Lukasz Andrzejewski
#'
#' @return A logical vector
check_if_year_month_day_entered <- function(data_frame, column_name, separator = "-", date_format = "ymd", year = "UNKN", month = "UNK", day = "UN") {

  dates <- clean_date(data_frame[[column_name]])

  if(date_format == "ymd"){
    check_day_of_month <- check_day_correctly_entered_ymd(data_frame = data_frame, column_name = column_name, separator = separator)
  } else {
    check_day_of_month <- check_day_correctly_entered_dmy(data_frame = data_frame, column_name = column_name, separator = separator)
  }
  check_unknown_year <- paste0(separator, year, "|", year, separator)
  check_unknown_month <- paste0(separator, month, separator)
  check_unknown_day <- paste0(separator, day, "$")


  complete_date_entered <- !grepl(check_unknown_year, dates) &
    !grepl(check_unknown_month, dates) &
    !grepl(check_unknown_day, dates) &
    !is.na(check_day_of_month) &
    !check_day_of_month

  return(complete_date_entered)
}


#' Check if a Vector Contains a Year and Month
#'
#' This function determines whether the elements of a vector contain a **year** and **month**
#' in the specified order. It returns a logical vector indicating which elements meet this criterion.
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @author Lukasz Andrzejewski
#'
#' @return A logical vector
check_if_year_month_entered <- function(data_frame, column_name, separator = "-") {

  dates <- data_frame[[column_name]]
  max_length <- max(nchar(clean_date(dates)), na.rm = TRUE)
  n_keep <- sapply(strsplit(x = dates, split = separator), function(x) length(x))
  day_from_date <- ifelse(n_keep == 3,
                          suppressWarnings(as.integer(data.table::tstrsplit(dates, split = separator, keep = n_keep)[[1]])),
                          NA)

  middle <- ifelse(n_keep >= 2,
                   middle <- data.table::tstrsplit(dates, split = separator, keep = 2)[[1]],
                   NA)
  month_from_date <- suppressWarnings(as.integer(middle))
  month_from_date[is.na(month_from_date)] <- match(toupper(middle[is.na(month_from_date)]), toupper(month.abb))


  year_month_entered <- (max_length > nchar(clean_date(dates)) | (nchar(clean_date(dates)) > 4 & nchar(clean_date(dates)) < 10)) &
    is.na(day_from_date) &
    month_from_date & nchar(clean_date(dates)) > 4


  return(year_month_entered)
}


#' Check if a Vector Contains a Month and Year
#'
#' This function determines whether the elements of a vector contain a **month** and **year**
#' in the specified order. It returns a logical vector indicating which elements meet this criterion.
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#'
#' @return A logical vector
check_if_month_year_entered <- function(data_frame, column_name, separator = "-") {

  dates <- data_frame[[column_name]]
  max_length <- max(nchar(clean_date(dates)), na.rm = TRUE)
  n_keep <- sapply(strsplit(x = dates, split = separator), function(x) length(x))
  day_from_date <- ifelse(n_keep == 3,
                          suppressWarnings(as.integer(data.table::tstrsplit(dates, split = separator, keep = 1)[[1]])),
                          NA)
  middle <- ifelse(n_keep == 3,
                   data.table::tstrsplit(dates, split = separator, keep = 2)[[1]],
                   ifelse(n_keep == 2,
                   data.table::tstrsplit(dates, split = separator, keep = 1)[[1]],
                   NA))
  month_from_date <- suppressWarnings(as.integer(middle))
  month_from_date[is.na(month_from_date)] <- match(toupper(middle[is.na(month_from_date)]), toupper(month.abb))

  year_month_entered <- (max_length > nchar(clean_date(dates)) | (nchar(clean_date(dates)) > 4 & nchar(clean_date(dates)) < 10)) &
    is.na(day_from_date) & month_from_date

  year_month_entered <- ifelse(is.na(year_month_entered), FALSE, year_month_entered)

  return(year_month_entered)
}



#' Impute Missing Components in Partial Date Strings
#'
#' This function imputes missing **month** and/or **day** components in partial date strings where the **year** is known.
#' It assumes input dates are provided in the *dmy* format (day-month-year) and does not process datetime values or
#' strings containing time components or non-date characters.
#'
#' If the **year** is missing or explicitly marked as unknown (e.g., `"UNKN"`), the function returns `NA`.
#' When the **month** is missing, the function imputes **January (01)** as the default month.
#' When the **day** is missing, it imputes the **first day of the month (01)**.
#'
#' Any datetime strings (e.g., `"NA-01-2025T11:10:00"`) must be preprocessed to remove the time component
#' before applying this function (e.g., convert to `"NA-01-2025"`).
#'
#' In addition to imputing the date, the function creates an accompanying **flag variable** named as:
#' `"<source_variable>_<suffix>F"`.
#' This flag variable indicates the type of imputation performed:
#' \itemize{
#'   \item `NA` — No imputation was performed (the original date was complete or missing year).
#'   \item `"D"` — The **day** component was imputed.
#'   \item `"M"` — The **month** component was imputed.
#'   \item `"D, M"` — Both **month** and **day** components were imputed.
#' }
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @param year by default "UNKN" - the format of unknown year
#' @param month by default "UNK" - the format of unknown month
#' @param day by default "UN" - the format of unknown day
#' @param min_max by default "min". controlling imputation direction."min" → Impute the earliest possible date
#' "max"` → Impute the latest possible date
#' @param suffix by default "_DT" - new imputed date is named as source variable with suffix
#' @author Lukasz Andrzejewski
#'
#' @return A data frame identical to the input, with an additional column representing the imputed values.
#' The imputed column name is constructed by appending the suffix "_imputed" to the source variable name.
#' @export
#'
#' @examples impute_date_dmy(data_frame = data.frame(K = c('NA 11 2025', '23 11 2025')),
#' column_name = "K", separator = " ", day = "NA")
impute_date_dmy <- function(data_frame, column_name, separator = "-", year = "UNKN", month = "UNK", day = "UN", min_max = "min", suffix = "_DT") {

  dates <- data_frame[[column_name]]


  check_unknown_year <- paste0(separator, year, "|", year, separator)
  check_unknown_month <- paste0(separator, month, separator)
  check_unknown_day <- paste0("^", day, separator)

  if(min_max == "min") {
  imputed <-
    dplyr::case_when(
      check_if_year_month_day_entered(data_frame = data_frame, column_name = column_name, date_format = 'dmy',
                                      separator = separator, year = year, month = month, day = day) ~
          suppressWarnings(dplyr::coalesce(as.character(lubridate::dmy(dates)), datetoiso::viso(dates))),

      grepl(check_unknown_year, dates) ~
        NA_character_,

      check_if_only_year_entered(data_frame = data_frame, column_name = column_name,
                                 separator = separator, month = month, day = day) ~
        sapply(strsplit(dates, separator), function(x) paste0(x[1], "-01-01")),

      grepl(check_unknown_month, dates) &
        !grepl(check_unknown_day, dates) ~
        sapply(strsplit(dates, separator), function(x) paste0(x[3], "-01-", x[1])),

      check_if_month_year_entered(data_frame = data_frame, column_name = column_name, separator = separator) ~
        sapply(strsplit(dates, separator), function(x) paste(x[2], x[1], "01", sep = "-")),

      grepl(check_unknown_day, dates) |
        (!is.na(check_day_correctly_entered_dmy(data_frame = data_frame, column_name = column_name, separator = separator)) &
        check_day_correctly_entered_dmy(data_frame = data_frame, column_name = column_name, separator = separator)) ~
        sapply(strsplit(dates, separator), function(x) paste(x[3], x[2], "01", sep = "-")),

      TRUE ~ NA_character_
    )
  } else {
    imputed <-
      dplyr::case_when(
        check_if_year_month_day_entered(data_frame = data_frame, column_name = column_name, date_format = 'dmy',
                                        separator = separator, year = year, month = month, day = day) ~
          suppressWarnings(dplyr::coalesce(as.character(lubridate::dmy(dates)), datetoiso::viso(dates))),

        grepl(check_unknown_year, dates) ~
          NA_character_,

        check_if_only_year_entered(data_frame = data_frame, column_name = column_name,
                                   separator = separator, month = month, day = day) ~
          sapply(strsplit(dates, separator), function(x) paste0(x[1], "-12-31")),

        grepl(check_unknown_month, dates) &
          !grepl(check_unknown_day, dates) ~
          sapply(strsplit(dates, separator), function(x) paste0(x[3], "-12-", x[1])),

        check_if_month_year_entered(data_frame = data_frame, column_name = column_name, separator = separator) ~
          sapply(strsplit(dates, separator), function(x) paste(x[2], x[1],
          suppressWarnings(lubridate::days_in_month(lubridate::ymd(paste(x[2], x[1], '01', sep = "-")))),
                                                               sep = "-")),

        grepl(check_unknown_day, dates) |
          (!is.na(check_day_correctly_entered_dmy(data_frame = data_frame, column_name = column_name, separator = separator)) &
          check_day_correctly_entered_dmy(data_frame = data_frame, column_name = column_name, separator = separator)) ~
          sapply(strsplit(dates, separator), function(x) paste(x[3], x[2],
          suppressWarnings(lubridate::days_in_month(lubridate::ymd(paste(x[3], x[2], '01', sep = "-")))),
                                                               sep = "-")),

        TRUE ~ NA_character_
      )
  }


  imputation_flag <-
    dplyr::case_when(
      check_if_year_month_day_entered(data_frame = data_frame, column_name = column_name, date_format = 'dmy',
                                      separator = separator, year = year, month = month, day = day) ~
        NA_character_,

      grepl(check_unknown_year, dates) ~
        NA_character_,

      check_if_only_year_entered(data_frame = data_frame, column_name = column_name,
                                 separator = separator, month = month, day = day) ~
        "D, M",

      grepl(check_unknown_month, dates) &
        !grepl(check_unknown_day, dates) ~
        "M",

      check_if_month_year_entered(data_frame = data_frame, column_name = column_name, separator = separator) ~
        "D",

      grepl(check_unknown_day, dates) |
        (!is.na(check_day_correctly_entered_dmy(data_frame = data_frame, column_name = column_name, separator = separator)) &
        check_day_correctly_entered_dmy(data_frame = data_frame, column_name = column_name, separator = separator)) ~
        "D",

      TRUE ~ NA_character_
    )

  data_frame[[paste0(column_name, suffix)]] <- suppressWarnings(dplyr::coalesce(as.character(lubridate::ymd(imputed)), datetoiso::viso(imputed)))
  data_frame[[paste0(column_name, suffix, "F")]] <- imputation_flag

  return(data_frame)

}


#' Impute Missing Components in Partial Date Strings
#'
#' This function imputes missing **month** and/or **day** components in partial date strings where the **year** is known.
#' It assumes input dates are provided in either the *dmy* format (day-month-year) **or** the *ymd* format (year-month-day)
#' and does not process datetime values or strings containing time components or non-date characters.
#'
#' If the **year** is missing or explicitly marked as unknown (e.g., `"UNKN"`), the function returns `NA`.
#' When the **month** is missing, the function imputes **January (01)** as the default month.
#' When the **day** is missing, it imputes the **first day of the month (01)**.
#'
#' Any datetime strings (e.g., `"NA-01-2025T11:10:00"`) must be preprocessed to remove the time component
#' before applying this function (e.g., convert to `"NA-01-2025"`).
#'
#' In addition to imputing the date, the function creates an accompanying **flag variable** named as:
#' `"<source_variable>_<suffix>F"`.
#' This flag variable indicates the type of imputation performed:
#' \itemize{
#'   \item `NA` — No imputation was performed (the original date was complete).
#'   \item `"D"` — The **day** component was imputed.
#'   \item `"M"` — The **month** component were imputed.
#'   \item `"D, M"` — Both **month** and **day** components were imputed.
#' }
#'
#' @param data_frame data frame
#' @param column_name name of column that keeps dates to be imputed
#' @param separator by default "-" it is a day-month-year separator, for example "2024-10-21" has "-" separator
#' @param year by default "UNKN" - the format of unknown year
#' @param month by default "UNK" - the format of unknown month
#' @param day by default "UN" - the format of unknown day
#' @param date_format by default "ymd". choose between ymd (if first year, then month then day) and dmy (if first day, then month then year)
#' @param min_max by default "min". controlling imputation direction."min" → Impute the earliest possible date
#' "max"` → Impute the latest possible date
#' @param suffix by default "_DT" - new imputed date is named as source variable with suffix
#' @author Lukasz Andrzejewski
#'
#' @return A data frame identical to the input, with an additional column representing the imputed values.
#' The imputed column name is constructed by appending the suffix "_imputed" to the source variable name.
#' @export
#'
#' @examples impute_date(data_frame = data.frame(K = c('2025 11 UN', '2025 UNK 23')),
#' column_name = "K", separator = " ")
impute_date <- function(data_frame, column_name, date_format = "ymd", separator = "-", year = "UNKN", month = "UNK", day = "UN", min_max = "min", suffix = "_DT") {

  if(date_format == "ymd") {
    output <- impute_date_ymd(data_frame = data_frame, column_name = column_name, separator = separator, year = year, month = month, day = day, min_max = min_max, suffix = suffix)
  }else if (date_format == "dmy") {
    output <- impute_date_dmy(data_frame = data_frame, column_name = column_name, separator = separator, year = year, month = month, day = day, min_max = min_max, suffix = suffix)
  }else{
    print("please provide date_format as ymd or dmy")
  }

  return(output)

}
