#' List month names: full names and abbreviated  names in lower case
#'
#' @author Lukasz Andrzejewski
#' @return full names and abbreviations of months

get_months<-function(){
  months<- tolower(c("January", "February", "March", "April", "May", "June", "July", "August",
                     "September", "October", "November", "December",
                     "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                     "Oct",	"Nov","Dec"))
  return(months)
}

#' List month names: full names in lower case
#'
#' @author Lukasz Andrzejewski
#' @return full names of months

get_months_full_names<-function(){
  months<- tolower(c("January", "February", "March", "April", "May", "June", "July", "August",
                     "September", "October", "November", "December"))
  return(months)
}

#' Get vector with full and abbreviated name of months separated by vertical bar
#'
#' @author Lukasz Andrzejewski
#' @return full names and abbreviations of months separated by vertical bar

get_months_sep_by_vertical_bar<- function(){
  months_sep_by_vertical_bar<- paste(get_months(),sep="",collapse="|")
  return(months_sep_by_vertical_bar)
}

#' Get vector with full name of months separated by vertical bar
#'
#' @author Lukasz Andrzejewski
#' @return full names and abbreviations of months separated by vertical bar

get_full_name_months_sep_by_vertical_bar<- function(){
  months_sep_by_vertical_bar<- paste(get_months_full_names(),sep="",collapse="|")
  return(months_sep_by_vertical_bar)
}

#' Replace full month name by abbreviated month name
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return vector, if any full length month name, then replace by abbreviated month name

get_abbreviated_month_name<-function(df_column){
  month_abbreviations <- c("january" = "Jan", "february" = "Feb", "march" = "Mar",
                           "april" = "Apr", "may" = "May", "june" = "Jun",
                           "july" = "Jul", "august" = "Aug", "september" = "Sep",
                           "october" = "Oct", "november" = "Nov", "december" = "Dec")
  replace_full_month_name_by_abb <- ifelse(grepl(get_full_name_months_sep_by_vertical_bar(),tolower(df_column)),
                                                                                          stringr::str_replace_all(tolower(df_column), month_abbreviations),
                                                                                          df_column)
  return(replace_full_month_name_by_abb)
}

#' function return observations with up to 12 characters
#'
#' @param df_column data frame column or vector to extract observarions up to 12 characters
#' @author Lukasz Andrzejewski
#' @return return up to 12 characters

get_up_to_12_char<-function(df_column){
  substr_up_to_12_chars<-substr(df_column,0,12)
  return(substr_up_to_12_chars)
}

#' Function return special characters separated by vertical bars
#'
#' @param special_characters by default dash, slash, white space characters
#' @author Lukasz Andrzejewski
#' @return special characters: "-|\\/|\\w+\\s+"

has_dash_or_slash_or_white_space_characters_separated_by_vertical_bar <- function(special_characters=c("-","\\/","\\w+\\s+")){
  special_char_sep_by_vertical_bar<- paste(special_characters,sep="",collapse="|")
  return(special_char_sep_by_vertical_bar)

}

#' Function return special characters and months separated by vertical bars
#' @author Lukasz Andrzejewski
#' @return special characters and months: "-|\\/|\\w+\\s+|january|february|march|april|may|june|july|august|september|october|november|december|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"

has_dash_or_slash_or_white_space_characters_or_months_separated_by_vertical_bar <- function(){
  special_char_and_months_sep_by_vertical_bar<-paste(has_dash_or_slash_or_white_space_characters_separated_by_vertical_bar(),
                                                     get_months_sep_by_vertical_bar(),sep="|")
  return(special_char_and_months_sep_by_vertical_bar)
}

#' Return TRUE if data frame column or vector contains date
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, return TRUE if number of characters is higher than 5, contains digits and special characters
#' or month names

find_only_dates<-function(df_column){
  df_column<-get_abbreviated_month_name(df_column)
  df_column<-get_up_to_12_char(df_column)
  find_date<-grepl(has_dash_or_slash_or_white_space_characters_or_months_separated_by_vertical_bar(),tolower(df_column)) &
              grepl("[[:digit:]]",df_column) & nchar(df_column)>=6 &
              !is.na(df_column) & df_column!=" " & !find_unknow_date(df_column)
  return(find_date)
}

#' Find Unknown date, defined as UN or UNK
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if "un" character is found but not "jun"
find_unknow_date<-function(df_column){
  unknown_date<-grepl("[^j]un",tolower(df_column))
  return(unknown_date)
}



#' Function remove symbols
#'
#' @param df_column data frame column or vector from which symbols need to be removed
#' @param symbols by default ; : +
#' @author Lukasz Andrzejewski
#' @return by default delete semicolon, colon and plus sign from vector or data frame

remove_no_date_characters<- function(df_column,symbols="[;:+]"){
  df_column<-stringr::str_replace_all(df_column, pattern = symbols, replacement = "")
  return(df_column)
}

#' Function to find number of symbols in date
#'
#' @param df_column data frame date column or vector with dates
#' @param symbol symbol that needs to be found, by default "T"
#' @author Lukasz Andrzejewski
#' @return number of found symbols
get_number_of_symbols_in_string<-function(df_column,symbol="T"){
  n_of_symbols<-stringr::str_count(toupper(df_column), symbol)
  return(n_of_symbols)
}

#' Get substring of date to eliminate unnecessary part
#'
#' @param df_column date column or vector with dates
#' @param symbol symbol that needs to be found, by default "T"
#' @author Lukasz Andrzejewski
#' @return substring of date from position 1 to position where last "symbol" is located
remove_unnecessary_part_of_date<- function(df_column,symbol="T"){
  last_symbol_position<-unlist(gregexpr(symbol,toupper(df_column)))[-1]
  substring_from_1_to_last_symbol<-substr(df_column,1,last_symbol_position - 1)
  return(substring_from_1_to_last_symbol)
}

#' Additional step for YMD date type
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return output up to 12 characters, remove whitespace from start and end of string, keep characters from
#' the left site of letter "T"

prepare_date<-function(df_column){
  df_column<-get_abbreviated_month_name(df_column)
  df_column<-get_up_to_12_char(df_column)
  df_column<-remove_no_date_characters(df_column)
  df_column<-stringr::str_trim(df_column)
  df_column_ymd<-ifelse(grepl("T",toupper(df_column)) &
                          !is.na(df_column) &
                          get_number_of_symbols_in_string(df_column)==1 &
                          !grepl("august|october|oct",df_column,ignore.case = TRUE),
                stringr::str_split_fixed(toupper(df_column),"T",n=2)[,1],

                ifelse(grepl("T",toupper(df_column)) &
                !is.na(df_column) &
                get_number_of_symbols_in_string(df_column)>1,
                remove_unnecessary_part_of_date(df_column),

                ifelse(nchar(stringr::str_split_fixed(df_column, " ",n=4)[,1])>4,
                stringr::str_split_fixed(df_column, " ",n=4)[,1],
                df_column)))

  return(df_column_ymd)
}

#' Find YMD dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is YMD

find_ymd_date_format<-function(df_column){
  prepare_ymd_date<-prepare_date(df_column)
  ymd_date<-suppressWarnings({!(is.na(prepare_ymd_date) | is.na(lubridate::ymd(prepare_ymd_date)) |
                                  prepare_ymd_date=="" | prepare_ymd_date==" ")})
  return(ymd_date)
}

#' Find DMY dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is DMY

find_dmy_date_format<-function(df_column){
  prepare_dmy_date<-prepare_date(df_column)
  dmy_date<-suppressWarnings({!(is.na(prepare_dmy_date) | is.na(lubridate::dmy(prepare_dmy_date)) |
                                  prepare_dmy_date=="" | prepare_dmy_date==" ")})
  return(dmy_date)
}

#' Find MDY dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is MDY

find_mdy_date_format<-function(df_column){
  prepare_mdy_date<-prepare_date(df_column)
  mdy_date<-suppressWarnings({!(is.na(prepare_mdy_date) | is.na(lubridate::mdy(prepare_mdy_date)) |
                                  prepare_mdy_date=="" | prepare_mdy_date==" ")})
  return(mdy_date)
}

#' Find MYD dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is MYD

find_myd_date_format<-function(df_column){
  prepare_myd_date<-prepare_date(df_column)
  myd_date<-suppressWarnings({!(is.na(prepare_myd_date) | is.na(lubridate::myd(prepare_myd_date)) |
                                  prepare_myd_date=="" | prepare_myd_date==" ")})
  return(myd_date)
}

#' Find YDM dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is YDM

find_ydm_date_format<-function(df_column){
  prepare_ydm_date<-prepare_date(df_column)
  ydm_date<-suppressWarnings({!(is.na(prepare_ydm_date) | is.na(lubridate::ydm(prepare_ydm_date)) |
                                  prepare_ydm_date=="" | prepare_ydm_date==" ")})
  return(ydm_date)
}


#' Find DYM dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is DYM

find_dym_date_format<-function(df_column){
  prepare_dym_date<-prepare_date(df_column)
  dym_date<-suppressWarnings({!(is.na(prepare_dym_date) | is.na(lubridate::dym(prepare_dym_date)) |
                                  prepare_dym_date=="" | prepare_dym_date==" ")})
  return(dym_date)
}


#' Score each of date format ymd, ydm, dmy, dym, mdy, myd and return only the highest score
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return return score of most probable date format
get_max_score_within_data_formats<-function(df_column){
  ymd_score<-sum(find_ymd_date_format(df_column))
  ydm_score<-sum(find_ydm_date_format(df_column))
  dmy_score<-sum(find_dmy_date_format(df_column))
  dym_score<-sum(find_dym_date_format(df_column))
  mdy_score<-sum(find_mdy_date_format(df_column))
  myd_score<-sum(find_myd_date_format(df_column))

  all_scores<-c(ymd_score,ydm_score,dmy_score,dym_score,mdy_score,myd_score)
  max<-max(all_scores,na.rm=TRUE)
  return(max)
}


#' Get TRUE if date format is ymd
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is YMD
choose_ymd_format<-function(df_column){
  dates_yn<-find_only_dates(df_column) & find_ymd_date_format(df_column) & sum(find_ymd_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is dmy
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is DMY
choose_dmy_format<-function(df_column){
  dates_yn<-find_only_dates(df_column) & find_dmy_date_format(df_column) & sum(find_dmy_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is mdy
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is MDY
choose_mdy_format<-function(df_column){
  dates_yn<-find_only_dates(df_column) & find_mdy_date_format(df_column) & sum(find_mdy_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#'  Get TRUE if date format is ydm
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is YDM
choose_ydm_format<-function(df_column){
  dates_yn<-find_only_dates(df_column) & find_ydm_date_format(df_column) & sum(find_ydm_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is myd
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is MYD
choose_myd_format<-function(df_column){
  dates_yn<-find_only_dates(df_column) & find_myd_date_format(df_column) & sum(find_myd_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is dym
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is DYM
choose_dym_format<-function(df_column){
  dates_yn<-find_only_dates(df_column) & find_dym_date_format(df_column) & sum(find_dym_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}
