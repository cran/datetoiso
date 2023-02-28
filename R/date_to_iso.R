#' List month names in lower case
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


#' Get vector with months separated by vertical bar
#'
#' @author Lukasz Andrzejewski
#' @return full names and abbreviations of months separated by vertical bar

get_months_sep_by_vertical_bar<- function(){
  months_sep_by_vertical_bar<- paste(get_months(),sep="",collapse="|")
  return(months_sep_by_vertical_bar)
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
  df_column<-get_up_to_12_char(df_column)
  find_date<-grepl(has_dash_or_slash_or_white_space_characters_or_months_separated_by_vertical_bar(),df_column) &
  ((grepl("^[[:digit:]]",df_column) & nchar(df_column)>=6) &
  (!is.na(df_column) & df_column!=" "))
  return(find_date)
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

#' Additional step for YMD date type
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return output up to 12 characters, remove whitespace from start and end of string, keep characters from
#' the left site of letter "T"

prepare_date<-function(df_column){
  df_column<-get_up_to_12_char(df_column)
  df_column<-remove_no_date_characters(df_column)
  df_column<-stringr::str_trim(df_column)
  df_column_ymd<-ifelse(grepl("T",df_column) & !is.na(df_column),
         stringr::str_split_fixed(df_column,"T",n=2)[,1],
         ifelse(nchar(stringr::str_split_fixed(df_column, " ",n=4)[,1])>4,
          stringr::str_split_fixed(df_column, " ",n=4)[,1],df_column))
  return(df_column_ymd)
}

#' Find YMD dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is YMD

find_ymd_date_format<-function(df_column){
  ymd_date<-suppressWarnings({!(is.na(df_column) | is.na(lubridate::ymd(df_column)) |
            df_column=="" | df_column==" ")})
  return(ymd_date)
}

#' Find DMY dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is DMY

find_dmy_date_format<-function(df_column){
  dmy_date<-suppressWarnings({!(is.na(df_column) | is.na(lubridate::dmy(df_column)) |
                                  df_column=="" | df_column==" ")})
  return(dmy_date)
}

#' Find MDY dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is MDY

find_mdy_date_format<-function(df_column){
  mdy_date<-suppressWarnings({!(is.na(df_column) | is.na(lubridate::mdy(df_column)) |
                                  df_column=="" | df_column==" ")})
  return(mdy_date)
}

#' Find MYD dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is MYD

find_myd_date_format<-function(df_column){
  myd_date<-suppressWarnings({!(is.na(df_column) | is.na(lubridate::myd(df_column)) |
                                  df_column=="" | df_column==" ")})
  return(myd_date)
}

#' Find YDM dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is YDM

find_ydm_date_format<-function(df_column){
  ydm_date<-suppressWarnings({!(is.na(df_column) | is.na(lubridate::ydm(df_column)) |
                                  df_column=="" | df_column==" ")})
  return(ydm_date)
}


#' Find DYM dates only
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if date format is DYM

find_dym_date_format<-function(df_column){
  dym_date<-suppressWarnings({!(is.na(df_column) | is.na(lubridate::dym(df_column)) |
                                  df_column=="" | df_column==" ")})
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
  dates_yn<-find_ymd_date_format(df_column) & sum(find_ymd_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is dmy
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is DMY
choose_dmy_format<-function(df_column){
  dates_yn<-find_dmy_date_format(df_column) & sum(find_dmy_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is mdy
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is MDY
choose_mdy_format<-function(df_column){
  dates_yn<-find_mdy_date_format(df_column) & sum(find_mdy_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#'  Get TRUE if date format is ydm
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is YDM
choose_ydm_format<-function(df_column){
  dates_yn<-find_ydm_date_format(df_column) & sum(find_ydm_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is myd
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is MYD
choose_myd_format<-function(df_column){
  dates_yn<-find_myd_date_format(df_column) & sum(find_myd_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}

#' Get TRUE if date format is dym
#'
#' @param df_column data frame date column or vector with dates
#' @author Lukasz Andrzejewski
#' @return logical vector, TRUE if most probable date format is DYM
choose_dym_format<-function(df_column){
  dates_yn<-find_dym_date_format(df_column) & sum(find_dym_date_format(df_column))==get_max_score_within_data_formats(df_column)
  return(dates_yn)
}
