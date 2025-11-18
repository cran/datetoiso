#' transform date vector to date vector in ISO standard ("International Organization for Standardization")
#'
#' @param df_column vector or string
#'
#' @author Lukasz Andrzejewski
#' @return dates formatted to ISO standard (yyyy-mm-dd)
#' @examples
#' #day month year vector
#' viso(c("12Mar2022","21Feb2022"))
#'
#' #day month year vector in different formats
#' viso(c("12Mar2022","21-02-2022"))
#'
#' #month year day vector
#' viso(c("Mar-2022-12","Feb-2022-21"))
#' @export
viso <- function(df_column){
  df_column <- clean_date(df_column)
  df_column_to_date <- ifelse(choose_ymd_format(df_column),
                            format(lubridate::ymd(df_column)),

                            ifelse(choose_dmy_format(df_column),
                                   format(lubridate::dmy(df_column)),

                                   ifelse(choose_mdy_format(df_column),
                                          format(lubridate::mdy(df_column)),

                                          ifelse(choose_ydm_format(df_column),
                                                 format(lubridate::ydm(df_column)),

                                                 ifelse(choose_myd_format(df_column),
                                                        format(lubridate::myd(df_column)),

                                                        ifelse(choose_dym_format(df_column),
                                                               format(lubridate::dym(df_column)),df_column
                                                        ))))))
  return(df_column_to_date)
}
