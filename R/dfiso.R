#' Function recognize date variables and modify them to ISO standard ("International Organization for Standardization")
#'
#' @param df data frame or variable/s, for example data.frame(date=c("12-Mar-2021","01-Jan-2023"))
#' @author Lukasz Andrzejewski
#'
#' @return dates formatted to ISO standard (yyyy-mm-dd)
#' @examples
#' # data frame with different formatted dates
#' dfiso(data.frame(date1=c("13-02-2022","13/Feb/2022","13-Feb-2022")))
#'
#' @export
dfiso <- function(df){
  df <- sapply(df, FUN = function(X) viso(X))
  df <- as.data.frame(df)
  return(df)
}
