#' @title Truncate holdings table to most recent date
#' @param tbl_hold data.frame of holdings with date in TimeStamp field
#' @return data.frame with holdings for the most recent date
#' @export
latest_holdings <- function(tbl_hold) {
  tbl_hold$TimeStamp <- as.Date(tbl_hold$TimeStamp)
  is_latest <- tbl_hold$TimeStamp == max(tbl_hold$TimeStamp)
  tbl_hold[is_latest, ]
}

#' @export
read_macro_wb <- function(wb, fact_nm) {
  menu <- readxl::read_excel(wb, 'menu')
  menu <- as.data.frame(menu)
  col_off <- menu[menu[, 2] == idx, 3]
  col_off <- na.omit(col_off)
  dat <- readxl::read_excel(wb, 'data', skip = 4)
  model <- dat[, c(1:7, (col_off-1):(col_off+3))]
  model <- as.data.frame(model)
  colnames(model)[8:12] <- fact_nm
  return(model) 
}