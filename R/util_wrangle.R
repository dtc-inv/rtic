#' @title Truncate holdings table to most recent date
#' @param tbl_hold data.frame of holdings with date in TimeStamp field
#' @return data.frame with holdings for the most recent date
#' @export
latest_holdings <- function(tbl_hold) {
  tbl_hold$TimeStamp <- as.Date(tbl_hold$TimeStamp)
  is_latest <- tbl_hold$TimeStamp == max(tbl_hold$TimeStamp)
  tbl_hold[is_latest, ]
}
