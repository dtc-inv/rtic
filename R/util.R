#' @title Space Out ids in multiple iterations
#' @param ids vector of ids
#' @param max_ids max number of ids per iteration
#' @details utility function for factset download that limits number of ids per
#'  api call
#' @return iteration index
space_ids <- function(ids, max_ids = 50) {
  if (length(ids) > max_ids) {
    iter <- iter <- seq(1, length(ids), (max_ids - 1))
    if (iter[length(iter)] < length(ids)) {
      iter <- c(iter, length(ids))
    }
    ret_list <- list()
  } else {
    iter <- c(1, length(ids))
  }
  return(iter)
}

#' @export
extract_list <- function(x, nm) {
  y <- lapply(x, '[[', nm)
  y[sapply(y, is.null)] <- NA
  unlist(y)
}

#' @export
list_replace_null <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' @export
month_end <- function(dt) {
  lubridate::ceiling_date(as.Date(dt), 'months') - 1
}
