#' @title Convert arctic data with columns as rownames into data.frame with
#'   Date column
#' @field dat dataframe with dates as rownames
#' @export
arc_to_dataframe <- function(dat) {
  dat$Date <- as.Date(x)
  dat
}

#' @title Convert dataframe with Date column to arctic storage dataframe with
#'   dates in rownames
#' @param xdf data.frame with Date column (as first column xdf[, 1])
#' @export
dataframe_to_arc <- function(xdf) {
  xdf$Date <- as.character(xdf$Date)
  xdf
}

#' @export
xts_to_arc <- function(x) {
  x <- xts_to_dataframe(x)
  dataframe_to_arc(x)
}

#' @title Column bind xts objects while preserving columns names
#' @param x xts object
#' @param y xts object
#' @details
#' Column names will get converted to `data.frame` formats when `cbind` is called
#' on the xts object. E.g., Small Cap will be Small.Cap. This method preserves
#' the spaces or special characters in the original column names.
#' @return xts with `cbind(x, y)` with original column names of `x` and `y`
#' @export
xts_cbind <- function(x, y) {
  col_nms <- c(colnames(x), colnames(y))
  combo <- cbind(x, y)
  colnames(combo) <- col_nms
  return(combo)
}

# formatting ----

#' @title Numeric to character percent
#' @export
f_percent <- function(x, d) {
  paste0(round(x * 100, d), '%')
}

#' @title Numeric to character number
#' @export
f_num <- function(x, d) {
  as.character(round(x, d))
}
