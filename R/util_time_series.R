# truncate ----

#' @title Truncate time-series with start and end dates
#' @param x xts object
#' @param date_start optional beginning date to truncate time-series
#' @param date_end optional ending date to truncate time-series
#' @details
#' If neither date_start or date_end are entered the original
#' time series, x, will be returned. The date_start and date_end
#' parameters are inclusive, i.e., the date_start will be the first
#' date in the time-series.
#' @examples
#' \dontrun{
#' data(etf_ret)
#' # cut time-series to start in Feb 1, 2010 and end on June 30, 2010
#' cut_time(etf_ret, '2010-02-01', '2010-06-30')
#' # leave beginning of time-serie as is, end on June 30, 2010
#' cut_time(etf_ret, NULL, '2010-06-30')
#' }
#' @export
cut_time <- function(x, date_start = NULL, date_end = NULL) {
  if (is.null(date_start) & is.null(date_end)) {
    return(x)
  }
  if (is.null(date_start)) {
    return(x[paste0('/', date_end)])
  }
  if (is.null(date_end)) {
    return(x[paste0(date_start, '/')])
  }
  return(x[paste0(date_start, '/', date_end)])
}

#' @title Find first common start date
#' @export
first_comm_start <- function(x) {
  first_days <- rep(NA, ncol(x))
  for (i in 1:ncol(x)) {
    first_days[i] <- zoo::index(na.omit(x[, i]))[1]
  }
  return(max(as.Date(first_days, origin = '1970-01-01')))
}

#' @title Find last common end date
#' @export
last_comm_end <- function(x) {
  last_days <- rep(NA, ncol(x))
  for (i in 1:ncol(x)) {
    last_days[i] <- zoo::index(na.omit(x[, i]))[nrow(na.omit(x[, i]))]
  }
  return(min(as.Date(last_days, origin = "1970-01-01")))
}

#' @title Fill Missing Price Data with Last Found Observation
#' @param x xts
#' @details
#' For any missing data in a price time-series, found with `is.na`, the
#' last known price will be forward filled. E.g., for a monthly time-series
#' if Feb is missing than the Jan price will be filled in Feb.
#' @returns xts with missing data filled
#' @export
fill_na_price <- function(x) {
  x_fill <- apply(x, 2, na_price)
  xts(x_fill, zoo::index(x))
}

#' @title Fill Missing Price Data for a Vector
#' @param x vector of price data
#' @details see `fill_na_price`, which applies this function to an xts
#' @export
na_price <- function(x) {
  ind <- which(!is.na(x))
  if (is.na(x[1])) {
    ind <- c(1, ind)
  }
  rep(x[ind], times = diff(c(ind, length(x) + 1)))
}

# dataframe / xts conversion ----

#' @title Convert dataframe to xts
#' @export
dataframe_to_xts <- function(df) {
  ix <- is.na(df[[1]])
  df <- df[!ix, ]
  res <- xts(df[, -1], as.Date(df[[1]]))
  colnames(res) <- colnames(df)[-1]
  res
}

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

#' @title Convert xts object to a data.frame
#' @param x xts object
#' @return data.frame with Dates in the first column
#' @export
xts_to_dataframe <- function(x) {
  date_vec <- zoo::index(x, origin = '1970-01-01')
  df <- data.frame(Date = as.Date(date_vec), x, row.names = NULL)
  if (!is.null(colnames(x))) {
    colnames(df)[2:ncol(df)] <- colnames(x)
  }
  return(df)
}

#' @title Convert matrix to xts object
#' @param x matrix with dates in first column
#' @return xts
#' @export
mat_to_xts <- function(x) {
  xts(x[, -1], as.Date(x[[1]]))
}

#' @title Convert xts to tidy (long) data.frame
#' @param x xts
#' @export
xts_to_tidy <- function(x) {
  xdf <- xts_to_dataframe(x)
  tidyr::pivot_longer(xdf, -Date)
}

# combine xts ----

#' @title Row bind xts
#' @param new older time-series
#' @param old newer time-series
#' @param is_xts boolean, if set to FALSE, assumes new and old are data.frames
#'   with `Date` column
#' @param backfill if set to TRUE missing values of new will be filled with old
#' @details
#' For any overlapping dates, the old xts will be overwritten by the new xts.
#' The function will align the intersection of column names for the old and new
#' xts objects. For any column names that do not intersect NAs will be added
#' to the new or old xts depending on which xts is missing the column name(s).
#' @export
xts_rbind <- function(new, old, is_xts = TRUE, backfill = FALSE) {
  if (is_xts) {
    new_df <- xts_to_dataframe(new)
    old_df <- xts_to_dataframe(old)
  } else {
    new_df <- new
    old_df <- old
  }
  new_ldf <- pivot_longer(new_df, cols = -Date, names_to = "name",
                          values_to = "value")
  old_ldf <- pivot_longer(old_df, cols = -Date, names_to = "name",
                          values_to = "value")
  if (backfill) {
    new_ldf <- na.omit(new_ldf)
  }
  new_id <- paste0(new_ldf$Date, new_ldf$name)
  old_id <- paste0(old_ldf$Date, old_ldf$name)
  # find new ids place in old ids, replace with new values
  ix <- na.omit(match(new_id, old_id))
  if (length(ix) > 0) {
    old_ldf <- old_ldf[-ix, ]
  }
  combo <- rbind(new_ldf, old_ldf)
  combo_w <- pivot_wider(combo, id_cols = Date, names_from = name,
                         values_from = value)
  return(dataframe_to_xts(combo_w))
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


#' @title Column bind the intersection of two xts objects
#' @param x xts object
#' @param y xts object
#' @details
#' The first common start date and last common end date are used to find the
#' interection of returns. The returns are also cleaned for missing columns.
#' See rm_na_col for more info.
#' @return list containing intersection of returns and columns that were removed
#'   due to too many missing values (if any)
#' @export
xts_cbind_inter <- function(x, y) {
  combo <- xts_cbind(x, y)
  res <- clean_ret(combo)
  return(res)
}

# price to ret conversion ----

#' @title Convert price time-series to a return
#' @param x xts of prices
#' @details
#' Return is defined by period over period change
#' @return xts of returns
#' @export
price_to_ret <- function(x) {
  ret <- x / lag.xts(x, 1) - 1
  ret[2:nrow(ret), ]
}

#' @title Convert returns to a price index
#' @param x xts of returns
#' @return a price index calculating from \code{x} and a initial value of 1
#' @export
ret_to_price <- function(x) {

  price <- apply(x + 1, 2, cumprod)
  first_row <- xts(matrix(1, ncol = ncol(x)), zoo::index(x)[1] - 1)
  price_out <- rbind(first_row, price)
  colnames(price_out) <- colnames(x)
  return(price_out)
}

# trading days and freq ----

#' @title Get US Trading Days based on NYSE Holidays
#' @param date_start beginning date of sequence
#' @param date_end last date of sequence
#' @return sequence of trading days
#' @export
us_trading_days <- function(date_start = NULL, date_end = NULL) {
  if (is.null(date_start)) date_start <- as.Date('1970-01-01')
  if (is.null(date_end)) date_end <- Sys.Date()
  all_days <- seq.Date(date_start, date_end, 'days')
  year_start <- lubridate::year(date_start)
  year_end <- lubridate::year(date_end)
  holidays <- timeDate::holidayNYSE(year_start:year_end)
  busday <- timeDate::isBizday(timeDate::timeDate(all_days), as.Date(holidays@Data))
  all_days[busday]
}


#' @title Get Last U.S. Trading Day
#' @export
last_us_trading_day <- function() {
  yr <- lubridate::year(Sys.Date())
  bizdays::create.calendar('cal', holidays = timeDate::holidayNYSE((yr-1):yr),
                           weekdays = c('saturday', 'sunday'))
  bizdays::adjust.previous(Sys.Date() - 1, 'cal')
}


#' @title Change time-series frequency
#' @param x xts object
#' @param period character string of the desired time-series periods
#' @param dtype character string of "return" or "price" to represent the data type
#' @return xts object with new frequency
#' @importFrom lubridate ceiling_date
#' @export
change_freq <- function(x, period = 'months', dtype = c('return', 'price')) {
  x[is.na(x)] <- 0
  dtype <- tolower(dtype[1])
  if (dtype == 'return') {
    price <- ret_to_price(x)
  } else {
    price <- x
  }
  eo <- endpoints(price, on = period)
  price_new_freq <- price[eo, ]
  if (dtype == 'return') {
    data_out <- price_to_ret(price_new_freq)
  } else {
    data_out <- price_new_freq
  }
  if (tolower(period) %in% c('months', 'quarters', 'years')) {
    zoo::index(data_out) <- lubridate::ceiling_date(zoo::index(data_out),
                                                    'months') - 1
  }
  is_miss <- data_out[data_out == 0] <- NA
  return(data_out)
}

#' @title Change Character Frequency into Numeric Scaler
#' @param period days, weeks, months, quarters, years
#' @return corresponding numeric value, e.g., months = 12
#' @export
freq_to_scaler <- function(period) {
  switch(tolower(period),
         days = 252,
         weeks = 52,
         months = 12,
         quarters = 4,
         years = 1
  )
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


# read in xts from Excel ----

#' @title Read excel time-series
#' @param wb workbook full file name, e.g.,
#'   'C:/users/asotolongo/documents/wb.xslx'
#' @param sht worksheet number or name
#' @param skip number of header rows to skip, default is 0
#' @details
#' See `readxl::read_excel` for more info. Format of workbook needs to have
#'   date column in first row.
#' @export
read_xts <- function(wb, sht = 1, skip = 0) {
  dat <- readxl::read_excel(wb, sheet = sht, col_types = 'numeric', skip = skip)
  xts(dat[, -1], as.Date(dat[[1]], origin = '1899-12-30'))
}

# cleaning ----

#' @title Get Intersection of Asset, Benchmark, and Risk-free time-series
#' @param x asset time-series, xts, multiple columns accepted
#' @param b benchmark time-series, xts, mutliple columns accepted
#' @param rf risk-free time-series, optional, one column accepted
#' @return list with `x`, `b`, and `rf` aligned for common dates
#' @export
clean_asset_bench_rf <- function(x, b, rf = NULL, freq = NULL, 
                                 date_start = NULL, date_end = NULL) {
  if (is.null(freq)) {
    freq <- sort(c(guess_freq(x), guess_freq(b), guess_freq(rf)), na.last = TRUE,
                 decreasing = TRUE)
  }
  if (freq[1] == "M") {
    x <- change_freq(x)
    b <- change_freq(b)
    if (!is.null(rf)) {
      rf <- change_freq(rf)
    }
  }
  combo <- xts_cbind_inter(x, b)
  if (!is.null(colnames(combo$miss_ret))) {
    if (colnames(combo$miss_ret) == colnames(b)) {
      stop('benchmark is missing')
    }
  }
  if (!is.null(rf)) {
    combo <- xts_cbind_inter(combo$ret, rf)
    if (!is.null(colnames(combo$miss_ret))) {
      if (colnames(combo$miss_ret) == colnames(rf)) {
        stop('rf is missing')
      }
    }
  }
  if (!is.null(date_start)) {
    combo$ret <- cut_time(combo$ret, date_start = date_start)
  }
  if (!is.null(date_end)) {
    combo$ret <- cut_time(combo$ret, date_end = date_end)
  }
  res <- list()
  if (!is.null(rf)) {
    res$rf <- combo$ret[, colnames(combo$ret) %in% colnames(rf)]
  } else {
    res$rf <- NULL
  }
  res$b <- combo$ret[, colnames(combo$ret) %in% colnames(b)]
  res$x <- combo$ret[, colnames(combo$ret) %in% colnames(x)]
  return(res)
}

#' @export
clean_ret <- function(x, trunc_start = TRUE, trunc_end = TRUE, eps = 0.05) {
  if (trunc_start) {
    x <- x[paste0(first_comm_start(x), "/")]
  }
  if (trunc_end) {
    x <- x[paste0("/", last_comm_end(x))]
  }
  miss_col <- colSums(is.na(x)) > floor(eps * nrow(x))
  x[is.na(x)] <- 0
  if (all(miss_col)) {
    ret <- xts()
  } else {
    ret <- x[, !miss_col]
  }
  if (!all(miss_col)) {
    miss <- xts()
  } else {
    miss <- x[, miss_col]
  }
  res <- list()
  res$ret <- ret
  res$miss <- miss
  return(res)
}

#' @export
ret_date_info <- function(x) {
  sdate <- rep(NA, ncol(x))
  edate <- sdate
  for (i in 1:ncol(x)) {
    dt <- zoo::index(na.omit(x[, i]))
    sdate[i] <- dt[1]
    edate[i] <- dt[length(dt)]
  }
  nm <- colnames(x)
  if (is.null(nm)) {
    nm <- 1:ncol(x)
  }
  data.frame(Name = nm, Start = as.Date(sdate), End = as.Date(edate))
}
