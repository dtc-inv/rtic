#' @title Truncate holdings table to most recent date
#' @param tbl_hold data.frame of holdings with date in TimeStamp field
#' @return data.frame with holdings for the most recent date
#' @export
latest_holdings <- function(tbl_hold) {
  is_char <- "character" %in% class(tbl_hold$Timestamp) 
  if (is_char) {
    tbl_hold$TimeStamp <- as.Date(tbl_hold$TimeStamp)
  }
  is_latest <- tbl_hold$TimeStamp == max(tbl_hold$TimeStamp)
  if (is_char) {
    tbl_hold$TimeStamp <- as.character(tbl_hold$TimeStamp)
  }
  tbl_hold[is_latest, ]
}

#' @title Read Macro Select Model Workbook
#' @param wb full file name of workbook
#' @param fact_nm string vector representing the factor names
#' @param indx_nm name of index, e.g., "Russell 3000"
#' @return data.frame
#' @export
read_macro_wb <- function(wb, idx_nm) {
  menu <- readxl::read_excel(wb, 'menu')
  menu <- as.data.frame(menu)
  col_off <- menu[menu[, 2] == idx_nm, 3]
  col_off <- na.omit(col_off)
  dat <- readxl::read_excel(wb, 'data', skip = 4)
  model <- dat[, c(1:7, (col_off-1):(col_off+3))]
  model <- as.data.frame(model)
  return(model) 
}

#' @title Read Returns
#' @param ids ticker, cusip, lei, dtc name, or identifier
#' @param ac arcticdb datastore
#' @return xts of time-series of ids, will warn if some ids are not found
#' @export
read_ret <- function(ids, ac) {
  lib_ret <- ac$get_library("returns")
  lib_tbl <- ac$get_library("meta-tables")
  tbl_msl <- lib_tbl$read("msl")$data
  ids_dict <- filter(
    tbl_msl, 
      DtcName %in% ids | Ticker %in% ids | Cusip %in% ids | Sedol %in% ids | 
      Lei %in% ids |  Identifier %in% ids
  )
  found <- ids %in% ids_dict$DtcName | ids %in% ids_dict$Ticker | 
    ids %in% ids_dict$Cusip | ids %in% ids_dict$Lei | ids %in% ids_dict$Lei | 
    ids %in% ids_dict$Identifier
  if (all(!found)) {
    warning("no ids found")
    return(NULL)
  }
  if (any(!found)) {
    warning(paste0(ids[!found], " not found. "))
  }
  ret_src <- unique(na.omit(ids_dict$ReturnLibrary))
  ret_meta <- lib_tbl$read("ret-meta")$data
  ret_data <- left_join(data.frame(ReturnLibrary = ret_src),
                        ret_meta, by = "ReturnLibrary")
  res <- list()
  for (i in 1:length(ret_src)) {
    x_dict <- filter(ids_dict, ReturnLibrary %in% ret_src[i])
    record <- lib_ret$read(ret_src[i], columns = c("Date", x_dict$DtcName))
    res[[i]] <- dataframe_to_xts(record$data)
  }
  if ("monthly" %in% ret_data$Freq) {
    for (i in 1:length(res)) {
      res[[i]] <- change_freq(res[[i]])
    }
  }
  if (length(res) == 1) {
    return(res[[1]])
  } else {
    ret <- do.call("cbind", res)
    nm <- unlist(lapply(res, colnames))
    colnames(ret) <- nm
    
    return(ret)
  }
}

#' @title Space Out ids in multiple iterations
#' @param ids vector of ids
#' @param max_ids max number of ids per iteration
#' @details utility function for factset download that limits number of ids per
#'  api call
#' @return iteration index
#' @export
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
clean_ids <- function(tbl_hold) {
  if ("Cusip" %in% colnames(tbl_hold)) {
    ix <- tbl_hold[, "Cusip"] == "000000000"
    if (any(na.omit(ix))) {
      tbl_hold[ix, "Cusip"] <- NA
    }
  }
  return(tbl_hold)
}

#' @export
get_ids <- function(tbl_hold) {
  tbl_hold <- clean_ids(tbl_hold)
  id_field <- c("DtcName", "Ticker", "Cusip", "Sedol", "Lei", "Identifier")
  id_bool <- id_field %in% colnames(tbl_hold)
  if (!any(id_bool)) {
    stop("no id fields found")
  }
  tbl_id <- tbl_hold[, id_field[id_bool], drop = FALSE]
  ids <- tbl_id[, 1]
  if (ncol(tbl_id) > 1) {
    for (i in 2:ncol(tbl_id)) {
      ids[is.na(ids)] <- tbl_id[, i][is.na(ids)]
    }
  }
  ids <- unique(ids)
  return(ids)
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
create_ids <- function(tbl_msl) {
  ids <- tbl_msl$Isin
  ids[is.na(ids)] <- tbl_msl$Sedol[is.na(ids)]
  ids[is.na(ids)] <- tbl_msl$Lei[is.na(ids)]
  ids[is.na(ids)] <- tbl_msl$Cusip[is.na(ids)]
  ids[is.na(ids)] <- tbl_msl$Ticker[is.na(ids)]
  ids[is.na(ids)] <- tbl_msl$Identifier[is.na(ids)]
  return(ids)
}

#' @export
fill_ix <- function(a, b) {
  if (length(a) == length(b)) {
    a[is.na(a)] <- b[is.na(a)]
    return(a)
  } else {
    warning("a and b were different lengths, returning a")
    return(a)
  }
}

#' @export
match_ids_dtc_name <- function(ids, tbl_msl) {
  incomps <- c(NA, "000000000", "N/A", "0")
  ix_dtc <- match(ids, tbl_msl$DtcName, incomparables = incomps)
  ix_ticker <- match(ids, tbl_msl$Ticker, incomparables = incomps)
  ix_isin <- match(ids, tbl_msl$Isin, incomparables = incomps)
  ix_cusip <- match(ids, tbl_msl$Cusip, incomparables = incomps)
  ix_sedol <- match(ids, tbl_msl$Sedol, incomparables = incomps)
  ix_lei <- match(ids, tbl_msl$Lei, incomparables = incomps)
  ix_id <- match(ids, tbl_msl$Identifier, incomparables = incomps)
  ix <- rep(NA, length(ids))
  ix <- fill_ix(ix, ix_dtc)
  ix <- fill_ix(ix, ix_cusip)
  ix <- fill_ix(ix, ix_isin)
  ix <- fill_ix(ix, ix_sedol)
  ix <- fill_ix(ix, ix_lei)
  ix <- fill_ix(ix, ix_ticker)
  ix <- fill_ix(ix, ix_id)
  return(ix)
}

#' @export
match_mult <- function(x, y, match_by) {
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  incomps <- c(NA, "000000000", "N/A", "0")
  ix <- rep(NA, nrow(x))
  for (i in 1:length(match_by)) {
    a <- try(x[, match_by[i]], silent = TRUE)
    if ("try-error" %in% class(a)) {
      warning(paste0(match_by[i], " not found in x"))
      a <- rep(NA, nrow(x))
    }
    b <- try(y[, match_by[i]], silent = TRUE)
    if ("try-error" %in% class(b)) {
      warning(paste0(match_by[i], " not found in y"))
      b <- rep(NA, nrow(y))
    }
    ix <- fill_ix(ix, match(a, b, incomparables = incomps))
  }
  return(ix)
}

#' @export
left_merge <- function(x, y, match_by, keep_x_dup_col = TRUE) {
  ix <- match_mult(x, y, match_by)
  if (keep_x_dup_col) {
    dup_col <- colnames(y) %in% colnames(x)
    tbl_union <- cbind(x, y[ix, !dup_col, drop = FALSE])
  } else {
    dup_col <- colnames(x) %in% colnames(y)
    tbl_union <- cbind(x[, !dup_col, drop = FALSE], y[ix, ])
  }
  tbl_inter <- tbl_union[!is.na(ix), ]
  tbl_miss <- tbl_union[is.na(ix), ]
  list(
    union = tbl_union,
    inter = tbl_inter,
    miss = tbl_miss
  )
}

#' @export
rob_rbind <- function(df1, df2) {
  if (nrow(df1) == 0) {
    return(df2)
  }
  if (nrow(df2) == 0) {
    return(df1)
  }
  nm_union <- unique(c(colnames(df1), colnames(df2)))
  df1_miss <- !nm_union %in% colnames(df1)
  df2_miss <- !nm_union %in% colnames(df2)
  df1[, nm_union[df1_miss]] <- NA
  df2[, nm_union[df2_miss]] <- NA
  df2 <- df2[, colnames(df1)]
  rbind(df1, df2)
}

#' @export
get_list_fld <- function(x, fld) {
  if (is.null(x[fld][[1]])) {
    return(NA)
  } else {
    x[fld]
  }
}


#' @export
remove_holding_dup <- function(tbl_hold, id = "Name") {
  is_dup <- duplicated(paste0(tbl_hold[, id], tbl_hold[, "TimeStamp"]))
  tbl_hold[!is_dup, ]
}

#' @export
avg_fina_ratio <- function(w, x, rm_negative = TRUE) {
  if (rm_negative) {
    x[x < 0] <- NA
  }
  is_miss <- is.na(x) | is.na(w)
  x[is_miss] <- NA
  w[is_miss] <- NA
  w <- w / sum(x, na.rm = TRUE)
  wgt_harmonic_mean(w, x)
} 

#' @export
wgt_avg <- function(w, x) {
  is_miss <- is.na(x) | is.na(w)
  x[is_miss] <- NA
  w[is_miss] <- NA
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

#' @title Read MSL from Database
#' @param ac ArcticDB object
#' @return data.frame of MSL
#' @export
read_msl <- function(ac) {
  lib <- ac$get_library("meta-tables")
  lib$read("msl")$data
}

#' @title Check Holdings Table Specification
#' @param tbl_hold data.frame with holdings, need id, CapWgt, and TimeStamp
#' @export
check_tbl_hold <- function(tbl_hold) {
  if (!"data.frame" %in% class(tbl_hold)) {
    stop("holdings table is not a data.frame")
  }
  id_check <- any(c("DtcName", "Ticker", "Cusip", "Sedol", "Isin", "Lei",
                    "Identifier") %in% colnames(tbl_hold))
  wgt_check <- "CapWgt" %in% colnames(tbl_hold)
  time_check <- "TimeStamp" %in% colnames(tbl_hold)
  if (!id_check | !wgt_check | !time_check) {
    stop("Holdings table not properly specified. Need id, weight, and date")
  }
}


drill_down <- function(ac, tbl_hold, layer = 1, latest = TRUE) {
  
}
