#' @title Update index returns from Factset
#' @param ac ArcticDB datastore, see create_arctic()
#' @param tbl_msl master security list data.frame
#' @param ids leave NULL to update all indexes, or enter specific index
#'   ids to update, note ids will have to first exist in the Master Security
#'   List `tbl_msl`
#' @param api_keys list with keys
#' @param t_minus_m how many months to download
#' @export
ret_index <- function(ac, tbl_msl, api_keys, ids = NULL, t_minus_m = 1) {
  if (is.null(ids)) {
    idx <- filter(tbl_msl, ReturnLibrary == "index")
    ids <- idx$Ticker
  }
  res <- list()
  is_miss <- rep(FALSE, length(ids))
  for (i in 1:length(ids)) {
    dat <- try(download_fs_ra_ret(ids[i], api_keys, t_minus_m, "D"))
    if ("try-error" %in% class(dat)) {
      is_miss[i] <- TRUE
    } else {
      res[[i]] <- dat
    }
  }
  dtc_name <- filter(tbl_msl, Ticker %in% na.omit(ids))$DtcName[!is_miss]
  dtc_name <- na.omit(dtc_name)
  ret <- do.call("cbind", res)
  colnames(ret) <- dtc_name
  lib <- ac$get_library("returns")
  old_dat <- lib$read("index")$data
  new_dat <- xts_to_dataframe(ret)
  combo <- xts_rbind(new_dat, old_dat, FALSE)
  combo_df <- xts_to_dataframe(combo)
  combo_df$Date <- as.character(combo_df$Date)
  lib$write("index", combo_df)
}

#' @title Update CTF Returns from Factset
#' @param ac ArcticDB datastore, see create_arctic()
#' @param tbl_msl master security list data.frame
#' @param ids leave `NULL` to update all CTFs or enter a vector of ids to
#'   only update specific CTFs
#' @param api_keys list with keys
#' @param t_minus_m integer to indicate how many months back to download new
#'   data from
#' @export
ret_ctf_monthly <- function(ac, tbl_msl, api_keys, ids = NULL, t_minus_m = 1) {
  if (is.null(ids)) {
    ctf <- filter(tbl_msl, ReturnLibrary == "ctf")
    ids <- ctf$Isin
  }
  res <- list()
  is_miss <- rep(FALSE, length(ids))
  for (i in 1:length(ids)) {
    dat <- try(download_fs_ra_ret(ids[i], api_keys, t_minus_m, "M"))
    if ("try-error" %in% class(dat)) {
      is_miss[i] <- TRUE
    } else {
      res[[i]] <- dat
    }
  }
  dtc_name <- filter(tbl_msl, Isin %in% ids)$DtcName[!is_miss]
  dtc_name <- na.omit(dtc_name)
  ret <- do.call("cbind", res)
  colnames(ret) <- dtc_name
  lib <- ac$get_library("returns")
  old_dat <- lib$read("ctf")$data
  new_dat <- xts_to_dataframe(ret)
  combo <- xts_rbind(new_dat, old_dat, FALSE)
  combo_df <- xts_to_dataframe(combo)
  combo_df$Date <- as.character(combo_df$Date)
  lib$write("ctf", combo_df)
}
