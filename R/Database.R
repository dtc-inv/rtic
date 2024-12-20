#' Database Object
#'
#' @description
#'   Download and store holdings and price time-series data in s3 from various
#'   sources such as factset, blackdiamond, excel
#' @export
Database <- R6::R6Class(
  'Database',
  public = list(
    #' @field api_keys of api keys
    api_keys = NULL,
    #' @field bucket aws s3 bucket
    bucket = NULL,
    #' @field ac ArcticDB object store
    ac = NULL,
    #' @field tbl_msl master security list: table of meta data for all investments
    tbl_msl = NULL,
    #' @field tbl_cust custodian list: table of meta data for CTFs and SMAs
    tbl_cust = NULL,
    #' @field tbl_sec SEC list: table of meta data for mutual funds and ETFs
    tbl_sec = NULL,
    #' @field tbl_xl_mod excel model list: table of meta data for model portfolios
    tbl_xl_mod = NULL,
    #' @field tbl_hold_field holdings fields: table of fields / columns for
    #'  holdings data
    tbl_hold_field = NULL,

    #' @description Create a db object
    #' @param api_keys list of api keys or `.RData` file location to load the list
    #'   of keys
    #' @param py_loc optional file path where python is installed to be used
    #'   with `reticulate`
    initialize = function(api_keys, py_loc) {

      if (!is.list(api_keys)) {
        rdat_file <- grep(".RData", api_keys, fixed = TRUE)
        if (length(rdat_file) == 0) {
          rdat_file <- FALSE
        }
        if (rdat_file) {
          load(api_keys)
        }
      }
      self$check_api_keys(api_keys)
      self$api_keys <- api_keys
      bucket <- arrow::s3_bucket(
        "dtc-inv",
        access_key = api_keys$s3$access_key,
        secret_key = api_keys$s3$secret_key
      )
      use_python(py_loc)
      adb <- import("arcticdb")
      base_url <- 's3://s3.us-east-1.amazonaws.com:dtc-rtic?'
      s3_url <- paste0(
        base_url,
        "access=", api_keys$s3$access_key,
        "&secret=", api_keys$s3$secret_key
      )
      ac <- adb$Arctic(s3_url)
      self$bucket <- bucket
      self$ac <- ac
      self$tbl_msl <- read_parquet(self$bucket$path("tables/tbl_msl.parquet"))
      self$tbl_cust <- read_parquet(self$bucket$path("tables/tbl_cust.parquet"))
      self$tbl_sec <- read_parquet(self$bucket$path("tables/tbl_sec.parquet"))
      self$tbl_xl_mod <- NULL # place holder
    },

    #' @description Check if api keys are properly specified
    #' @param api_keys list of api_keys
    check_api_keys = function(api_keys) {
      if (all(c("s3", "fs") %in% names(api_keys))) {
        return(api_keys)
      } else {
        stop("s3 and/or fs not found in api_keys")
      }
    },

    # update returns ----

    #' @description Update index returns from Factset
    #' @param ids leave NULL to update all indexes, or enter specific index
    #'   ids to update, note ids will have to first exist in the Master Security
    #'   List `tbl_msl`
    #' @param t_minus how many months to download
    ret_index = function(ids = NULL, t_minus = 1) {
      if (is.null(ids)) {
        idx <- filter(self$tbl_msl, ReturnLibrary == "index")
        ids <- idx$Ticker
      }
      res <- list()
      is_miss <- rep(FALSE, length(ids))
      for (i in 1:length(ids)) {
        dat <- try(download_fs_ra_ret(ids[i], self$api_keys, t_minus, "D"))
        if ("try-error" %in% class(dat)) {
          is_miss[i] <- TRUE
        } else {
          res[[i]] <- dat
        }
      }
      dtc_name <- filter(self$tbl_msl, Ticker %in% ids)$DtcName[!is_miss]
      dtc_name <- na.omit(dtc_name)
      ret <- do.call("cbind", res)
      colnames(ret) <- dtc_name
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("index")$data
      new_dat <- xts_to_dataframe(ret)
      combo <- xts_rbind(new_dat, old_dat, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write("index", combo_df)
    },

    #' @description Update ETF returns from Factset
    #' @param ids leave `NULL` to udpate all ETFs in the Master Security List,
    #'   or enter a vector to only update specific ETFs
    #' @param date_start beginning date for update, if left `NULL` will
    #'   default to the last date of the existing data
    #' @param date_end ending date for update, by default is yesterday
    ret_etf = function(ids = NULL, date_start = NULL, date_end = Sys.Date()-1) {
      if (is.null(ids)) {
        etf <- filter(self$tbl_msl, ReturnLibrary == "etf")
        ids <- etf$Ticker
      }
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("etf")$data
      if (is.null(date_start)) {
        date_start <- rownames(old_dat)[nrow(old_dat)]
      }
      iter <- space_ids(ids)
      xdf <- data.frame()
      for (i in 1:(length(iter)-1)) {
        json <- download_fs_global_prices(
          api_keys = self$api_keys,
          ids = ids[iter[i]:iter[i+1]],
          date_start = date_start,
          date_end = date_end,
          freq = "D"
        )
        xdf <- rob_rbind(xdf, flatten_fs_global_prices(json))
        print(iter[i])
      }
      xdf$DtcName <- etf$DtcName[match(xdf$RequestId, etf$Ticker)]
      is_dup <- duplicated(paste0(xdf$Date, xdf$DtcName))
      new_dat <- pivot_wider(xdf[!is_dup, ], id_cols = Date,
                             names_from = DtcName, values_from = TotalReturn)
      combo <- xts_rbind(new_dat, old_dat, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write("etf", combo_df)
    },

    #' @description Update Private Asset Indexes from Excel
    ret_private_index = function() {
      base <- "N:/Investment Team/DATABASES/CustomRet/PE-Downloads/"
      pe_q <- read_private_xts(
        paste0(base, "PrivateEquity.xlsx"),
        "Private Equity Index"
      ) |>
        unsmooth_ret()
      re_q <- read_private_xts(
        paste0(base, "PrivateRealEstateValueAdd.xlsx"),
        "Private Real Estate Value Add Index"
      ) |>
        unsmooth_ret()
      pc_q <- read_private_xts(
        paste0(base, "PrivateCredit.xlsx"),
        "Private Credit Index"
      ) |>
        unsmooth_ret()
      lib <- self$ac$get_library("returns")
      ind <- lib$read("index")
      ind <- dataframe_to_xts(ind$data)
      pe_m <- change_freq(na.omit(ind$`Russell 2000`))
      re_m <- change_freq(na.omit(ind$`Wilshire US REIT`))
      pc_m <- change_freq(na.omit(ind$`BofAML U.S. HY Master II`))
      pe <- monthly_spline(pe_m, pe_q)
      re <- monthly_spline(re_m, re_q)
      pc <- monthly_spline(pc_m, pc_q)
      colnames(pe) <- "Private Equity Index"
      colnames(re) <- "Private Real Estate Value Add Index"
      colnames(pc) <- "Private Credit Index"
      dat <- xts_cbind(pe, re)
      dat <- xts_cbind(dat, pc)
      xdf <- xts_to_dataframe(dat)
      xdf$Date <- as.character(xdf$Date)
      lib$write("private-index", xdf)
    },

    #' @description Update CTF Returns from Factset
    #' @param ids leave `NULL` to update all CTFs or enter a vector of ids to
    #'   only update specific CTFs
    #' @param t_minus integer to indicate how many months back to download new
    #'   data from
    ret_ctf_monthly = function(ids = NULL, t_minus = 1) {
      if (is.null(ids)) {
        ctf <- filter(self$tbl_msl, ReturnLibrary == "ctf")
        ids <- ctf$Isin
      }
      res <- list()
      is_miss <- rep(FALSE, length(ids))
      for (i in 1:length(ids)) {
        dat <- try(download_fs_ra_ret(ids[i], self$api_keys, t_minus, "M"))
        if ("try-error" %in% class(dat)) {
          is_miss[i] <- TRUE
        } else {
          res[[i]] <- dat
        }
      }
      dtc_name <- filter(self$tbl_msl, Isin %in% ids)$DtcName[!is_miss]
      dtc_name <- na.omit(dtc_name)
      ret <- do.call("cbind", res)
      colnames(ret) <- dtc_name
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("ctf")$data
      new_dat <- xts_to_dataframe(ret)
      combo <- xts_rbind(new_dat, old_dat, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write("ctf", combo_df)
    },

    ret_stock = function(ids = NULL, date_start = NULL, date_end = Sys.Date(),
                         freq = "D") {
      if (is.null(ids)) {
        stock <- filter(self$tbl_msl, ReturnLibrary == "stock")
        ids <- create_ids(stock)
      }
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("stock")$data
      if (is.null(date_start)) {
        date_start <- old_dat$Date[nrow(old_dat)]
      }
      iter <- space_ids(ids)
      xdf <- data.frame()
      for (i in 1:(length(iter)-1)) {
        json <- download_fs_global_prices(
          api_keys = self$api_keys,
          ids = ids[iter[i]:iter[i+1]],
          date_start = date_start,
          date_end = date_end,
          freq = freq
        )
        xdf <- rob_rbind(xdf, flatten_fs_global_prices(json))
        print(iter[i])
      }
      ix <- match_ids_dtc_name(xdf$RequestId, self$tbl_msl)
      dtc_name <- self$tbl_msl$DtcName[ix]
      xdf$DtcName <- dtc_name
      is_dup <- duplicated(paste0(xdf$DtcName, xdf$date))
      xdf <- xdf[!is_dup, ]
      new_dat <- pivot_wider(xdf, id_cols = date, values_from = totalReturn,
                             names_from = DtcName)

    },

    # holdings ----

    #' @description Download holdings from SEC EDGAR Database
    #' @param dtc_name leave `NULL` to download all, or enter a vector of
    #'   dtc_names to download specific funds
    #' @param user_email need to provide an email address to download
    #' @param save_to_db save data to DTC's database
    #' @param return_data return data.frame of holdings
    hold_sec = function(dtc_name = NULL,
                        user_email = "asotolongo@diversifiedtrust.com",
                        save_to_db = TRUE, return_data = FALSE) {
      lib <- self$ac$get_library("holdings")
      sec <- self$tbl_sec
      if (!is.null(dtc_name)) {
        sec <- filter(sec, DtcName %in% dtc_name)
        if (nrow(sec) == 0) {
          stop("dtc_names not found in SEC table")
        }
      } else {
        dtc_name <- sec$DtcName
      }
      res <- list()
      for (i in 1:length(dtc_name)) {
        print(paste0("working on ", dtc_name[i]))
        dat <- try(download_sec(sec$LongCIK[i], sec$ShortCIK[i], user_email))
        if ("try-error" %in% class(dat)) {
          warning(paste0("could not download ", dtc_name[i]))
          next
        }
        if (save_to_db) {
          dat$TimeStamp <- as.character(dat$TimeStamp)
          if (dtc_name[i] %in% lib$list_symbols()) {
            old_dat <- lib$read(dtc_name[i])
            if (dat$TimeStamp[1] %in% unique(old_dat$data$TimeStamp)) {
              warning(paste0(dtc_name[i], " already has data for latest date"))
              if (return_data) {
                res[[i]] <- dat
                next
              }
            }
            combo <- rob_rbind(old_dat$data, dat)
            lib$write(dtc_name[i], combo)
          } else {
            warning(
              paste0(
                dtc_name[i],
                " not found in library, creating new symbol"
              )
            )
            lib$write(dtc_name[i], dat)
          }
        }
        if (return_data) {
          res[[i]] <- dat
        }
      }
      if (return_data) {
        return(res)
      }
    },

    #' @description Download account data from custodian, currently done via
    #'   BlackDiamond
    #' @param dtc_name leave `NULL` to download all, or enter a vector of
    #'   dtc_names to download specific funds
    #' @param save_to_db save data to DTC's database
    #' @param return_data return data.frame of holdings
    hold_cust = function(dtc_name = NULL, save_to_db = TRUE,
                         return_data = FALSE) {
      lib <- self$ac$get_library("holdings")
      cust <- self$tbl_cust
      if (!is.null(dtc_name)) {
        cust <- filter(cust, DtcName %in% dtc_name)
        if (nrow(cust) == 0) {
          stop("dtc_names not found in custodian table")
        }
      } else {
        dtc_name <- cust$DtcName
      }
      res <- list()
      for (i in 1:length(dtc_name)) {
        print(paste0("working on ", dtc_name[i]))
        dat <- try(download_bd(cust$BdAccountId[i], self$api_keys))
        if ("try-error" %in% class(dat)) {
          warning(paste0("could not download ", dtc_name[i]))
          next
        }
        if (save_to_db) {
          dat$TimeStamp <- as.character(dat$TimeStamp)
          if (dtc_name[i] %in% lib$list_symbols()) {
            old_dat <- lib$read(dtc_name[i])
            if (dat$TimeStamp[1] %in% unique(old_dat$data$TimeStamp)) {
              warning(paste0(dtc_name[i], " already has data for latest date"))
              if (return_data) {
                res[[i]] <- dat
                next
              }
            }
            combo <- rob_rbind(old_dat$data, dat)
            lib$write(dtc_name[i], combo)
          } else {
            warning(
              paste0(
                dtc_name[i],
                " not found in library, creating new symbol"
              )
            )
            lib$write(dtc_name[i], dat)
          }
        }
        if (return_data) {
          res[[i]] <- dat
        }
      }
      if (return_data) {
        return(res)
      }
    },

    # Company data ----
    download_fundamental_data = function(
      ids = NULL, yrs_back = 1,
      dtype = c('PE', 'PB', 'PFCF', 'DY', 'ROE', 'MCAP')) {

      dtype <- dtype[1]
      if (dtype == 'PE') {
        formulas <- paste0('FG_PE(-', yrs_back, 'AY,NOW,CQ)')
      } else if (dtype == 'PB') {
        formulas <- paste0('FG_PBK(-', yrs_back, 'AY,NOW,CQ)')
      } else if (dtype == 'PFCF') {
        formulas <- paste0('FG_CFLOW_FREE_EQ_PS(-', yrs_back, 'AY,NOW,CQ,USD)')
      } else if (dtype == 'DY') {
        formulas <- paste0('FG_DIV_YLD(-', yrs_back, 'AY,NOW,CQ)')
      } else if (dtype == 'ROE') {
        formulas <- paste0('FG_ROE(-', yrs_back, 'AY,NOW,CQ)')
      } else if (dtype == 'MCAP') {
        formulas <- paste0('FF_MKT_VAL(ANN_R,-', yrs_back, 'AY,NOW,CQ,,USD)')
      } else {
        stop("dtype must be 'PE', 'PB', 'PFCF', 'DY', 'ROE', or 'MCAP'")
      }
      if (is.null(ids)) {
        stock <- filter(self$tbl_msl, ReturnLibrary == "stock")
        ids <- create_ids(stock)
      }
      ids <- gsub(" ", "", ids)
      iter <- space_ids(ids)
      xdf <- data.frame()
      for (i in 1:(length(iter)-1)) {
        json <- download_fs_formula(self$api_keys, ids[iter[i]:iter[i+1]],
                                    formulas)
        xdf <- rbind(xdf, flatten_fs_formula(json))
        print(iter[i])
      }
      ix <- match_ids_dtc_name(xdf$requestId, self$tbl_msl)
      dtc_name <- self$tbl_msl$DtcName[ix]
      xdf$DtcName <- dtc_name
      is_dup <- duplicated(paste0(xdf$DtcName, xdf$date))
      xdf <- xdf[!is_dup, ]
      colnames(xdf)[2:3] <- c(dtype, "Date")
      wdf <- pivot_wider(xdf, id_cols = Date, names_from = DtcName,
                         values_from = all_of(dtype))
      lib <- self$ac$get_library("co-data")
      old_data <- lib$read(dtype)
      combo <- xts_rbind(wdf, old_data$data, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write(dtype, combo_df)
    },

    download_cusip = function(ids = NULL) {
      if (is.null(ids)) {
        stock <- filter(self$tbl_msl, ReturnLibrary == "stock")
        ids <- stock$Isin
      }
      ids <- gsub(" ", "", ids)
      ids <- na.omit(ids)
      iter <- space_ids(ids)
      formulas <- "FSYM_CUSIP(0,'ID')"
      json <- download_fs_formula(self$api_keys, ids[1:10], formulas)
    }
  )
)

