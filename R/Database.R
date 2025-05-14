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
    #' @field ac ArcticDB object store
    ac = NULL,
    #' @field tbl_msl master security list: table of meta data for all 
    #'   investments
    tbl_msl = NULL,
    #' @field tbl_cust custodian list: table of meta data for CTFs and SMAs
    tbl_cust = NULL,
    #' @field tbl_sec SEC list: table of meta data for mutual funds and ETFs
    tbl_sec = NULL,
    #' @field tbl_xl_mod excel model list: table of meta data for model 
    #'   portfolios
    tbl_xl_mod = NULL,
    #' @field tbl_hold_field holdings fields: table of fields / columns for
    #'  holdings data
    tbl_hold_field = NULL,

    #' @description Create a db object
    #' @param api_keys list of api keys or `.RData` file location to load the 
    #' list of keys
    #' @param py_loc optional file path where python is installed to be used
    #'   with `reticulate`
    initialize = function(api_keys, py_loc = NULL) {

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
      ac <- create_arctic(api_keys, py_loc = py_loc)
      self$ac <- ac
      lib <- ac$get_library("meta-tables")
      self$tbl_msl <- lib$read("msl")$data
      self$tbl_cust <- lib$read("cust")$data
      self$tbl_sec <- lib$read("sec")$data
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

    # tables ----
    #' @description Read MSL from Excel
    #' @param wb excel file full path
    #' @param write boolean to overwrite MSL in database, default FALSE
    read_msl_xl = function(wb = "N:/Investment Team/DATABASES/CustomRet/msl.xlsx") {
      prev_msl <- read_msl(self$ac)
      lib <- self$ac$get_library("meta-tables")
      lib$write("z-msl", prev_msl)
      col_types <- rep("text", 11)
      col_types[3] <- "numeric" 
      dat <- readxl::read_excel(wb, 1, col_types = col_types)
      self$tbl_msl <- dat
      self$write_msl(dat)
    },
    
    #' @description write MSL to database
    write_msl = function(tbl_msl) {
      lib <- self$ac$get_library("meta-tables")
      lib$write("msl", tbl_msl)
    },
    
    #' @description write SEC table to database
    write_sec = function() {
      lib <- self$ac$get_library("meta-tables")
      lib$write("sec", self$tbl_sec)
    },
    
    #' @description write Custodian table to database
    write_cust = function() {
      lib <- self$ac$get_library("meta-tables")
      lib$write("cust", self$tbl_cust)
    },
    
    #' @description Add an asset to the MSL and other meta tables
    #' @param dtc_name DtcName field
    #' @param id Ticker, Cusip, Sedol, Isin, Lei, or Identifier
    #' @param id_type specify which kind of id (above)
    #' @param layer 1 = lowest no holdings, 2 fund manager, 3 asset class, etc
    #' @param sec_type security type, e.g., etf, mutual-fund, index
    #' @param ret_lib return library name
    #' @param hold_tbl table with holdings (for models)
    #' @param short_cik if SEC holding, parent company CIK, otherwise leave NA
    #' @param long_cik if SEC holding, shareclass CIK, otherwise leave NA
    #' @param bd_acct_id if SMA black diamond account id, otherwise leave NA
    add_asset = function(dtc_name, id, id_type, layer, sec_type, ret_lib = NA, 
                         hold_tbl = NA, short_cik = NA, long_cik = NA,
                         bd_acct_id = NA) {
      if (!id_type %in% c("Ticker", "Cusip", "Sedol", "Isin", "Lei", 
                          "Identifier")) {
        stop("id type not found")
      }
      msl_row <- data.frame(
        DtcName = dtc_name,
        Ticker = NA,
        Layer = layer,
        Cusip = NA,
        Sedol = NA,
        Isin = NA,
        Lei = NA,
        Identifier = NA,
        SecType = sec_type,
        ReturnLibrary = ret_lib,
        HoldingsTable = hold_tbl
      )
      msl_row[1, id_type] <- id
      self$tbl_msl <- rbind(self$tbl_msl, msl_row)
      self$write_msl()
      if (!is.na(short_cik) & !is.na(long_cik)) {
        sec_row <- data.frame(
          DtcName = dtc_name,
          ShortCIK = short_cik,
          LongCIK = long_cik
        )
        self$tbl_sec <- rbind(self$tbl_sec, sec_row)
        self$write_sec()
      }
      if (!is.na(bd_acct_id)) {
        cust_row <- data.frame(
          DtcName = dtc_name,
          BdAccountId = bd_account_id
        )
        self$tbl_cust <- rbind(self$tbl_cust, cust_row)
        self$write_cust()
      }
    },
    
    # update returns ----

    #' @description Update index returns from Factset
    #' @param ids leave NULL to update all indexes, or enter specific index
    #'   ids to update, note ids will have to first exist in the Master Security
    #'   List `tbl_msl`
    #' @param t_minus_m how many months to download
    ret_index = function(ids = NULL, t_minus_m = 1) {
      if (is.null(ids)) {
        idx <- filter(self$tbl_msl, ReturnLibrary == "index")
        ids <- idx$Ticker
      }
      res <- list()
      is_miss <- rep(FALSE, length(ids))
      for (i in 1:length(ids)) {
        dat <- try(download_fs_ra_ret(ids[i], self$api_keys, t_minus_m, "D"))
        if ("try-error" %in% class(dat)) {
          is_miss[i] <- TRUE
        } else {
          res[[i]] <- dat
        }
      }
      ret <- do.call("cbind", res)
      dtc_name <- idx$DtcName[!is_miss]
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
      } else {
        etf <- filter(self$tbl_msl, Ticker %in% ids)
      }
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("etf")$data
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
          freq = "D"
        )
        xdf <- rob_rbind(xdf, flatten_fs_global_prices(json))
        print(iter[i])
      }
      xdf$DtcName <- etf$DtcName[match(xdf$RequestId, etf$Ticker)]
      is_dup <- duplicated(paste0(xdf$Date, xdf$DtcName))
      xdf$TotalReturn <- xdf$TotalReturn / 100
      new_dat <- pivot_wider(xdf[!is_dup, ], id_cols = Date,
                             names_from = DtcName, values_from = TotalReturn)
      combo <- xts_rbind(new_dat, old_dat, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write("etf", combo_df)
    },

    #' @description Update index returns from Factset
    #' @param ids leave NULL to update all mutual funds, or enter specific
    #'   mutual fund, note if ids are entered they must be in the MSL
    #' @param days_back how many days to download
    ret_mutual_fund = function(ids = NULL, days_back = 1) {
      if (is.null(ids)) {
        mf <- filter(self$tbl_msl, ReturnLibrary == "mutual-fund")
        ids <- mf$Ticker
      } else {
        mf <- filter(self$tbl_msl, Ticker %in% ids)
      }
      formulas <- paste0('P_TOTAL_RETURNC(-', days_back, 'D,NOW,D,USD)')
      iter <- space_ids(ids)
      xdf <- data.frame()
      for (i in 1:(length(iter)-1)) {
        json <- download_fs_formula(
          api_keys = self$api_keys,
          ids = ids[iter[i]:iter[i+1]],
          formulas = formulas
        )
        xdf <- rob_rbind(xdf, flatten_fs_formula(json))
      }
      colnames(xdf)[2] <- "TotalReturn"
      xdf$TotalReturn <- xdf$TotalReturn / 100
      xdf$DtcName <- mf$DtcName[match(xdf$requestId, mf$Ticker)]
      is_dup <- duplicated(paste0(xdf$date, xdf$DtcName))
      new_dat <- pivot_wider(xdf[!is_dup, ], id_cols = date,
                             names_from = DtcName, values_from = TotalReturn)
      colnames(new_dat)[1] <- "Date"
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("mutual-fund")$data
      combo <- xts_rbind(new_dat, old_dat, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write("mutual-fund", combo_df)
    },

    #' @description Update Private Asset Indexes from Excel
    ret_private_index = function() {
      base <- "N:/Investment Team/DATABASES/CustomRet/PE-Downloads/"
      pe_q <- read_private_xts(
        paste0(base, "PrivateEquity.xlsx"),
        "Private Equity Index"
      )
      re_q <- read_private_xts(
        paste0(base, "PrivateRealEstateValueAdd.xlsx"),
        "Private Real Estate Value Add Index"
      )
      reg_q <- read_private_xts(
        paste0(base, "PrivateRealEstate.xlsx"),
        "Private Real Estate Index"
      )
      pc_q <- read_private_xts(
        paste0(base, "PrivateCredit.xlsx"),
        "Private Credit Index"
      )
      lib <- self$ac$get_library("returns")
      ind <- lib$read("index")
      ind <- dataframe_to_xts(ind$data)
      pe_m <- change_freq(na.omit(ind$`Russell 2000`))
      re_m <- change_freq(na.omit(ind$`Wilshire US REIT`))
      pc_m <- change_freq(na.omit(ind$`BofAML U.S. HY Master II`))
      pe <- monthly_spline(pe_m, pe_q)
      re <- monthly_spline(re_m, re_q)
      reg <- monthly_spline(re_m, reg_q)
      pc <- monthly_spline(pc_m, pc_q)
      colnames(pe) <- "Private Equity Index"
      colnames(re) <- "Private Real Estate Value Add Index"
      colnames(reg) <- "Private Real Estate Index"
      colnames(pc) <- "Private Credit Index"
      dat <- xts_cbind(pe, re)
      dat <- xts_cbind(dat, reg)
      dat <- xts_cbind(dat, pc)
      xdf <- xts_to_dataframe(dat)
      xdf$Date <- as.character(xdf$Date)
      lib$write("private-index", xdf)
    },

    #' @description Update CTF Returns from Factset
    #' @param t_minus_m integer to indicate how many months back to download new
    #'   data from
    ret_ctf_monthly = function(t_minus_m = 1) {
      tbl_msl <- self$tbl_msl
      lib_meta <- self$ac$get_library("meta-tables")
      ctf <- lib_meta$read("ctf-meta")$data
      ids <- ctf$MonthlyId
      res <- list()
      is_miss <- rep(FALSE, length(ids))
      for (i in 1:length(ids)) {
        dat <- try(download_fs_ra_ret(ids[i], self$api_keys, t_minus_m, "M"))
        if ("try-error" %in% class(dat)) {
          is_miss[i] <- TRUE
        } else {
          res[[i]] <- dat
        }
      }
      dtc_name <- ctf$DtcName[!is_miss]
      ret <- do.call("cbind", res)
      colnames(ret) <- dtc_name
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("ctf-monthly")$data
      new_dat <- xts_to_dataframe(ret)
      combo <- xts_rbind(new_dat, old_dat, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write("ctf-monthly", combo_df)
    },
    
    ret_ctf_daily = function(t_minus_m = 1) {
      lib_meta <- self$ac$get_library("meta-tables")
      ctf_meta <- lib_meta$read("ctf-meta")$data
      is_daily <- !is.na(ctf_meta$DailyId)
      ctf_meta <- ctf_meta[is_daily, ]
      ids <- ctf_meta$DailyId
      is_found <- rep(TRUE, length(ids))
      res <- list()
      for (i in 1:length(ids)) {
        dat <- try(download_fs_ra_ret(ids[i], self$api_keys, t_minus_m, "D"))
        if (inherits(dat, "try-error")) {
          is_found[i] <- FALSE
        } else {
          res[[i]] <- dat
        }
      }
      dtc_name <- ctf_meta$DtcName[is_found]
      ret <- do.call("cbind", res)
      colnames(ret) <- dtc_name
      old_dat <- lib$read("ctf-daily")
      new_dat <- xts_to_dataframe(ret)
      combo <- xts_rbind(new_dat, old_dat, FALSE)
      combo_df <- xts_to_dataframe(combo)
      combo_df$Date <- as.character(combo_df$Date)
      lib$write("ctf-daily", combo_df)
    },
    
    ret_ctf_daily_adj = function() {
      lib_meta <- self$ac$get_library("meta-tables")
      lib_ret <- self$ac$get_library("returns")
      ctf_meta <- lib_meta$read("ctf-meta")$data
      daily <- lib_ret$read("ctf-daily")$data
      daily <- dataframe_to_xts(daily)
      monthly <- lib_ret$read("ctf-monthly")$data
      monthly <- dataframe_to_xts(monthly)
      for (i in 1:ncol(daily)) {
        d <- daily[, i]
        d <- clean_ret(d, eps = 1)
        d <- d$ret
        ix <- colnames(monthly) %in% colnames(d)
        if (sum(ix) == 0) {
          warning(paste0(colnames(d), " not found in monthly returns"))
          next
        }
        m <- monthly[, ix]
        res <- try(daily_spline(d, m))
        if (inherits(res, "try-error")) {
          warning(paste0(colnames(d), " did not spline"))
          next
        }
        daily[, i] <- res
      }
      lib_ret$write("ctf-daily", xts_to_arc(daily))
    },
    
    #' @description read in manually uploaded returns from Excel
    #' @param xl_file full file path of excel workbook
    ret_workup = function(
      xl_file = "N:/Investment Team/DATABASES/CustomRet/workup.xlsx") {
      
      dat <- read_xts(xl_file)
      dat <- xts_to_arc(dat)
      lib <- self$ac$get_library("returns")
      lib$write("workup", dat)
    },

    #' @description read in returns to backfill daily and monthly returns
    ret_backfill = function() {
      xl_file <- "N:/Investment Team/DATABASES/CustomRet/backfill-daily.xlsx"
      backfill <- read_xts(xl_file)
      backfill <- xts_to_dataframe(backfill)
      backfill$Date <- as.character(backfill$Date)
      lib <- self$ac$get_library("returns")
      lib$write("backfill-daily", backfill)
      xl_file <- "N:/Investment Team/DATABASES/CustomRet/backfill-monthly.xlsx"
      backfill <- read_xts(xl_file)
      backfill <- xts_to_dataframe(backfill)
      backfill$Date <- as.character(backfill$Date)
      lib$write("backfill-monthly", backfill)
    },
    
    #' @description execute backfill
    #' @param dtc_name name of return to backfill
    run_backfill = function() {
      lib_mt <- self$ac$get_library("meta-tables")
      ret_meta <- lib_mt$read("ret-meta")$data
      lib <- self$ac$get_library("returns")
      daily <- lib$read("backfill-daily")$data
      dd <- filter(self$tbl_msl, DtcName %in% colnames(daily)[-1])
      if (nrow(dd > 0)) {
        ret_lib <- na.omit(unique(dd$ReturnLibrary))
      }
      if (length(ret_lib) > 0) {
        ret_lib <- data.frame(ReturnLibrary = ret_lib)
        res <- left_merge(ret_lib, ret_meta, "ReturnLibrary")
        ret_lib <- res$inter
        if (any(ret_lib$Freq != "daily")) {
          print(ret_lib)
          stop("daily backfill for wrong frequency")
        }
        for (i in 1:nrow(ret_lib)) {
          x <- filter(dd, ReturnLibrary %in% ret_lib$ReturnLibrary[i])
          new <- lib$read(ret_lib$ReturnLibrary[i])$data
          combo <- xts_rbind(new, daily[, c("Date", x$DtcName)], FALSE, TRUE)
          combo <- xts_to_arc(combo)
          lib$write(ret_lib$ReturnLibrary[i], combo)
        }
      }
      monthly <- lib$read("backfill-monthly")$data
      dd <- filter(self$tbl_msl, DtcName %in% colnames(monthly)[-1])
      if (nrow(dd > 0)) {
        ret_lib <- na.omit(unique(dd$ReturnLibrary))
      }
      if (length(ret_lib) > 0) {
        ret_lib <- data.frame(ReturnLibrary = ret_lib)
        res <- left_merge(ret_lib, ret_meta, "ReturnLibrary")
        ret_lib <- res$inter
        if (any(ret_lib$Freq != "monthly")) {
          print(ret_lib)
          stop("monthly backfill for wrong frequency")
        }
        for (i in 1:nrow(ret_lib)) {
          x <- filter(dd, ReturnLibrary %in% ret_lib$ReturnLibrary[i])
          new <- lib$read(ret_lib$ReturnLibrary[i])$data
          combo <- xts_rbind(new, monthly[, c("Date", x$DtcName)], FALSE, TRUE)
          combo <- xts_to_arc(combo)
          lib$write(ret_lib$ReturnLibrary[i], combo)
        }
      }
    },
    
    #' @description Update returns of models
    #' @param dtc_name option to specify specific models to update, leave NULL
    #'   to update all models
    ret_model = function(dtc_name = NULL, months_back = 1) {
      lib_ret <- self$ac$get_library("returns")
      msl <- read_msl(self$ac)
      lib <- self$ac$get_library("meta-tables")
      model <- lib$read("model")$data
      if (!is.null(dtc_name)) {
        model <- filter(model, DtcName %in% dtc_name)
      }
      d <- filter(model, ReturnLibrary == "model-daily")
      d_id <- filter(msl, DtcName %in% d$DtcName)
      if (nrow(d_id) > 0) {
        dat <- list()
        found <- rep(TRUE, nrow(d_id))
        for (i in 1:nrow(d_id)) {
          x <- try(download_fs_ra_ret(d_id$Identifier[i], self$api_keys, 
                                      months_back))
          if ("try-error" %in% class(x)) {
            found[i] <- FALSE
          } else {
            dat[[i]] <- x
          }
        }
        r <- do.call(cbind, dat)
        colnames(r) <- d_id$DtcName[found]
        new <- xts_to_arc(r)
        old <- lib_ret$read("model-daily")$data
        combo <- xts_rbind(new, old, FALSE)
        lib_ret$write("model-daily", xts_to_arc(combo))
      }
      m <- filter(model, ReturnLibrary == "model-monthly")
      m_id <- filter(msl, DtcName %in% m$DtcName)
      if (nrow(m_id) > 0) {
        dat <- list()
        found <- rep(TRUE, nrow(m_id))
        for (i in 1:nrow(m_id)) {
          x <- try(download_fs_ra_ret(m_id$Identifier[i], self$api_keys, 
                                      months_back, "M"))
          if ("try-error" %in% class(x)) {
            found[i] <- FALSE
          } else {
            dat[[i]] <- x
          }
        }
        r <- do.call(cbind, dat)
        colnames(r) <- m_id$DtcName[found]
        new <- xts_to_arc(r)
        old <- lib_ret$read("model-monthly")$data
        combo <- xts_rbind(new, old, FALSE)
        lib_ret$write("model-monthly", xts_to_arc(combo))
      }
      
      # model <- model[order(model$Layer), ]
      # ret_m <- list()
      # ret_d <- list()
      # for (i in 1:nrow(model)) {
      #   h <- self$read_hold(model$DtcName[i], FALSE)
      #   p <- Portfolio$new(self$ac, h)
      #   p$init_rebal(model$RebFreq[i], model$RetFreq[i], clean_ret = FALSE)
      #   colnames(p$rebal$rebal_ret) <- model$DtcName[i]
      #   if (model$ReturnLibrary[i] == "model-daily") {
      #     ret_d[[length(ret_d)+1]] <- p$rebal$rebal_ret
      #   } else if (model$ReturnLibrary[i] == "model-monthly") {
      #     ret_m[[length(ret_m)+1]] <- p$rebal$rebal_ret
      #   } else {
      #     warning(paste0(model$DtcName[i], " invalid ReturnLibrary"))
      #     next
      #   }
      # }
      # ret_lib <- self$ac$get_library("returns")
      # if (length(ret_d) >= 1) {
      #   colnm <- unlist(lapply(ret_d, "colnames"))
      #   ret_d <- do.call("cbind", ret_d)
      #   colnames(ret_d) <- colnm
      #   old <- ret_lib$read("model-daily")$data
      #   new <- xts_to_arc(ret_d)
      #   combo <- xts_rbind(new, old, is_xts = FALSE)
      #   ret_lib$write("model-daily", xts_to_arc(combo))
      # }
      # if (length(ret_m) >= 1) {
      #   colnm <- unlist(lapply(ret_m, "colnames"))
      #   ret_m <- do.call("cbind", ret_m)
      #   colnames(ret_m) <- colnm
      #   old <- ret_lib$read("model-monthly")$data
      #   new <- xts_to_arc(ret_m)
      #   combo <- xts_rbind(new, old, is_xts = FALSE)
      #   ret_lib$write("model-monthly", xts_to_arc(combo))
      # }
    },
    
    ret_stock = function(ids = NULL, date_start = NULL, date_end = Sys.Date(),
                         freq = "D", geo = c("us", "intl")) {
      geo <- tolower(geo[1])
      if (!geo %in% c("us", "intl")) {
        stop("geo must be us or intl")
      }
      geo <- paste0(geo, "-stock")
      if (is.null(ids)) {
        stock <- filter(self$tbl_msl, ReturnLibrary == geo)
        ids <- create_ids(stock)
      } else {
        ix <- match_ids_dtc_name(ids, self$tbl_msl)
        if (any(is.na(ix))) {
          stop("ids missing from MSL")
        }
      }
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read(geo)$data
      if (is.null(date_start)) {
        date_start <- old_dat$Date[nrow(old_dat)]
        date_start <- as.Date(date_start)
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
        Sys.sleep(1)
      }
      ix <- match_ids_dtc_name(xdf$RequestId, self$tbl_msl)
      dtc_name <- self$tbl_msl$DtcName[ix]
      xdf$DtcName <- dtc_name
      is_dup <- duplicated(paste0(xdf$DtcName, xdf$Date))
      xdf <- xdf[!is_dup, ]
      xdf$TotalReturn <- xdf$TotalReturn / 100
      new_dat <- pivot_wider(xdf, id_cols = Date, values_from = TotalReturn,
                             names_from = DtcName)
      combo <- xts_rbind(new_dat, old_dat, FALSE, TRUE)
      lib$write(geo, xts_to_arc(combo))
    },

    #' HFRI Return index from csv file
    ret_hfr_index = function(file_nm) {
      dat <- read_hfr_csv(file_nm)
      dat <- dat / 100
      lib <- self$ac$get_library("returns")
      lib$write("hfr-index", xts_to_arc(dat))
    },
    
    #' @description Update returns that require computational changes
    ret_function = function() {
      # cash plus 200 and 400 bps
      cash <- read_ret("BofAML U.S. Treasury Bill 3M", self$ac)
      cash_plus_2 <- cash + 0.02 / 252
      cash_plus_4 <- cash + 0.04 / 252
      lib <- self$ac$get_library("returns")
      rec <- lib$read("index")
      rec$data[, "Cash Plus 200 bps"] <- as.vector(cash_plus_2)
      rec$data[, "Cash Plus 400 bps"] <- as.vector(cash_plus_4)
      lib$write("index", rec$data)
      # money market proxy with cash
      mmkt <- cash
      colnames(mmkt) <- "Blackrock Liquidity Fed Funds (TFDXX)"
      lib$write("money-market", xts_to_arc(mmkt))
    },
    
    ret_fred = function() {
      dict <- filter(self$tbl_msl, ReturnLibrary == "fred-monthly")
      lib_meta <- self$ac$get_library("meta-tables")
      lib_ret <- self$ac$get_library("returns")
      fred <- lib_meta$read("fred-meta")
      x <- left_merge(dict, fred$data, match_by = "DtcName")
      if (nrow(x$miss) > 0) {
        warning(x$miss$DtcName)
      }
      if (nrow(x$inter) == 0) {
        stop("no fred records found")
      }
      dat <- list()
      for (i in 1:nrow(x$inter)) {
        dat[[i]] <- download_fred(x$inter$Ticker[i], self$api_keys$fred)
        if (tolower(x$inter$Type[i]) == "price") {
          dat[[i]] <- price_to_ret(dat[[i]])
        } 
      }
      r <- do.call(cbind.xts, dat)
      tix <- sapply(dat, colnames)
      ix <- match_ids_dtc_name(tix, self$tbl_msl)
      dtc_name <- self$tbl_msl$DtcName[ix]
      colnames(r) <- dtc_name
      r <- xts_eo_month(r)
      lib_ret$write("fred-monthly", xts_to_arc(r))
    },
    
    # read returns ----
    #' @description Read Returns by ids
    #' @param ids ids to read in, search in order of Ticker, Cusip, Sedol,
    #'   Lei, DtcName, Identifier
    #' @details
        #' If any returns are pulled from a monthly return library, e.g., CTF
        #' official returns, then all returns pinged will be converted to monthly
    read_ret = function(ids) {
      read_ret(ids, self$ac)
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
            dup_date <- as.character(dat$TimeStamp[1]) %in% 
              unique(old_dat$data$TimeStamp)
            if (dup_date) {
              warning(paste0(dtc_name[i], " already has data for latest date"))
              if (return_data) {
                res[[i]] <- dat
              }
              next
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
                         return_data = FALSE, as_of = NULL) {
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
        dat <- try(download_bd(cust$BdAccountId[i], self$api_keys, 
                               as_of = as_of))
        if ("try-error" %in% class(dat)) {
          warning(paste0("could not download ", dtc_name[i]))
          next
        }
        if (save_to_db) {
          dat$TimeStamp <- as.character(as.Date(dat$TimeStamp))
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
    
    hold_cust_backfill = function(dtc_name, date_start, freq = "months",
                                  save_to_db = TRUE, return_data = FALSE) {
      freq <- check_freq(freq)
      date_start <- as.Date(date_start)
      if (freq == "days") {
        return("months only for now")
        # dt <- us_trading_days(date_start, last_us_trading_day())
      } else if (freq == "months") {
        date_start <- eo_month(date_start)
        dt <- seq.Date(date_start, last_us_trading_day(), by = "months")
        dt[2:length(dt)] <- lubridate::ceiling_date(dt[2:length(dt)] - 10, 
                                                    unit = "months") - 1
        dt <- as_trading_day(dt)
      } else {
        stop("freq only supported for days or months")
      }
      xdf <- data.frame()
      cust <- self$tbl_cust
      rec <- filter(cust, DtcName == dtc_name)
      if (nrow(rec) ==  0) {
        stop(paste0(dtc_name, " not found."))
      }
      for (i in 1:length(dt)) {
        x <- try(download_bd(rec$BdAccountId, self$api_keys, as_of = dt[i]))
        if ("try-error" %in% class(x)) {
          warning(dt[i])
          next
        }
        print(dt[i])
        xdf <- rbind(xdf, x)
      }
      if (save_to_db) {
        lib <- self$ac$get_library("holdings")
        lib$write(dtc_name, xdf)
      }
      if (return_data) {
        return(xdf)
      }
    },
    
    hold_model = function(dtc_name, tbl_hold = NULL, xl_file = NULL, 
                          sum_to_1 = TRUE) {
      if (!is.null(xl_file)) {
        tbl_hold <- readxl::read_excel(xl_file)
      }
      check_tbl_hold(tbl_hold)
      tbl_hold$TimeStamp <- as.character(tbl_hold$TimeStamp)
      if (sum_to_1) {
        s <- split(tbl_hold, tbl_hold$TimeStamp)
        for (i in 1:length(s)) {
          s[[i]]$CapWgt <- s[[i]]$CapWgt / sum(s[[i]]$CapWgt, na.rm = TRUE)
        }
        tbl_hold <- do.call(rbind, s)
      }
      lib <- self$ac$get_library("holdings")
      lib$write(dtc_name, tbl_hold)
        
    }, 
    
    hold_ctf = function(dtc_name = NULL, as_of = NULL, save_to_db = TRUE,
                        return_data = FALSE, download_ctf = FALSE) {
      dict <- filter(self$tbl_msl, SecType == "ctf")
      dict <- filter(dict, Layer == 3)
      
      if (!is.null(dtc_name)) {
        dict <- filter(dict, DtcName %in% dtc_name)
      } 
      lib_hold <- self$ac$get_library("holdings")
      if (nrow(dict) == 0) {
        stop("dtc_name not found")
      }
      all_hold <- lib_hold$list_symbols()
      for (i in 1:nrow(dict)) {
        tbl_hold <- self$hold_cust(dict$DtcName[i], save_to_db = FALSE, 
                                   return_data = TRUE, as_of = as_of)
        res <- merge_msl(tbl_hold[[1]], self$tbl_msl)
        mv <- rep(NA, nrow(res$union))
        for (j in 1:nrow(res$union)) {
          if (download_ctf) {
            x <- try(self$hold_cust(res$union$DtcName[j], save_to_db = FALSE,
                     return_data = TRUE)[[1]])
            if (inherits(x, "try-error")) {
              mv[j] <- res$union$Value[j]
            } else {
              mv[j] <- sum(x$Value, na.rm = TRUE)
            }
          } else {
            if (res$union$DtcName[j] %in% all_hold) {      
              x <- self$read_hold(res$union$DtcName[j])
              mv[j] <- sum(as.numeric(x$Value), na.rm = TRUE)
            } else {
              mv[j] <- res$union$Value[j]
            }
          }
        }
        tbl_hold[[1]]$Value <- mv
        tbl_hold[[1]]$CapWgt <- tbl_hold[[1]]$Value / 
          sum(tbl_hold[[1]]$Value, na.rm = TRUE)
        if (save_to_db) {
          old <- lib_hold$read(dict$DtcName[i])
          if (!inherits(old, "data.frame")) {
            warning(paste0(dtc_name[i], " does not have existing holdings."))
            old <- data.frame()
          }
          combo <- rbind_holdings(old, tbl_hold[[1]])
          lib_hold$write(dict$DtcName[i], combo)
        }
      }
    if (return_data) {
        return(tbl_hold)
      }
    },

    hold_ctf_backfill = function(dtc_name = NULL, combo = FALSE) {
      res <- self$hold_ctf(dtc_name, save_to_db = FALSE, return_data = TRUE,
                           download_ctf = FALSE)
    },
    
    #' @description Read Holdings Data
    #' @param dtc_name DtcName field in MSL to pull holdings
    #' @param latest option to truncate to most recent holdings
    read_hold = function(dtc_name, latest = TRUE) {
      read_hold(self$ac, dtc_name, latest)
    },

    # Company data ----

    #' @description Download equity financial data
    #' @param ids leave `NULL` to get for all stocks, or enter specific ids
    #' @param yrs_back how many years of data to pull
    #' @param dtype data type: one of PE, PB, PFCF, DY, ROE, and MCAP
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
    
    #' @description Download Sector Data
    #' @param ids specify ids or leave `NULL` to gather for all stocks in MSL
    #' @details
        #' Sector data are gathered from Factset, mapped to GICS, and from 
        #' Piper Sandler Macro Downloads
    download_sectors = function(ids = NULL) {
      if (is.null(ids)) {
        stock <- filter(self$tbl_msl, ReturnLibrary == "stock")
        ids <- stock$Isin
      }
      ids[is.na(ids)] <- stock$Cusip[is.na(ids)]
      ids <- gsub(" ", "", ids)
      ids <- na.omit(ids)
      iter <- space_ids(ids)
      xformula <- "FG_FACTSET_SECTOR"
      xdf <- data.frame()
      for (i in 1:(length(iter)-1)) {
        json <- download_fs_formula(
          api_keys = self$api_keys, 
          ids = ids[iter[i]:iter[i+1]], 
          formulas = xformula
        )
        xdf <- rbind(xdf, flatten_fs_formula(json))
      }
      colnames(xdf) <- c("RequestId", "FactsetSector")
      is_dup <- duplicated(xdf$RequestId)
      xdf <- xdf[!is_dup, ]
      ix <- match(xdf$RequestId, stock$Isin)
      ix[is.na(ix)] <- match(xdf$RequestId, stock$Cusip)[is.na(ix)]
      xdf$DtcName <- stock$DtcName[ix]
      macro_lib <- self$ac$get_library("ps-macro")
      r3 <- macro_lib$read("macro_sel_r3")$data
      # TO-DO add ACWI
      r3 <- rename(r3, Isin = ISIN)
      res <- left_merge(r3, self$tbl_msl, c("Ticker", "Isin"))
      res <- left_merge(xdf, res$inter, "DtcName")
      sect <- res$union[, c("RequestId", "FactsetSector", "DtcName", "Sector")]
      sect <- rename(sect, GicsMacro = Sector)
      lib <- self$ac$get_library("meta-tables")
      sect_map <- lib$read("sector-map")$data
      res <- left_merge(sect, sect_map, c("FactsetSector"))
      lib <- self$ac$get_library("co-qual-data")
      lib$write("sector", res$union)
    },

    # download_cusip = function(ids = NULL) {
    #   if (is.null(ids)) {
    #     stock <- filter(self$tbl_msl, ReturnLibrary == "stock")
    #     ids <- stock$Isin
    #   }
    #   ids <- gsub(" ", "", ids)
    #   ids <- na.omit(ids)
    #   iter <- space_ids(ids)
    #   formulas <- "FSYM_CUSIP(0,'ID')"
    #   json <- download_fs_formula(self$api_keys, ids[1:10], formulas)
    # }
    
    #' @description Download Macro Select Workbook and Save to Library
    #' @param wb file location of workbook
    #' @param is_us TRUE for Russell 3000, FALSE for MSCI ACWI
    update_ps_macro_select = function(wb, is_us = TRUE) {
      if (is_us) {
        idx_nm <- "Russell 3000"
      } else {
        idx_nm <- "MSCI ACWI"
      }
      dat <- read_macro_wb(wb, idx_nm)
      bad_row <- rowSums(is.na(dat)) == ncol(dat)
      dat <- dat[!bad_row, ]
      lib <- self$ac$get_library("ps-macro")
      if (is_us) {
        lib$write("macro_sel_r3", dat)
      } else {
        lib$write("macro_sel_acwi", dat)  
      }
    },
    
    #' @description Create Portfolio from DtcName (holdings table)
    #' @param dtc_name DtcName
    #' @param latest truncate holdings to last update, default is FALSE for 
    #'   entire time-series of holdings
    create_port = function(dtc_name, latest = FALSE) {
      tbl_hold <- self$read_hold(dtc_name, latest)
      Portfolio$new(self$ac, tbl_hold, dtc_name, dtc_name)
    },
    
    #' @description Create Portfolio from ids (quick set up)
    #' @param ids Ticker, Cusip, Sedol, etc
    #' @param wgt optional vector of corresponding weights, default is 1/n
    #' @param name optional string to name portfolio
    #' @param tr_id optional string for id to pull track record, default will
    #'   be rebalance of weights and returns from ids
    create_port_from_ids = function(ids, wgt = NULL, incept = NULL, name = NULL,
                                    tr_id = NULL) {
      r <- self$read_ret(ids)
      if (is.null(incept)) {
        incept <- first_comm_start(r)
      }
      if (is.null(wgt)) {
        wgt <- rep(1 / ncol(r), ncol(r))
      }
      tbl_hold <- data.frame(DtcName = colnames(r), CapWgt = wgt, 
                             TimeStamp = incept)
      if (is.null(name) & ncol(r) == 1) {
        name <- colnames(r)[1]
      }
      Portfolio$new(self$ac, tbl_hold, name, tr_id)
    }
  )
)