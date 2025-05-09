#' Portfolio Object
#' 
#' @description
#' Structure to contain and wrangle portfolio holdings and returns
#' 
#' @export
Portfolio <- R6::R6Class(
  "Portfolio",
  public = list(
    #' @field name portfolio name
    name = NULL,
    #' @field tr_id id to pull track record
    tr_id = NULL,
    #' @field tbl_hold holdings table
    tbl_hold = data.frame(),
    #' @field tbl_miss holdings that are missing from MSL
    tbl_miss = data.frame(),
    #' @field ac ArcticDB datastore
    ac = NULL,
    #' @field rebal Rebalance object
    rebal = NULL,
    #' @field track_rec xts return time-series
    track_rec = NULL,
    
    #' @description Create new Portfolio
    #' @param ac ArcticDB datastore from Database Object
    #' @param tbl_hold holdings table, see details
    #' @param name portfolio name
    #' @param tr_id track record id
    #' @details tbl_hold is gathered with the Database Object method $get_hold()
    initialize = function(ac, tbl_hold, name = NULL, tr_id = NULL) {
      if (is.null(name)) {
        name <- "Port"
      }
      self$name <- name
      self$tr_id <- tr_id
      self$ac <- ac
      self$check_ac()
      lib <- self$ac$get_library("meta-tables")
      self$tbl_hold <- tbl_hold
      self$tbl_hold_check()
      self$tbl_miss <- data.frame()
      self$merge_msl()
    },

    #' @description check specification of arctic db
    check_ac = function() {
      if (!"arcticdb.arctic.Arctic" %in% class(self$ac)) {
        stop("ac not proper arcticdb object")
      } 
    },
    
    #' @description check specification of holdings table
    tbl_hold_check = function() {
      check_tbl_hold(self$tbl_hold)
    },
    
    #' @description Merge MSL with Holdings Table
    merge_msl = function() {
      tbl_msl <- read_msl(self$ac)
      res <- merge_msl(
        self$tbl_hold,
        tbl_msl
      )
      self$tbl_hold <- res$inter
      self$tbl_miss <- res$miss
    },

    #' @description Drill down to underlying holdings of funds / CTFs / models
    #' @param layer what layer to drill down to
    #' @param latest boolean to truncate to only most recent holdings
    drill_down = function(layer = 1, latest = TRUE) {
      tbl_msl <- read_msl(self$ac)
      if (latest) {
        self$tbl_hold <- latest_holdings(self$tbl_hold)
      }
      is_lay_1 <- self$tbl_hold$Layer <= layer
      if (all(is_lay_1)) {
        warning("no layers beyond 1 found")
        return()
      }
      lay_1 <- self$tbl_hold[is_lay_1, ]
      x <- self$tbl_hold[!is_lay_1, ]
      lib_hold <- self$ac$get_library("holdings")
      for (i in 1:10) {
        for (j in 1:nrow(x)) {
          record <- lib_hold$read(x$DtcName[j])
          if (latest) {
            record$data <- latest_holdings(record$data)
          }
          record$data[, paste0("Layer", x$Layer[j])] <- x$DtcName[j]
          record$data$CapWgt <- record$data$CapWgt * x$CapWgt[j]
          lay_1 <- rob_rbind(lay_1, record$data)
        }
        # res <- left_merge(
        #   x = lay_1,
        #   y = tbl_msl,
        #   match_by = c("DtcName", "Ticker", "Cusip", "Sedol", "Isin", "Lei",
        #                "Identifier"),
        #   keep_x_dup_col = FALSE
        # )
        res <- merge_msl(lay_1, tbl_msl, FALSE)
        lay_1 <- res$inter
        self$tbl_miss <- rob_rbind(self$tbl_miss, res$miss)
        is_lay_1 <- lay_1$Layer <= layer + i - 1
        if (any(is.na(is_lay_1))) {
          warning("some layer observations missing in tbl_msl")
          is_lay_1[is.na(is_lay_1)] <- TRUE
        }
        x <- lay_1[!is_lay_1, ] # case where lay 2 invests in lay 2
        if (nrow(x) == 0) {
          break
        }
      }
      lay_1 <- lay_1[lay_1$Layer == 1, ]
      self$tbl_hold <- lay_1
    },

    #' @description Get Company
    #' @param xsymbols leave blank for all data, or specify specific data, e.g.,
    #'   `"PE"`, `"PB"`
    get_fina_data = function(xsymbols = NULL) {
      lib <- self$ac$get_library("co-data")
      if (is.null(xsymbols)) {
        xsymbols <- lib$list_symbols()
      }
      symb_exists <- xsymbols %in% lib$list_symbols()
      if (all(!symb_exists)) {
        warning("no symbols found in library")
        return()
      }
      if (any(!symb_exists)) {
        warning(paste0(xsymbols[!symb_exists], " not found in library"))
        xsymbols <- xsymbols[symb_exists]
      }
      for (i in 1:length(xsymbols)) {
        record <- lib$read(xsymbols[i])
        ix <- match(self$tbl_hold$DtcName, colnames(record$data))
        miss <- is.na(ix)
        latest_data <- record$data[nrow(record$data), ix[!miss]]
        self$tbl_hold[!miss, xsymbols[i]] <- as.numeric(latest_data)
      }
    },
    
    #' @description Get Sector Data
    get_sector_data = function() {
      lib <- self$ac$get_library("co-qual-data")
      sect <- lib$read("sector")$data
      res <- left_merge(self$tbl_hold, sect, "DtcName")
      self$tbl_hold <- res$union
    },
    
    #' @description Get Country Data
    get_country_data = function() {
      lib <- self$ac$get_library("co-qual-data")
      country <- lib$read("country")$data
      res <- left_merge(self$tbl_hold, country, "DtcName")
      self$tbl_hold <- res$union
    },
    
    # returns ----
    
    #' @description read track record
    #' @param id option to pass id, otherwise self$tr_id is used
    read_track_rec = function(id = NULL) {
      if (is.null(id)) {
        id <- self$tr_id
      }
      if (!is.null(id)) {
        self$tr_id <- id
        self$track_rec <- read_ret(id, self$ac)
      }
    },
    
    #' @description read asset returns
    read_asset_ret = function() {
      ids <- get_ids(self$tbl_hold)
      read_ret(ids, self$ac)      
    },
    
    #' @description get rebalance weights from holdings
    read_rebal_wgt = function() {
      rebal_wgt <- tidyr::pivot_wider(
        data = self$tbl_hold, 
        id_cols = TimeStamp,
        values_from = CapWgt,
        names_from = DtcName)
      dataframe_to_xts(rebal_wgt)
    },
    
    #' @description execute rebalance
    #' @param rebal_freq rebalance frequency: D, M, Q, A, or BH (buy and hold)
    #' @param asset_freq asset return frequency, if left NULL will guess
    #' @param sum_to_1 force rebalance weights to sum to 100%
    #' @param clean_ret option to clean returns, find common start dates, 
    #'   remove missing values, etc
    init_rebal = function(rebal_freq = "M", asset_freq = NULL, sum_to_1 = TRUE,
                          clean_ret = TRUE) {
      asset_ret <- self$read_asset_ret()
      if (clean_ret) {
        res <- clean_ret(asset_ret)
        asset_ret <- res$ret
      } else {
        asset_ret[is.na(asset_ret)] <- 0
      }
      rebal_wgt <- self$read_rebal_wgt()
      rebal_wgt[is.na(rebal_wgt)] <- 0
      if (is.null(asset_freq)) {
        asset_freq <- guess_freq(asset_ret)
      }
      self$rebal <- Rebal$new(rebal_wgt, asset_ret, asset_freq, rebal_freq, 
                              self$name)
      self$rebal$rebal(sum_to_1)
    }
  )
)
