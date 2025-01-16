#' Portfolio Object
#' 
#' @description
#' Structure to contain and wrangle portfolio holdings and returns
#' 
#' @export
Portfolio <- R6::R6Class(
  "Portfolio",
  public = list(
    name = NULL,
    #' @field tbl_hold holdings table
    tbl_hold = data.frame(),
    #' @field tbl_miss holdings that are missing from MSL
    tbl_miss = data.frame(),
    #' @field tbl_msl table containing master security list
    tbl_msl = data.frame(),
    #' @field ac ArcticDB datastore
    ac = NULL,
    #' @field lib_hold ArcticDB library of holdings tables
    lib_hold = NULL,
    #' @field lib_ret ArcticDb library of returns
    lib_ret = NULL,

    #' @description Create new Portfolio
    #' @param ac ArcticDB datastore from Database Object
    #' @param tbl_hold holdings table, see details
    #' @details tbl_hold is gathered with the Database Object method $get_hold().
    initialize = function(ac, tbl_hold, name = NULL) {
      if (is.null(name)) {
        name <- "Port"
      }
      self$name <- name
      self$ac <- ac
      lib_meta <- ac$get_library("meta-tables")
      self$lib_hold <- ac$get_library("holdings")
      self$lib_ret <- ac$get_library("returns")
      self$tbl_hold <- tbl_hold
      self$tbl_msl <- lib_meta$read("msl")$data
      self$tbl_miss <- data.frame()
      self$merge_msl()
    },

    #' @description Merge MSL with Holdings Table
    merge_msl = function() {
      res <- left_merge(
        x = self$tbl_hold,
        y = self$tbl_msl,
        match_by = c("DtcName", "Ticker", "Cusip", "Sedol", "Isin", "Lei",
                     "Identifier")
      )
      self$tbl_hold <- res$inter
      self$tbl_miss <- res$miss
    },

    #' @description Drill down to underlying holdings of funds / CTFs / models
    drill_down = function() {
      is_lay_1 <- self$tbl_hold$Layer == 1
      if (all(is_lay_1)) {
        warning("no layers beyond 1 found")
        return(NULL)
      }
      lay_1 <- self$tbl_hold[is_lay_1, ]
      x <- self$tbl_hold[!is_lay_1, ]
      for (i in 1:10) {
        for (j in 1:nrow(x)) {
          record <- self$lib_hold$read(x$DtcName[j])
          record$data[, paste0("Layer", x$Layer[j])] <- x$DtcName[j]
          record$data$CapWgt <- record$data$CapWgt * x$CapWgt[j]
          lay_1 <- rob_rbind(lay_1, record$data)
        }
        res <- left_merge(
          x = lay_1,
          y = self$tbl_msl,
          match_by = c("DtcName", "Ticker", "Cusip", "Sedol", "Isin", "Lei",
                       "Identifier"),
          keep_x_dup_col = FALSE
        )
        lay_1 <- res$inter
        self$tbl_miss <- rob_rbind(self$tbl_miss, res$miss)
        is_lay_1 <- lay_1$Layer == 1
        if (any(is.na(is_lay_1))) {
          warning("some layer observations missing in tbl_msl")
          is_lay_1[is.na(is_lay_1)] <- TRUE
        }
        x <- lay_1[!is_lay_1, ]
        if (nrow(x) == 0) {
          break
        }
      }
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
    }
    
  )
)
