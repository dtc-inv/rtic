Portfolio <- R6::R6Class(
  "Portfolio",
  public = list(
    tbl_hold = data.frame(),
    tbl_miss = data.frame(),
    tbl_msl = data.frame(),
    ac = NULL,
    lib_hold = NULL,
    lib_ret = NULL,

    initialize = function(ac, tbl_hold, tbl_msl) {
      self$ac <- ac
      self$lib_hold <- ac$get_library("holdings")
      self$lib_ret <- ac$get_library("returns")
      self$tbl_hold <- tbl_hold
      self$tbl_msl <- tbl_msl
      self$tbl_miss <- data.frame()
      self$merge_msl()
    },

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

    drill_down = function() {
      is_lay_1 <- self$tbl_hold$Layer == 1
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

    get_fund_data = function(xsymbols = NULL) {
      lib <- self$ac$get_library("co-data")
      if (is.null(xsymbols)) {
        xsymbols<- lib$list_symbols()
      }
      for (i in 1:length(xsymbols)) {
        record <- lib$read(xsymbols[i])
        ix <- match(self$tbl_hold$DtcName, colnames(record$data))
        miss <- is.na(ix)
        latest_data <- record$data[nrow(record$data), ix[!miss]]
        self$tbl_hold[!miss, xsymbols[i]] <- as.numeric(latest_data)
      }
    }
  )
)
