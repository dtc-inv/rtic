Portfolio <- R6::R6Class(
  "Portfolio",
  public = list(
    tbl_hold = data.frame(),
    tbl_miss = data.frame(),
    tbl_msl = data.frame(),

    initialize = function(tbl_hold, tbl_msl) {
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

    }
  )
)
