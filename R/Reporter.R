#' Reporter Object
#' 
#' @description
#' Structure to wrangle portfolios and an optional benchmark into reports
#' 
#' @export
Reporter <- R6::R6Class(
  "Reporter",
  public = list(
    #' @field port list of Portfolio Object(s)
    port = list(),
    #' @field bench portfolio object representing benchmark, can be NULL
    bench = NULL,
    #' @field ac ArcticDB datastore object
    ac = NULL,
    #' @field rf risk-free time-series
    rf = NULL,
    #' @field col vector of colors for charts, leave NULL for standard DTC colors
    col = NULL,
    
    #' @description Create Reporter object
    #' @param port list of Portfolio Object(s)
    #' @param bench benchmark (Portfolio Object), can leave NULL
    #' @param rf xts representing risk-free rate, can leave NULL, will default
    #'   to BIL ETF
    #' @param col vector of colors, leave NULL for standard DTC colors
    initialize = function(port, bench = NULL, rf = NULL, col = NULL) {
      self$port <- port
      self$bench <- bench
      self$clean_port_nm()
      self$ac <- port[[1]]$ac
      if (is.null(rf)) {
        lib <- self$ac$get_library("returns")
        rf <- lib$read(
          symbol = "etf", 
          columns = c("Date", "SPDR Bloomberg 1-3 Month T-bill (BIL)")
        )
      }
      self$rf <- na.omit(dataframe_to_xts(rf$data))
      if (is.null(col)) {
        self$col <- dtc_col()
      }
    },
    
    # holdings ----
    
    #' @description Forces all portfolio names to be unique
    clean_port_nm = function() {
      nm <- sapply(self$port, '[[', "name")
      is_dup <- duplicated(nm)
      if (any(is_dup)) {
        warning("duplicated portfolio names found, using make.unique")
        warning(nm[is_dup])
        uniq <- make.unique(nm)
        for (i in 1:length(self$port)) {
          self$port[[i]]$name <- uniq[i] 
        }
      }
    },
    
    #' @description Sector summary
    #' @param tgt target column: FactsetSector, GicsMacro, or GicsMap
    #' @param layer 1 for security level, 2 for fund level
    #' @details
        #' GicsMacro is from piper sandler macro workbook, GicsMap is a mapping
        #' of Factset Sectors to GICS sectors
    sector_summ = function(tgt = c("FactsetSector", "GicsMacro", "GicsMap"),
                           layer = 1) {
      lib <- self$ac$get_library("co-qual-data")
      tgt <- tgt[1]
      sect <- lib$read("sector")$data
      if (!tgt %in% colnames(sect)) {
        warning(paste0(tgt, " not found. Returning NULL"))
        return(NULL)
      }
      self$cat_summ(sect, tgt, layer)
    },
    
    #' @description Country summary
    #' @param tgt target column, RiskCountry currently supported
    #' @param layer 1 for security level, 2 for fund level
    #' @param rgn option to include a column with regions
    country_summ = function(tgt = "RiskCountry", layer = 1, rgn = FALSE) {
      lib <- self$ac$get_library("co-qual-data")
      tgt <- tgt[1]
      country <- lib$read("country")$data
      if (!tgt %in% colnames(country)) {
        warning(paste0(tgt, " not found. Returning NULL"))
        return(NULL)
      }
      res <- self$cat_summ(country, tgt)
      if (rgn) {
        lib <- self$ac$get_library("meta-tables")
        dict <- lib$read("country-map")$data
        res <- left_merge(res, dict, "RiskCountry")
        res <- res$union
      }
      return(res)
    },
    
    #' @description Summarize category, use sector and country functions to 
    #'   execute
    #' @param tbl_cat data.frame with catagories to summarize
    #' @param tgt_nm target column name / category in tbl_cat
    #' @param layer 1 for security level, 2 for fund level
    cat_summ = function(tbl_cat, tgt_nm, layer = 1) {
      tbl_cat <- data.frame(x = na.omit(sort(unique(tbl_cat[, tgt_nm]))))
      colnames(tbl_cat)[1] <- tgt_nm
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]$clone()
        port$tbl_hold <- latest_holdings(port$tbl_hold)
        port$drill_down()
        port$get_sector_data()
        port$get_country_data()
        tbl_group <- group_by(port$tbl_hold, .data[[tgt_nm]]) |>
          summarize(x = sum(CapWgt))
        colnames(tbl_group)[2] <- port$name
        if (layer > 1) {
          tgt_layer <- paste0("Layer", layer) 
          if (tgt_layer %in% colnames(port$tbl_hold)) {
            tbl_group <- group_by(port$tbl_hold, FactsetSector, 
                                  .data[[tgt_layer]]) |>
              summarize(x = sum(CapWgt)) |> 
              pivot_wider(id_cols = FactsetSector, 
                          names_from = all_of(tgt_layer), values_from = x)
          }
        }
        res <- left_merge(tbl_cat, tbl_group, tgt_nm)
        tbl_cat <- res$union
        rm(port)
      }
      if (!is.null(self$bench)) {
        bench <- self$bench$clone()
        bench$drill_down()
        bench$get_sector_data()
        bench$get_country_data()
        tbl_group <- group_by(bench$tbl_hold, .data[[tgt_nm]]) |>
          summarize(x = sum(CapWgt))
        colnames(tbl_group)[2] <- "Benchmark" 
        res <- left_merge(tbl_cat, tbl_group, tgt_nm)
        rm(bench)
      }
      return(res$union)
    },
    
    #' @description Financial data summary: PE, PB, PFCF, and DY
    #' @param layer 1 for security, 2 for fund
    fina_summ = function(layer = 1) {
      met <- c("PE", "PB", "PFCF", "DY")
      res <- data.frame(Metric = met)
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]$clone()
        port$drill_down()
        port$get_fina_data()
        w <- port$tbl_hold$CapWgt
        pe <- avg_fina_ratio(w, port$tbl_hold$PE)
        pb <- avg_fina_ratio(w, port$tbl_hold$PB)
        pfcf <- avg_fina_ratio(w, port$tbl_hold$PFCF)
        dy <- wgt_avg(w, port$tbl_hold$DY)
        xdf <- data.frame(Metric = met, x = c(pe, pb, pfcf, dy))
        colnames(xdf)[2] <- port$name
        x <- left_merge(res, xdf, "Metric")
        if (layer >= 1) {
          tgt_layer <- paste0("Layer", layer)
          if (tgt_layer %in% colnames(port$tbl_hold)) {
            tbl_group <- group_by(port$tbl_hold, .data[[tgt_layer]])
            pe <- summarize(tbl_group, PE = avg_fina_ratio(CapWgt, PE))
            pb <- summarize(tbl_group, PB = avg_fina_ratio(CapWgt, PB))
            pfcf <- summarize(tbl_group, PFCF = avg_fina_ratio(CapWgt, PFCF))
            dy <- summarize(tbl_group, DY = wgt_avg(CapWgt, DY))
            xdf <- cbind(pe[, 2], pb[, 2], pfcf[, 2], dy[, 2])
            is_miss <- is.na(pe[[1]])
            xdf <- xdf[!is_miss, ]
            xdf <- t(xdf)
            colnames(xdf) <- pe[[1]][!is_miss]
            xdf <- as.data.frame(xdf)
            xdf$Metric <- rownames(xdf)
            x <- left_merge(res, xdf, "Metric")
          }
        }
        res <- x$union
        rm(port)
      }
      if (!is.null(self$bench) & layer == 1) {
        bench <- self$bench$clone()
        bench$drill_down()
        bench$get_fina_data()
        w <- bench$tbl_hold$CapWgt
        pe <- avg_fina_ratio(w, bench$tbl_hold$PE)
        pb <- avg_fina_ratio(w, bench$tbl_hold$PB)
        pfcf <- avg_fina_ratio(w, bench$tbl_hold$PFCF)
        dy <- wgt_avg(w, bench$tbl_hold$DY)
        xdf <- data.frame(Metric = met, x = c(pe, pb, pfcf, dy))
        colnames(xdf)[2] <- bench$name
        x <- left_merge(x$union, xdf, "Metric")
        res <- x$union
        rm(bench)
      }
      return(res)
    },
    
    # returns ----
    
    #' @description Clean and lineup portfolio, benchmark, asset, and rf returns
    #' @param freq option to change frequency
    ret_combo = function(freq = NULL, date_start = NULL, date_end = NULL) {
      res <- list()
      bench <- self$bench$clone()
      bench$read_track_rec()
      b_asset_ret <- bench$read_asset_ret()
      if (is.null(bench$track_rec)) {
        bench$init_rebal()
        bench$track_rec <- bench$rebal$rebal_ret
      }
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]$clone()
        port$read_track_rec()
        if (is.null(port$track_rec)) {
          port$init_rebal()
          port$track_rec <- port$rebal$rebal_ret
        }
        asset_ret <- port$read_asset_ret()
        x <- clean_asset_bench_rf(asset_ret, bench$track_rec, self$rf, freq,
                                  date_start, date_end)
        p <- clean_asset_bench_rf(port$track_rec, bench$track_rec, self$rf, 
                                  freq, date_start, date_end)
        r <- list()
        r$xp <- x$x
        r$xb <- x$b
        r$xrf <- x$rf
        r$p <- p$x
        r$b <- p$b
        r$rf <- p$rf
        res[[i]] <- r
        rm(port)
      }
      rm(bench)
      return(res)
    }
  )
)