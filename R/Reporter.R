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
    
    #' @description Create Reporter object
    #' @param port list of Portfolio Object(s)
    #' @param bench benchmark (Portfolio Object), can leave NULL
    #' @param rf xts representing risk-free rate, can leave NULL, will default
    #'   to BIL ETF
    initialize = function(port, bench = NULL, rf = NULL) {
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
    
    country_summ = function(tgt = "RiskCountry", layer = 1) {
      lib <- self$ac$get_library("co-qual-data")
      tgt <- tgt[1]
      country <- lib$read("country")$data
      if (!tgt %in% colnames(country)) {
        warning(paste0(tgt, " not found. Returning NULL"))
        return(NULL)
      }
      self$cat_summ(country, tgt)
    },
    
    cat_summ = function(tbl_cat, tgt_nm, layer = 1) {
      tbl_cat <- data.frame(x = na.omit(sort(unique(tbl_cat[, tgt_nm]))))
      colnames(tbl_cat)[1] <- tgt_nm
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]$clone()
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
      }
      return(res$union)
    },
    
    fina_summ = function() {
      
    },
    
    # returns ----
    
    ret_combo = function() {
      
    }
  )
)