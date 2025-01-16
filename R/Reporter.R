#' Reporter Object
#' 
#' @description
#' Structure to wrangle portfolios and an optional benchmark into reports
#' 
#' @export
Reporter <- R6::R6Class(
  "Reporter",
  public = list(
    port = list(),
    bench = NULL,
    ac = NULL,
    
    initialize = function(port, bench = NULL) {
      self$port <- port
      self$bench <- bench
      self$clean_port_nm()
      self$ac <- port[[1]]$ac
    },
    
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
    
    cat_summ = function(tbl_cat, tgt_nm) {
      tbl_cat <- data.frame(x = sort(unique(tbl_cat[, tgt_nm])))
      colnames(tbl_cat)[1] <- tgt_nm
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]$clone()
        port$drill_down()
        port$get_sector_data()
        port$get_country_data()
        tbl_group <- group_by(port$tbl_hold, .data[[tgt_nm]]) |>
          summarize(x = sum(CapWgt))
        colnames(tbl_group)[2] <- port$name
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
    }
  )
)