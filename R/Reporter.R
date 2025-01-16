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
    
    cat_summ = function(cat_tbl, tgt_nm) {
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]
        port$drill_down()
        port$get_sector_data()
        
        xsect <- group_by(port$tbl_hold, .data[[sect_col]]) |>
          summarize(x = sum(CapWgt))
        colnames(xsect)[2] <- port$name
        res <- left_merge(tbl_sect, xsect, sect_col)
        tbl_sect <- res$union
      }
      return(tbl_sect)
    },
    
    sect_summ = function(sect_col = c("GicsMacro", "GicsMap", "FactsetSector")) {
      sect_col <- sect_col[1]
      lib <- self$ac$get_library("meta-tables")
      sect_map <- lib$read("sector-map")$data
      if (sect_col == "FactsetSector") {
        tbl_sect <- data.frame(
          "FactsetSector" = sort(unique(sect_map$FactsetSector))  
        )
      } else if (sect_col == "GicsMap") {
        tbl_sect <- data.frame(
          "GicsMap" = sort(unique(sect_map$GicsMap))
        )
      } else if (sect_col == "GicsMacro") {
        tbl_sect <- data.frame(
          "GicsMacro" = sort(unique(sect_map$GicsMap))
        )
      } else {
        stop("sect_col misspecified")
      }
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]
        port$drill_down()
        port$get_sector_data()
        xsect <- group_by(port$tbl_hold, .data[[sect_col]]) |>
          summarize(x = sum(CapWgt))
        colnames(xsect)[2] <- port$name
        res <- left_merge(tbl_sect, xsect, sect_col)
        tbl_sect <- res$union
      }
      return(tbl_sect)
    }
  )
)