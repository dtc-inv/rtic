Reporter <- R6::R6Class(
  "Reporter",
  public = list(
    port = list(),
    bench = NULL,
    
    initialize = function(port = NULL, bench = NULL) {
      self$port <- port
      self$bench <- bench
    },
    
    sect_summ = function(layer = 1) {
      tbl_sect <- data.frame()
      for (i in 1:length(self$port)) {
        port <- self$port[[i]]
        port$get_sector_data()
        
      }
    }
  )
)