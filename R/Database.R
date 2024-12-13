#' Database Object
#'
#' @description
#'   Download and store holdings and price time-series data in s3 from various
#'   sources such as factset, blackdiamond, excel
#' @export
db <- R6::R6Class(
  'db',
  public = list(
    #' @field api_keys of api keys
    api_keys = NULL,
    #' @field bucket aws s3 bucket
    bucket = NULL,
    #' @field ac ArcticDB object store
    ac = NULL,
    #' @field tbl_msl master security list: table of meta data for all investments
    tbl_msl = NULL,
    #' @field tbl_cust custodian list: table of meta data for CTFs and SMAs
    tbl_cust = NULL,
    #' @field tbl_sec SEC list: table of meta data for mutual funds and ETFs
    tbl_sec = NULL,
    #' @field tbl_xl_mod excel model list: table of meta data for model portfolios
    tbl_xl_mod = NULL,
    #' @field tbl_hold_field holdings fields: table of fields / columns for
    #'  holdings data
    tbl_hold_field = NULL,

    #' @description Create a db object
    #' @param api_keys list of api keys or `.RData` file location to load the list
    #'   of keys
    #' @param py_loc optional file path where python is installed to be used
    #'   with `reticulate`
    initialize = function(api_keys, py_loc) {

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
      bucket <- arrow::s3_bucket(
        "dtc-inv",
        access_key = api_keys$s3$access_key,
        secret_key = api_keys$s3$secret_key
      )
      use_python(py_loc)
      adb <- import("arcticdb")
      base_url <- 's3://s3.us-east-1.amazonaws.com:dtc-inv?'
      s3_url <- paste0(
        base_url,
        "access=", api_keys$s3$access_key,
        "&secret=", api_keys$s3$secret_key
      )
      ac <- adb$Arctic(s3_url)
      self$bucket <- bucket
      self$ac <- ac
      self$tbl_msl <- read_parquet(self$bucket$path("tables/tbl_msl.parquet"))
      self$tbl_cust <- read_parquet(self$bucket$path("tables/tbl_cust.parquet"))
      self$tbl_sec <- read_parquet(self$bucket$path("tables/tbl_sec.parquet"))
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

    # update returns ----

    #' @description Update index returns from Factset
    #' @param ids leave NULL to update all indexes, or enter specific index
    #'   ids to update, note ids will have to first exist in the Master Security
    #'   List `tbl_msl`
    #' @param t_minus how many months to download
    ret_index = function(ids = NULL, t_minus = 1) {
      if (is.null(ids)) {
        idx <- filter(self$tbl_msl, ReturnLibrary == "index")
        ids <- idx$Ticker
      }
      res <- list()
      is_miss <- rep(FALSE, length(ids))
      for (i in 1:length(ids)) {
        dat <- try(download_fs_ra_ret(ids[i], self$api_keys, t_minus, "D"))
        if ("try-error" %in% class(dat)) {
          is_miss[i] <- TRUE
        } else {
          res[[i]] <- dat
        }
      }
      dtc_name <- filter(self$tbl_msl, Ticker %in% ids)$DtcName[!is_miss]
      dtc_name <- na.omit(dtc_name)
      ret <- do.call("cbind", res)
      colnames(ret) <- dtc_name
      new_dat <- xts_to_dataframe(ret)
      colnames(new_dat) <- colnames(ret)
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("index")$data
      old_dat_df <- arc_to_dataframe(old_dat)
      combo <- xts_rbind(new_dat, old_dat_df, FALSE)
      lib$write("index", data.frame(combo))
    },

    ret_stock = function(ids = NULL, date_start = NULL, date_end = Sys.Date(),
                         freq = "D") {
      if (is.null(ids)) {
        stock <- filter(self$tbl_msl, ReturnLibrary == "stock")
        ids <- create_ids(stock)
      }
      lib <- self$ac$get_library("returns")
      old_dat <- lib$read("stock")$data
      if (is.null(date_start)) {
        date_start <- rownames(old_dat)[nrow(old_dat)]
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
      }
      ix <- match_ids_dtc_name(xdf$requestId, self$tbl_msl)
      dtc_name <- self$tbl_msl$DtcName[ix]
      xdf$DtcName <- dtc_name
      is_dup <- duplicated(paste0(xdf$DtcName, xdf$date))
      xdf <- xdf[!is_dup, ]
      new_dat <- pivot_wider(xdf, id_cols = date, values_from = totalReturn,
                             names_from = DtcName)
      colnames(new_dat)[1] <- "Date"
      old_dat_df <- arc_to_dataframe(old_dat)
      combo <- xts_rbind(new_dat, old_dat_df, FALSE)
    }
  )
)

