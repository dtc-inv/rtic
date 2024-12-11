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
    initialize = function(api_keys,
      py_loc = "C:/Users/asotolongo/AppData/Local/anaconda3") {

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
      adb <- try(import("arcticdb"))
      if ("try-error" %in% class(adb)) {
        use_python(py_loc)
        adb <- try(import("arcticdb"))
        if ("try-error" %in% class(adb)) {
          return("could not import arcticdb")
        }
      }
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
    }
  )
)

