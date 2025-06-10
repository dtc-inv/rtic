#' @title Create ArcticDB datastore
#' @param api_keys list or ".RData" file path with keys to s3
#' @param py_loc python location, leave NULL to guess based on user from 
#'   Sys.Info(). Windows = anaconda3 install
#' @param s3_name name of S3 bucket, default is "dtc-rtic"
#' @return arcticdb datastore
#' @export
create_arctic <- function(api_keys = NULL, py_loc = NULL, s3_name = "dtc-rtic") 
  {
  if (is.null(api_keys)) {
    load("~/api_keys.RData")
  }
  if (!is.list(api_keys)) {
    rdat_file <- grep(".RData", api_keys, fixed = TRUE)
    if (length(rdat_file) == 0) {
      rdat_file <- FALSE
    }
    if (rdat_file) {
      load(api_keys)
    }
  }
  if (Sys.info()[["user"]] == "rstudio") {
    if (is.null(py_loc)) {
      py_loc <- "/usr/bin/python3"
    }
    use_python(py_loc)
  }
  adb <- try(import("arcticdb"))
  if ("try-error" %in% class(adb)) {
    if (Sys.info()[["sysname"]] == "Windows") {
      user <- Sys.info()[["user"]]
      if (is.null(py_loc)) {
        py_loc <- paste0("C:/Users/" , user, 
                         "/AppData/Local/anaconda3/")
      }
      use_python(py_loc)
      adb <- import("arcticdb")
    }
  }
  base_url <- paste0("s3://s3.us-east-1.amazonaws.com:", s3_name, "?")
  s3_url <- paste0(
    base_url,
    "access=", api_keys$s3$access_key,
    "&secret=", api_keys$s3$secret_key
  )
  adb$Arctic(s3_url)
}

