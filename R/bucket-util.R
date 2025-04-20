try_bucket_ls <- function(api_keys, bucket, path) {
  x <- try(bucket$ls(path))
  if ("try-error" %in% class(x)) {
    # bucket <- init_bucket(api_keys)
    # assign("bucket", bucket, envir = .GlobalEnv)
    x <- try(bucket$ls(path))
    if ("try-error" %in% class(x)) {
      showModal(
        modalDialog(
          title = "Warning",
          "Time-out error with server, please try again.",
          easyClose = TRUE
        )
      )
      return(NULL)
    } else {
      return(x)
    }
  }
  return(x)
}

try_read_parquet <- function(api_keys, bucket, path, null_blank_df = FALSE) {
  x <- try(read_parquet(bucket$path(path)))
  if ("try-error" %in% class(x)) {
    # bucket <- init_bucket(api_keys)
    # assign("bucket", bucket, envir = .GlobalEnv)
    x <- try(read_parquet(bucket$path(path)))
    if ("try-error" %in% class(x)) {
      if (null_blank_df) {
        return(data.frame())
      } else {
        showModal(
          modalDialog(
            title = "Warning",
            "Time-out error with server, please try again."
          )
        )
        return(NULL)
      }
    } else {
      return(x)
    }
  }
  return(x)
}

try_write_parquet <- function(bucket, xdf, path) {
  x <- try(write_parquet(xdf, bucket$path(path)))
  if ("try-error" %in% class(x)) {
    x <- try(write_parquet(xdf, bucket$path(path)))
    if ("try-error" %in% class(x)) {
      showModal(
        modalDialog(
          title = "Warning",
          "Time-out error with server, please try again."
        )
      )
      return(NULL)
    }
  }
  return("success")
}

create_pm_opts <- function(api_keys, bucket) {
  pm_opts <- try_bucket_ls(api_keys, bucket, "ra/pm/")
  if (is.null(pm_opts)) {
    return("Error - try refreshing.")
  }
  names(pm_opts) <- gsub("ra/pm/", "", pm_opts)
  pm_opts
}

create_port_opts <- function(api_keys, bucket, path) {
  port_opts <- try_bucket_ls(api_keys, bucket, path)
  if (is.null(port_opts)) {
    return("Error - try refreshing.")
  }
  nm <- gsub(paste0(path, "/"), "", port_opts)
  nm <- gsub(".parquet", "", nm)
  names(port_opts) <- nm
  port_opts
}