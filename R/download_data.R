#' @export
refresh_bd_key <- function(api_keys, save_to_n = FALSE, save_local = FALSE) {
  bd_key <- api_keys$bd_key
  response <- httr::POST('https://login.bdreporting.com/connect/token',
                         encode = 'form',
                         body = list(grant_type = 'password',
                                     client_id = bd_key$client_id,
                                     client_secret = bd_key$client_secret,
                                     username = bd_key$username,
                                     password = bd_key$password))
  tk <- jsonlite::parse_json(response)
  bd_key$refresh_token <- tk$access_token
  if (save_to_n) {
    api_keys$bd_key <- bd_key
    save(api_keys, 
         file = 'N:/Investment Team/DATABASES/CustomRet/keys/api_keys.RData')
  }
  if (save_local) {
    api_keys$bd_key <- bd_key
    save(api_keys, file = "~/api_keys.RData")
  }
  return(bd_key)
}

#' @title Get Batch ID for bulk download
#' @param api_keys list of API Keys
#' @param as_of optional date, defaults to last trading day, transactions
#'   will default to last 7 days
#' @return batch id to pass to `download_bd_batch` for bulk download
#' @export
download_bd_batch_id <- function(api_keys, as_of = NULL) {
  if (is.null(as_of)) {
    as_of <- last_us_trading_day()
  }
  bd_key <- api_keys$bd_key
  body <- list(returnDate = format(as_of, "%m-%d-%Y"), 
               batchFiles = list("accounts"), 
               includeTransactions = TRUE, includeHoldings = TRUE)
  response <- httr::POST(
    'https://api.blackdiamondwealthplatform.com/batchV5',
    accept_json(),
    add_headers(
      Authorization = paste0('Bearer ', bd_key$refresh_token),
      `Ocp-Apim-Subscription-Key` = bd_key$bd_subkey
    ),
    encode = 'json',
    body = body)
  if (response$status_code != 200) {
    bd_key <- refresh_bd_key(api_keys, FALSE, TRUE)
    response <- httr::POST(
      'https://api.blackdiamondwealthplatform.com/batchV5',
      accept_json(),
      add_headers(
        Authorization = paste0('Bearer ', bd_key$refresh_token),
        `Ocp-Apim-Subscription-Key` = bd_key$bd_subkey
      ),
      encode = 'json',
      body = body)
  }
  res <- parse_json(response)
  return(res)
}

#' @title BlackDiamond Batch Download
#' @param api_keys list of API Keys
#' @param batch_id from `download_bd_batch_id`
#' @return response with raw data
#' @export
download_bd_batch <- function(api_keys, batch_id) {
  bd_key <- api_keys$bd_key
  url <- paste0(
    "https://api.blackdiamondwealthplatform.com/batch/",
    batch_id
  )
  resp <- httr::GET(
    url,
    accept_json(),
    add_headers(
      Authorization = paste0('Bearer ', bd_key$refresh_token),
      `Ocp-Apim-Subscription-Key` = bd_key$bd_subkey
    ),
    encode = 'json'
  )
  return(resp)
}

#' @title Unzip Response from BlackDiamond Batch Download
#' @param resp response returned by `download_bd_batch`
#' @return json data (list)
#' @export
unzip_bd_batch <- function(resp) {
  tmp <- tempfile(fileext = ".zip")
  brio::write_file_raw(resp$content, path = tmp)
  ufile <- unzip(tmp)
  read_json(ufile)
}

#' @title Parse JSON file from BlackDiamond Batch Download Process
#' @param ac arcticDB datastore
#' @param json from `unzip_bd_batch`
#' @return does not return data, transactions, market values, and holdings
#'   are stored in `ac`
#' @export
handle_bd_batch <- function(ac, json, as_of) {
  lib <- get_all_lib(ac)
  cust <- lib$`meta-tables`$read("cust")$data
  for (i in 1:length(json)) {
    x <- json[[i]]
    if (!x$Id %in% cust$BdAccountId) {
      next
    }
    print(x$Name)
    ix <- match(x$Id, cust$BdAccountId)
    dtc_name <- cust$DtcName[ix]
    mv <- x$Details$TotalEmv
    cf_dat <- data.frame(
      DtcName = dtc_name, 
      TradeDate = as_of, 
      Action = "EndingValue", 
      Value = mv, 
      TransactionId = paste0("EndingValue", "-", as_of)
    )
    cf_type <- sapply(x$Transactions, "[[", "ExternalFlowAffect")
    is_cf <- cf_type != "NotExternalFlow"
    if (any(is_cf)) {
      cf <- x$Transactions[is_cf]
      for (j in 1:length(cf)) {
        dir <- extract_list_null(cf[[j]], "ExternalFlowAffect")
        if (dir == "Withdrawal") {
          cf_dat[j + 1, "TradeDate"] <- extract_list_null(cf[[j]], "TradeDate")
          cf_dat[j + 1, "Action"] <- dir
          cf_dat[j + 1, "Value"] <- -extract_list_null(cf[[j]], "MarketValue")
          cf_dat[j + 1, "TransactionId"] <- extract_list_null(cf[[j]], "TransactionID")
        } else if (dir == "Contribution") {
          cf_dat[j + 1, "TradeDate"] <- extract_list_null(cf[[j]], "TradeDate")
          cf_dat[j + 1, "Action"] <- dir
          cf_dat[j + 1, "Value"] <- extract_list_null(cf[[j]], "MarketValue")
          cf_dat[j + 1, "TransactionId"] <- extract_list_null(cf[[j]], "TransactionID")
        } else {
         warning(paste0(dtc_name, " unfamiliar transaction"))
        } # end if dir
      } # end cf loop
    } # end any cf
    cf_dat$DtcName <- dtc_name
    old_dat <- try(lib$transactions$read(dtc_name)$data)
    if ("try-error" %in% class(old_dat)) {
      old_dat <- data.frame()
    }
    combo <- rbind_tx(old_dat, cf_dat)
    lib$transactions$write(dtc_name, combo)
  } # end json loop
}

#' @title Extract field from list checking for NULL
#' @param x list
#' @param nm field (string)
#' @return NA if field is NULL or field value if not NULL
#' @export
extract_list_null <- function(x, nm) {
  if (is.null(x[nm][[1]])) {
    return(NA)
  } else {
    return(x[nm][[1]])
  }
}

#' @title Rowbind transactions
#' @param old existing data.frame of transactions
#' @param new newly downloaded transactions
#' @return combo of new and old, removes duplicated values for new from old
#' @export
rbind_tx <- function(old, new) {
  ix <- na.omit(match(old$TransactionId, new$TransactionId))
  if (length(ix) > 0) {
    old <- old[-ix, ]
  }
  rbind(old, new)
}

#' @export
download_bd <- function(account_id, api_keys, as_of = NULL) {
  if (is.null(as_of)) {
    as_of <- last_us_trading_day()
  }
  bd_key <- api_keys$bd_key
  response <- httr::POST(
    'https://api.blackdiamondwealthplatform.com/account/Query/HoldingDetailSearch',
    accept_json(),
    add_headers(
      Authorization = paste0('Bearer ', bd_key$refresh_token),
      `Ocp-Apim-Subscription-Key` = bd_key$bd_subkey
    ),
    encode = 'json',
    body = list(accountID = account_id, asOf = as_of,
                onlyCurrentHoldings = TRUE, limit = 100000,
                include = list(returnInfo = TRUE, assets = TRUE))
  ) # end POST
  if (response$status_code != 200) {
    bd_key <- refresh_bd_key(api_keys, FALSE, TRUE)
    response <- httr::POST(
      'https://api.blackdiamondwealthplatform.com/account/Query/HoldingDetailSearch',
      accept_json(),
      add_headers(
        Authorization = paste0('Bearer ', bd_key$refresh_token),
        `Ocp-Apim-Subscription-Key` = bd_key$bd_subkey
      ),
      encode = 'json',
      body = list(accountID = account_id, asOf = as_of,
                  onlyCurrentHoldings = TRUE, limit = 100000,
                  include = list(returnInfo = TRUE, assets = TRUE))
    ) # end POST
  }
  # parse jason and extract data
  rd <- parse_json(response)
  rd <- rd$data
  if (length(rd) == 0) {
    warning('no data found')
    return(NULL)
  }
  # convert json list into data.frame by looping through each holding
  df <- data.frame(
    Name = NA,
    Cusip = NA,
    Ticker = NA,
    Identifier = NA,
    Units = NA,
    Value = NA,
    TimeStamp = NA
  )
  robcheck <- function(x, l) {
    if (x %in% names(l)) {
      if (!is.null(l[[x]])) {
        return(l[x])
      } else {
        return(NA)
      }
    } else {
      return(NA)
    }
  }
  for (i in 1:length(rd)) {
    df[i, 'Name'] <- rd[[i]]$asset['name'][[1]]
    df[i, 'Cusip'] <- robcheck('cusip', rd[[i]]$asset)
    df[i, 'Ticker'] <- robcheck('ticker', rd[[i]]$asset)
    df[i, 'Identifier'] <- robcheck('identifier', rd[[i]]$asset)
    df[i, 'Units'] <- rd[[i]]$returnInfo['units'][[1]]
    df[i, 'Value'] <- rd[[i]]$returnInfo['emv'][[1]]
    df[i, 'TimeStamp'] <- rd[[i]]$returnInfo['returnDate'][[1]]
  }
  df$CapWgt <- df$Value / sum(df$Value, na.rm = TRUE)
  return(df)
}

#' @export
download_sec <- function(long_cik, short_cik, user_email) {
  doc_type <- 'nport-p'
  url <- paste0(
    'https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=',
    long_cik,
    '&type=',
    doc_type,
    '&dateb=&count=5&scd=filings&search_text=')
  # SEC requires email as a header for authentication, you don't have to 
  # register email on their site, just need valid email address
  response <- httr::GET(url, add_headers(`User-Agent` = user_email))
  html_doc <- rvest::read_html(response)
  tbl <- rvest::html_table(html_doc)
  # might need to change which table to target if web design changes
  long_str <- tbl[[3]]$Description[1]
  file_date <- tbl[[3]]$`Filing Date`[1]
  # need to extract ##-##-## from table cell to target the url of the latest 
  # filing
  num_id <- stringr::str_extract(long_str, 
                                 '[[:digit:]]+-[[:digit:]]+-[[:digit:]]+')
  num_str <- gsub('-', '', num_id)
  # now we can target url of latest filing and get the holdings
  url <- paste0('https://www.sec.gov/Archives/edgar/data/',
                short_cik, '/', num_str, '/primary_doc.xml')
  response <- httr::GET(url, add_headers(`User-Agent` = user_email))
  xml_file <- xml2::read_xml(response)
  doc <- XML::xmlParse(xml_file)
  root <- XML::xmlRoot(doc)
  # xmlToList can take ~10+ seconds for large amount of holdings, is there more efficient way?
  all_list <- XML::xmlToList(root)
  # current node is invstOrSecs for the holdings
  all_sec <- all_list$formData$invstOrSecs
  xdf <- data.frame(
    Name = NA,
    Lei = NA,
    Cusip = NA,
    Isin = NA, # conditional, required to list at least one additional ID
    Identifier = NA, # ID is usually ISIN, check for ISIN otherwise call alt
    Value = NA,
    Units = NA,
    Currency = NA,
    CapWgt = NA,
    Payoff = NA,
    AssetCat = NA,
    IssuerCat = NA,
    Country = NA,
    TimeStamp = NA,
    ValueLevel = NA,
    Maturity = NA,
    CouponType = NA,
    CouponRate = NA
  )
  for (i in 1:length(all_sec)) {
    x <- all_sec[[i]]
    xdf[i, 'Name'] <- get_list_fld(x, "name")
    xdf[i, 'Lei'] <- get_list_fld(x, "lei")
    xdf[i, 'Cusip']  <- get_list_fld(x, "cusip")
    if ('isin' %in% names(x$identifiers)) {
      xid <- x$identifiers
      xdf[i, 'Isin'] <- get_list_fld(xid, "isin")
      xdf[i, 'Identifier'] <- get_list_fld(xid, "altid")
    }
    xdf[i, 'Value'] <- get_list_fld(x, "valUSD")
    xdf[i, 'Units'] <- get_list_fld(x, "units")
    if ('curCd' %in% names(x)) {
      xdf[i, 'Currency'] <- get_list_fld(x, "curCd")
    }
    if ('currencyConditional' %in% names(x)) {
      xdf[i, 'Currency'] <- get_list_fld(x, "curCd")
    }
    xdf[i, 'CapWgt'] <- get_list_fld(x, "pctVal")
    xdf[i, 'Payoff'] <- get_list_fld(x, "payoffProfile")
    xdf[i, "AssetCat"] <- get_list_fld(x, "assetCat")
    xdf[i, "IssuerCat"] <- get_list_fld(x, "issuerCat")
    xdf[i, 'Country'] <- get_list_fld(x, "invCountry")
  }
  xdf[ ,'TimeStamp'] <- as.Date(file_date)
  xdf$CapWgt <- as.numeric(xdf$CapWgt)
  xdf$CapWgt <- xdf$CapWgt / 100
  return(xdf)
}

#' @title Factset Global Prices API for total returns
#' @param api_keys list of api keys
#' @param ids character vector of ids to download
#' @param date_start beginning date, earliest start is 2006-01-03
#' @param date_end ending date
#' @param freq "D", or "M" for daily or monthly time-series
#' @return json with data
#' @export
download_fs_global_prices <- function(api_keys, ids, date_start, date_end,
                                      freq = "D") {
  username <- api_keys$fs$username
  password <- api_keys$fs$password
  ids[is.na(ids)] <- ""
  url <- "https://api.factset.com/content/factset-global-prices/v1/returns"
  request <- list(
    ids = as.list(ids),
    startDate = date_start,
    endDate = date_end,
    frequency = freq,
    currency = "USD",
    dividendAdjust = "EXDATE_C",
    batch = "N"
  )
  response <- httr::POST(
    url, authenticate(username, password), body = request,
    add_headers(Accept = 'application/json'), encode = 'json')
  output <- rawToChar(response$content)
  json <- parse_json(output)
  return(json)
}

#' @title Factset Global Prices API for End of Day Prices
#' @param api_keys list of api keys
#' @param ids character vector of ids to download
#' @param date_start begginig date, earliest start is 2006-01-03
#' @param date_end ending date
#' @param freq "D", or "M" for daily or monthly time-series
#' @return json with data
#' @export
download_fs_exchange_price <- function(api_keys, ids, date_start, date_end,
                                       freq = "D") {
  username <- api_keys$fs$username
  password <- api_keys$fs$password
  ids[is.na(ids)] <- ""
  url <- "https://api.factset.com/content/factset-global-prices/v1/prices"
  request <- list(
    ids = as.list(ids),
    startDate = date_start,
    endDate = date_end,
    frequency = freq,
    fields = list("price"),
    batch = "N"
  )
  response <- httr::POST(
    url, authenticate(username, password), body = request,
    add_headers(Accept = 'application/json'), encode = 'json')
  output <- rawToChar(response$content)
  json <- parse_json(output)
  return(json)
}

#' @title Download Last Business Day Price
#' @param api_keys list of api keys
#' @param ids character vector of ids to download
#' @param as_of leave NULL for last trading day or specify as_of date for price
#' @return data.frame with prices
#' @export 
download_latest_price <- function(api_keys, ids, as_of = NULL) {
  as_of <- last_us_trading_day(as_of)
  json <- download_fs_exchange_price(api_keys, ids, as_of, as_of)
  flatten_fs_global_prices(json, TRUE)
}

#' @title Factset Formula API: RA_RET
#' @param id character representing id (only supports one id)
#' @param t_minus number of months of desired time-series, numeric value
#' @param freq "D", or "M" for daily or monthly time-series
#' @return xts time-series
#' @export
download_fs_ra_ret <- function(id, api_keys, t_minus = 12, freq = 'D') {
  username <- api_keys$fs$username
  password <- api_keys$fs$password
  base_url <- "https://api.factset.com/formula-api/v1/time-series?ids=$IDS&formulas="
  request <- paste0(
    base_url,
    'RA_RET(',
    '"',
    id,
    '"',
    ",",
    "-",
    t_minus,
    "/0/0,0,", freq,
    ",FIVEDAY,USD,1)&flatten=Y"
  )
  response <- httr::GET(request, authenticate(username, password))
  output <- rawToChar(response$content)
  dat <- parse_json(output)
  dat <- dat[[1]]
  dt <- lapply(dat, '[[', 'date')
  dt <- list_replace_null(dt)
  val <- lapply(dat, '[[', 2)
  val <- list_replace_null(val)
  res <- xts(unlist(val) / 100, as.Date(unlist(dt)))
  colnames(res) <- id
  return(res)
}

#' @title Download Factset formula API
#' @param api_keys list containing api keys
#' @param ids vector of ids to download
#' @param formulas formula to download
#' @param type "ts" for times-series or "cs" for cross-sectional
#' @param flatn boolean to add "flatten=Y" to api url
#' @return json data
#' @export
download_fs_formula <- function(api_keys, ids, formulas, type = c('ts', 'cs'),
                                flatn = TRUE) {
  if (type[1] == 'ts') {
    struc <- 'time-series'
  } else {
    struc <- 'cross-sectional'
  }
  username <- api_keys$fs$username
  password <- api_keys$fs$password
  ids[is.na(ids)] <- ""
  request <- paste0(
    "https://api.factset.com/formula-api/v1/",
    struc,
    "?ids=",
    paste0(ids, collapse = ","),
    "&formulas=",
    paste0(formulas, collapse = ",")
  )
  if (flatn) {
    request <- paste0(request, '&flatten=Y')
  }
  response <- httr::GET(request, authenticate(username, password))
  print(response$status)
  output <- rawToChar(response$content)
  json <- parse_json(output)
  return(json)
}

#' @export
flatten_fs_formula <- function(json) {
  create_df <- function(x) {
    y <- try(as.data.frame(x))
    if ("try-error" %in% class(y)) {
      return(data.frame())
    } else {
      return(y)
    }
  }
  df_list <- lapply(json$data, create_df)
  do.call("rbind", df_list)
}

#' @export
flatten_fs_global_prices <- function(json, price = FALSE) {
  if ('status' %in% names(json)) {
    if (json$status == "Bad Request") {
      warning('bad request, returning empty data.frame')
      return(data.frame())
    }
  }
  dat <- json$data
  requestId <- sapply(dat, '[[', 'requestId')
  if (is.list(requestId)) {
    requestId <- unlist(list_replace_null(requestId))
  }
  date <- sapply(dat, '[[', 'date')
  if (is.list(date)) {
    date <- unlist(list_replace_null(date))
  }
  if (price) {
    price <- sapply(dat, '[[', "price")
    if (is.list(price)) {
      price <- unlist(list_replace_null(price))
    }
    df <- data.frame(
      RequestId = requestId,
      Date = date,
      Price = price
    )
  } else {
    totalReturn <- sapply(dat, '[[', 'totalReturn')
    if (is.list(totalReturn)) {
      totalReturn <- unlist(list_replace_null(totalReturn))
    }
    df <- data.frame(
      RequestId = requestId,
      Date = date,
      TotalReturn = totalReturn
    )
  }
  return(df)
}

#' @export
read_private_xts <- function(file_nm, field_nm) {
  dat <- readxl::read_excel(file_nm)
  dat_xts <- xts(dat$`Return*`, as.Date(dat$Date))
  colnames(dat_xts) <- field_nm
  return(dat_xts)
}

#' @title Read HFR Returns CSV Export
#' @param file_nm file name of csv file, e.g., 
#'   "C:/users/asotolongo/Downloads/HFRI Indices.csv"
#' @return xts object of returns
#' @export
read_hfr_csv <- function(file_nm) {
  dat <- read.csv(file_nm)
  dt <- colnames(dat)[8:ncol(dat)]
  dt <- gsub("X", "", dt)
  dt <- paste0(dt, ".01")
  dt <- as.Date(dt, format = "%Y.%m.%d")
  dt <- eo_month(dt)
  r <- xts(t(dat[, 8:ncol(dat)]), dt)
  colnames(r) <- dat$FUND_NAME
  return(r)
}

upload_ctf_monthly <- function(ac, xl_path = NULL, skip = 4) {
  lib <- get_all_lib(ac)
  tbl_cust <- lib$`meta-tables`$read("cust")
  if (is.null(xl_path)) {
    xl_path <- "N:/Investment Team/DATABASES/FACTSET/BMO NAV & Platform Return Upload.xlsx"
  }
  dat <- read_xts(xl_path, skip = skip)
  ix <- match(tbl_cust$WorkupId, colnames(dat))
  if (any(ix)) {
    if (all(ix)) {
      stop("no values found")
    }
    warning("missing values created when matching workup excel to custodian library")
    ix <- na.omit(ix)
    
  }
  r <- dat[, ix]
  r <- r["1994/"] / 100
  lib$returns$write("ctf-monthly", xts_to_arc(r))
}