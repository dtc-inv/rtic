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
    bd_key <- refresh_bd_key(api_keys, TRUE)
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
    assetId = NA,
    name = NA,
    cusip = NA,
    ticker = NA,
    identifier = NA,
    units = NA,
    emv = NA,
    returnInfo = NA
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
    df[i, 'assetId'] <- rd[[i]]$asset['assetId'][[1]]
    df[i, 'name'] <- rd[[i]]$asset['name'][[1]]
    df[i, 'cusip'] <- robcheck('cusip', rd[[i]]$asset)
    df[i, 'ticker'] <- robcheck('ticker', rd[[i]]$asset)
    df[i, 'identifier'] <- robcheck('identifier', rd[[i]]$asset)
    df[i, 'units'] <- rd[[i]]$returnInfo['units'][[1]]
    df[i, 'emv'] <- rd[[i]]$returnInfo['emv'][[1]]
    df[i, 'returnInfo'] <- rd[[i]]$returnInfo['returnDate'][[1]]
  }
  df$pctVal <- df$emv / sum(df$emv, na.rm = TRUE)
  return(df)
}

download_sec <- function(long_cik, short_cik, user_email) {
  doc_type <- 'nport-p'
  url <- paste0(
    'https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=',
    long_cik,
    '&type=',
    doc_type,
    '&dateb=&count=5&scd=filings&search_text=')
  # SEC requires email as a header for authentication, you don't have to register
  # email on their site, just need valid email address
  response <- httr::GET(url, add_headers(`User-Agent` = user_email))
  html_doc <- rvest::read_html(response)
  tbl <- rvest::html_table(html_doc)
  # might need to change which table to target if web design changes
  long_str <- tbl[[3]]$Description[1]
  file_date <- tbl[[3]]$`Filing Date`[1]
  # need to extract ##-##-## from table cell to target the url of the latest filing
  num_id <- stringr::str_extract(long_str, '[[:digit:]]+-[[:digit:]]+-[[:digit:]]+')
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
  df <- data.frame(
    name = NA,
    lei = NA,
    title = NA,
    cusip = NA,
    isin = NA, # conditional, required to list at least one additional ID
    altid = NA, # ID is usually ISIN, check for ISIN otherwise call alt
    balance = NA,
    units = NA,
    curCd = NA,
    exchangeRt = NA, # conditional, if no FX then this field doesn't exist
    valUSD = NA,
    pctVal = NA,
    payoffProfile = NA,
    assetCat = NA,
    issuerCat = NA,
    invCountry = NA,
    returnInfo = NA
  )
  for (i in 1:length(all_sec)) {
    x <- all_sec[[i]]
    df[i, 'name'] <- x$name
    df[i, 'lei'] <- x$lei
    df[i, 'title'] <- x$title
    df[i, 'cusip']  <- x$cusip
    if ('isin' %in% names(x$identifiers)) {
      df[i, 'isin'] <- x$identifiers$isin
      df[i, 'altid'] <- NA
    } else {
      df[i, 'isin'] <- NA
      df[i, 'altid'] <- x$identifiers[[1]][2]
    }
    df[i, 'balance'] <- x$balance
    df[i, 'units'] <- x$units
    if ('curCd' %in% names(x)) {
      df[i, 'curCd'] <- x$curCd
      df[i, 'exchangeRt'] <- NA
    } else if ('currencyConditional' %in% names(x)) {
      df[i, 'curCd'] <- x$currencyConditional[1]
      df[i, 'exchangeRt'] <- x$currencyConditional[2]
    } else {
      df[i, 'curCd'] <- NA
      df[i, 'exchangeRt'] <- NA
    }
    df[i, 'valUSD'] <- x$valUSD
    df[i, 'pctVal'] <- x$pctVal
    df[i, 'payoffProfile'] <- x$payoffProfile
    if ('assetCat' %in% names(x)) {
      df[i, 'assetCat'] <- x$assetCat
    } else if ('assetConditional' %in% names(x)) {
      df[i, 'assetCat'] <- x$assetConditional[2]
    } else {
      df[i, 'assetCat'] <- NA
    }
    if ('issuerCat' %in% names(x)) {
      df[i, 'issuerCat'] <- x$issuerCat[1]
    } else {
      df[i, 'issuerCat'] <- NA
    }
    df[i, 'invCountry'] <- x$invCountry
  }
  df[ ,'returnInfo'] <- as.Date(file_date)
  df$pctVal <- as.numeric(df$pctVal)
  df$pctVal <- df$pctVal / 100
  return(df)
}

#' @title Factset Global Prices API
#' @param api_keys list of api keys
#' @param ids character vector of ids to download
#' @param date_start begginig date, earliest start is 2006-01-03
#' @param date_end ending date
#' @param freq "D", or "M" for daily or monthly time-series
#' @return json with data
#' @export
download_fs_global_prices <- function(api_keys, ids, date_start, date_end, freq = "D") {
  username <- api_keys$fs$username
  password <- api_keys$fs$password
  ids[is.na(ids)] <- ""
  url <- "https://api.factset.com/content/factset-global-prices/v1/returns"
  request <- list(
    ids = as.list(ids),
    startDate = date_start,
    endDate = date_end,
    frequency = freq,
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

#' @export
flatten_fs_global_prices <- function(json) {
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
  totalReturn <- sapply(dat, '[[', 'totalReturn')
  if (is.list(totalReturn)) {
    totalReturn <- unlist(list_replace_null(totalReturn))
  }
  df <- data.frame(
    requestId = requestId,
    date = date,
    totalReturn = totalReturn
  )
  return(df)
}
