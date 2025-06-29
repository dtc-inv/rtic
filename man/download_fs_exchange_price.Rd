% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_data.R
\name{download_fs_exchange_price}
\alias{download_fs_exchange_price}
\title{Factset Global Prices API for End of Day Prices}
\usage{
download_fs_exchange_price(api_keys, ids, date_start, date_end, freq = "D")
}
\arguments{
\item{api_keys}{list of api keys}

\item{ids}{character vector of ids to download}

\item{date_start}{begging date, earliest start is 2006-01-03}

\item{date_end}{ending date}

\item{freq}{"D", or "M" for daily or monthly time-series}
}
\value{
json with data
}
\description{
Factset Global Prices API for End of Day Prices
}
\examples{
load("~/api_keys.RData")
download_fs_exchange_price(
  api_keys = api_keys,
  ids = c("AAPL", "MSFT"),
  date_start = "2025-05-01",
  date_end = "2025-05-15",
  freq = "D")
}
\seealso{
\code{\link{flatten_fs_global_prices}} for converting json list into
  a data.table
}
