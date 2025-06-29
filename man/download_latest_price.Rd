% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_data.R
\name{download_latest_price}
\alias{download_latest_price}
\title{Download Last Business Day Price}
\usage{
download_latest_price(api_keys, ids, as_of = NULL)
}
\arguments{
\item{api_keys}{list of api keys}

\item{ids}{character vector of ids to download}

\item{as_of}{leave NULL for last trading day or specify as_of date for price}
}
\value{
data.frame with prices
}
\description{
Download Last Business Day Price
}
\examples{
load("~/api_keys.RData")
download_latest_price(api_keys, c("AAPL", "MSFT"), as.Date("2025-05-01"))
}
