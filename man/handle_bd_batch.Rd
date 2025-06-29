% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_data.R
\name{handle_bd_batch}
\alias{handle_bd_batch}
\title{Parse JSON file from BlackDiamond Batch Download Process}
\usage{
handle_bd_batch(ac, json, as_of)
}
\arguments{
\item{ac}{arcticDB datastore}

\item{json}{data read in from \code{\link{unzip_bd_batch}}}
}
\value{
does not return data, transactions, market values, and holdings
  are stored in arcticDB cloud (S3) through \code{ac}
}
\description{
Parse JSON file from BlackDiamond Batch Download Process
}
\seealso{
\code{\link{unzip_bd_batch}} and \code{\link{download_bd_batch}}
  for downloading bulk data from black diamond and \code{\link{create_artic}}
  for arcticDB datastore
}
