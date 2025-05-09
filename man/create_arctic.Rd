% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_arctic.R
\name{create_arctic}
\alias{create_arctic}
\title{Create ArcticDB datastore}
\usage{
create_arctic(api_keys = NULL, py_loc = NULL, s3_name = "dtc-rtic")
}
\arguments{
\item{api_keys}{list or ".RData" file path with keys to s3}

\item{py_loc}{python location, leave NULL to guess based on user from 
Sys.Info(). Windows = anaconda3 install}

\item{s3_name}{name of S3 bucket, default is "dtc-rtic"}
}
\value{
arcticdb datastore
}
\description{
Create ArcticDB datastore
}
