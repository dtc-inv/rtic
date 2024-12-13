% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Database.R
\name{Database}
\alias{Database}
\title{Database Object}
\description{
Download and store holdings and price time-series data in s3 from various
  sources such as factset, blackdiamond, excel
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{api_keys}}{of api keys}

\item{\code{bucket}}{aws s3 bucket}

\item{\code{ac}}{ArcticDB object store}

\item{\code{tbl_msl}}{master security list: table of meta data for all investments}

\item{\code{tbl_cust}}{custodian list: table of meta data for CTFs and SMAs}

\item{\code{tbl_sec}}{SEC list: table of meta data for mutual funds and ETFs}

\item{\code{tbl_xl_mod}}{excel model list: table of meta data for model portfolios}

\item{\code{tbl_hold_field}}{holdings fields: table of fields / columns for
holdings data}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Database-new}{\code{Database$new()}}
\item \href{#method-Database-check_api_keys}{\code{Database$check_api_keys()}}
\item \href{#method-Database-ret_index}{\code{Database$ret_index()}}
\item \href{#method-Database-ret_etf}{\code{Database$ret_etf()}}
\item \href{#method-Database-ret_stock}{\code{Database$ret_stock()}}
\item \href{#method-Database-hold_sec}{\code{Database$hold_sec()}}
\item \href{#method-Database-hold_cust}{\code{Database$hold_cust()}}
\item \href{#method-Database-clone}{\code{Database$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-new"></a>}}
\if{latex}{\out{\hypertarget{method-Database-new}{}}}
\subsection{Method \code{new()}}{
Create a db object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$new(api_keys, py_loc)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{api_keys}}{list of api keys or `.RData` file location to load the list
of keys}

\item{\code{py_loc}}{optional file path where python is installed to be used
with `reticulate`}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-check_api_keys"></a>}}
\if{latex}{\out{\hypertarget{method-Database-check_api_keys}{}}}
\subsection{Method \code{check_api_keys()}}{
Check if api keys are properly specified
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$check_api_keys(api_keys)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{api_keys}}{list of api_keys}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-ret_index"></a>}}
\if{latex}{\out{\hypertarget{method-Database-ret_index}{}}}
\subsection{Method \code{ret_index()}}{
Update index returns from Factset
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$ret_index(ids = NULL, t_minus = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ids}}{leave NULL to update all indexes, or enter specific index
ids to update, note ids will have to first exist in the Master Security
List `tbl_msl`}

\item{\code{t_minus}}{how many months to download}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-ret_etf"></a>}}
\if{latex}{\out{\hypertarget{method-Database-ret_etf}{}}}
\subsection{Method \code{ret_etf()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$ret_etf(
  ids = NULL,
  date_start = NULL,
  date_end = Sys.Date() - 1,
  freq = "D"
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-ret_stock"></a>}}
\if{latex}{\out{\hypertarget{method-Database-ret_stock}{}}}
\subsection{Method \code{ret_stock()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$ret_stock(
  ids = NULL,
  date_start = NULL,
  date_end = Sys.Date(),
  freq = "D"
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-hold_sec"></a>}}
\if{latex}{\out{\hypertarget{method-Database-hold_sec}{}}}
\subsection{Method \code{hold_sec()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$hold_sec(
  dtc_name = NULL,
  user_email = "asotolongo@diversifiedtrust.com",
  save_to_db = TRUE,
  return_data = FALSE
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-hold_cust"></a>}}
\if{latex}{\out{\hypertarget{method-Database-hold_cust}{}}}
\subsection{Method \code{hold_cust()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$hold_cust(dtc_name = NULL, save_to_db = TRUE, return_data = FALSE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Database-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Database$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
