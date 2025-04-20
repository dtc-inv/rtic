#' @title Select Asset Input
#' @param id namespace
#' @details build or add onto a portfolio by selecting assets, e.g., CTFs,
#'   ETFs, mutual funds. selectizeInput choices filled with
#'   `setup_asset_select_server`. The action button adds after one or multiple
#'   assets have been selected.
#' @export
setup_asset_select_input <- function(id) {
  edate <- last_us_trading_day()
  sdate <- ceiling_date(edate %m-% months(239), "months") - 1
  tagList(
    fluidRow(
      div(
        style = "display:inline-block; margin-left:10px",
        dateRangeInput(
          inputId = NS(id, "dt_rng"),
          label = "Analysis Date Range",
          start = sdate,
          end = edate
        )
      ),
      div(
        style = "display:inline-block; margin-left:10px",
        radioButtons(
          inputId = NS(id, "freq"),
          label = "Periodicity",
          choices = c("days", "weeks", "months"),
          selected = "months",
          inline = TRUE
        )
      )
    ),
    div(
      selectizeInput(
        inputId = NS(id, "select_bench"),
        label = "Select Benchmark",
        choices = NULL,
        multiple = FALSE
      )
    ),
    div(
      style = "display:inline-block",
      actionButton(
        inputId = NS(id, "add_asset"),
        label = "Add Selection",
        icon = icon("plus"),
        class = "btn btn-primary"
      )
    ),
    div(
      style = "display:inline-block; margin-left:5px",
      actionButton(
        inputId = NS(id, "clear"),
        label = "Clear Table",
        icon = icon("eraser", lib = "font-awesome")
      )
    ),
    div(
      style = "display:inline-block; margin-left:5px",
      actionButton(
        inputId = NS(id, "date"),
        label = "Get Dates",
        icon = icon("calendar", lib = "font-awesome")
      )
    ),
    div(
      style = "display:inline-block; margin-left:5px",
      actionButton(
        inputId = NS(id, "run"),
        label = "Run Analysis",
        icon = icon("chart-line", lib = "font-awesome"),
        class = "btn btn-success"
      )
    ),
    br(),
    br(),
    div(
      style = "display:inline-block",
      selectizeInput(
        inputId = NS(id, "select_asset"),
        label = "Select Investment(s)",
        choices = NULL,
        multiple = TRUE
      )
    ),
    div(
      style = "display:inline-block; margin-left:5px",
      selectizeInput(
        inputId = NS(id, "filter_asset"),
        label = "Filter",
        choices = NULL,
        multiple = TRUE
      )
    ),
    div(
      style = "display:inline-block; margin-left:5px",
      actionButton(
        inputId = NS(id, "reset_filter"),
        label = "Reset Filters",
        style = "display:inline-block"
      )
    )
  )
}

#' @title Asset Selection Server
#' @export
setup_asset_select_server <- function(id, rv, ac) {
  moduleServer(id, function(input, output, session) {
    observe({
      sec_type <- na.omit(unique(rv$msl$SecType))
      rv$asset_choices <- rv$msl$DtcName
      updateSelectizeInput(
        session,
        "filter_asset",
        choices = sec_type,
        selected = ""
      )
      updateSelectizeInput(
        session,
        "select_bench",
        choices = rv$msl$DtcName[rv$msl$SecType %in% 
          c("index", "private-index", "hfr-index")],
        selected = "MSCI ACWI"
      )
      updateSelectizeInput(
        session,
        "select_asset",
        choices = create_asset_choices(rv$msl),
        server = TRUE
      )
    })
    # date range ----
    observeEvent(input$dt_rng, {
      end <- last_us_trading_day()
      start <- ceiling_date(end %m-% months(239), "months") - 1
      minus_6_mo <- add_with_rollback(end, months(-6))
      if (start > minus_6_mo) {
        showModal(
          modalDialog(
            title = "Date Error!",
            "You must select at least 6 months between start and end date."
          )
        )
        rv$dt_rng <- c(minus_6_mo, end)
      }
      rv$dt_rng <- c(start, end)
    }) 
    # freq ----
    observeEvent(input$freq, {
      if (input$freq == "months") {
        end <- eo_month(input$dt_rng[2])
        # updateDateRangeInput(
        #   session,
        #   "dt_rng",
        #   end = end
        # )  
        rv$dt_rng[2] <- end
      }
      if (input$freq == "weeks") {
        end <- lubridate::ceiling_date(input$dt_rng[2], "weeks") - 2
        # updateDateRangeInput(
        #   session,
        #   "dt_rng",
        #   end = end
        # )
        rv$dt_rng[2] <- end
      }
      rv$freq <- input$freq
    })
    
    # reset filter ----
    observeEvent(input$reset_filter, {
      rv$asset_choices <- create_asset_choices(rv$msl)
      updateSelectizeInput(
        session,
        "select_asset",
        choices = rv$asset_choices
      )
      updateSelectizeInput(
        session,
        "filter_asset",
        selected = ""
      )
    })
    # filter ---- 
    observeEvent(input$filter_asset, {
      tbl_msl <- rv$msl
      if (input$filter_asset[1] != "") {
        tbl_msl <- filter(tbl_msl, SecType %in% input$filter_asset)
      } else {
        tbl_msl <- filter(tbl_msl, SecType %in% sec_type)
      }
      rv$asset_choices <- create_asset_choices(tbl_msl)
      updateSelectizeInput(
        session,
        "select_asset",
        choices = rv$asset_choices,
        selected = NULL,
        server = TRUE
      )
    })
    # add asset ----
    observeEvent(input$add_asset, {
      if (is.null(input$select_asset )) {
        showModal(
          modalDialog(
            title = "Warning",
            "You need to select an investment before adding.",
            easyClose = TRUE
          )
        )
        return(NULL)
      }
      new_funds <- data.frame(Asset = input$select_asset, Start = NA, 
                              End = NA)
      rv$tbl_asset <- rbind(new_funds, rv$tbl_asset)
      updateSelectizeInput(
        session,
        "select_asset",
        selected = NULL,
        choices = create_asset_choices(rv$msl),
        server = TRUE
      )
    })
    # clear ----
    observeEvent(input$clear, {
      rv$tbl_asset <- data.frame()
      rv$ra$asset <- NA
    })
    # get dates ----
    observeEvent(input$date, {
      if (nrow(rv$tbl_asset) > 0) {
        res <- handle_returns(ac, rv, input)
        if (is.null(res$combo)) {
          return(NULL)
        }
        r <- res$combo$x
        first_days <- rep(NA, ncol(r))
        last_days <- first_days
        for (i in 1:ncol(r)) {
          first_days[i] <- zoo::index(na.omit(r[, i]))[1]
          last_days[i] <- zoo::index(na.omit(r[, i]))[nrow(na.omit(r[, i]))]
        }
        rv$tbl_asset$Start <- as.Date(first_days, origin = "1970-01-01")
        rv$tbl_asset$End <- as.Date(last_days, origin = "1970-01-01")
        res <- clean_ret(r)
        rv$ra$asset <- res$ret
      }
    })
    # run ----
    observeEvent(input$run, {
      if (nrow(rv$tbl_asset) > 0) {
        res <- handle_returns(ac, rv, input)
        if (is.null(res$combo)) {
          if (res$error == "read returns") {
            showModal(
              modalDialog(
                "Error reading returns.",
                title = "Error"
              )
            )
          }
          if (res$error == "duplicate") {
            showModal(
              modalDialog(
                "Only asset(s) selected are duplicates of benchmark or 
                risk-free indexes.",
                title = "Error"
              )
            )
          }
          return(NULL)
        }
        if (!is.null(res$warn)) {
          if (res$warn == "duplicate") {
     
          }
        }
        combo <- res$combo
        rv$ra$asset <- combo$x
        rv$ra$bench <- combo$b
        rv$ra$rf <- combo$rf
        rv$dt_rng <- input$dt_rng
      } else {
        showModal(
          modalDialog(
            title = "Warning",
            "You need to add at least one investment!",
            easyClose = TRUE
          )
        )
        return(NULL)
      }
      rv <- return_analytics(rv)
    })
    return(rv)
  })
}

# handle returns ----
handle_returns <- function(ac, rv, input) {
  withProgress(message = "loading returns", {
    res <- list(
      combo = NULL,
      warn = NULL,
      error = NULL
    )
    r <- try(read_ret(rv$tbl_asset$Asset, ac))
    if (inherits(r, "try-error")) {
      res$error <- "read returns"
      return(res)
    }
    incProgress(0.5)
    rf <- try(read_ret("BofAML U.S. Treasury Bill 3M", ac))
    if (inherits(rf, "try-error")) {
      res$error <- "read returns"
      return(res)
    }
    incProgress(0.20)
    b <- try(read_ret(input$select_bench, ac))
    if (inherits(b, "try-error")) {
      res$error <- "read returns"
      return(res)
    }
    incProgress(0.20)
    nm <- c(colnames(b), colnames(rf))
    ix <- colnames(r) %in% nm
    if (any(ix)) {
      res$warn <- "duplicate"
      r <- r[, !ix]
    }
    if (all(ix)) {
      res$error <- "duplicate"
      return(res)
    }
    if (input$freq == "months") {
      freq <- "months"
    } else {
      if (guess_freq(r) == "months") {
        freq <- "months"
        rv$freq <- "months"
      } else {
        freq <- input$freq
      }
    }
    if (freq == "days") {
      freq <- NULL
    }
    combo <- try(clean_asset_bench_rf(r, b, rf, freq, eps = 1))
    if (inherits(combo, "try-error")) {
      res$error <- "combo"
    } else {
      res$combo <- combo
    }
    incProgress(0.1)
  })
  return(res)
}

try_catch_we <- function(expr) {
  warn <- NULL
  w_handler <- function(w) {
    warn <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(
    tryCatch(expr, error = function(e) e), warning = w_handler),
    warning = warn)
}

#' @title Selected Asset Table UI
#' @param id namespace
#' @export
reactable_ui <- function(id) {
  reactable::reactableOutput(NS(id, "tbl"))
}

#' @title Selected Asset Table Server
#' @param id namespace
#' @param rv reactive values
#' @export
setup_tbl_asset_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- reactable::renderReactable({
      if (nrow(rv$tbl_asset > 0)) {
        reactable::reactable(rv$tbl_asset)
      }
    })
  })
}

# reactive vals ----

#' @title Initiate Reactive Values
#' @export
setup_reactive_vals <- function() {
  ra <- set_up_analytics_result()
  reactiveValues(
    ra = ra,
    dt_rng = NULL,
    freq = "months",
    asset_choices = NULL,
    tbl_asset = data.frame(),
    port = data.frame(Asset = rep(NA, 10), CapWgt = rep(NA, 10)),
    old_port = data.frame(),
    custom_ts = data.frame(),
    upload_ts = data.frame(),
    display_ts = data.frame(),
    new_ts = FALSE
  )
}

#' @title Initiate Skeleton of Return Analytics Output
set_up_analytics_result <- function() {
  list(all_ret = NA, ret = NA, msl = NA,
       stats = list(ts = NA, cht = NA),
       trail_perf = list(ts = NA, cht = NA),
       wealth = list(ts = NA, cht = NA),
       capm = list(ts = NA, cht = NA),
       worst_dd = list(ts = NA, cht = NA),
       bench_dd = list(ts = NA, cht = NA),
       all_dd = list(ts = NA, cht = NA),
       roll_ret = list(ts = NA, cht = NA),
       roll_act_ret = list(ts = NA, cht = NA),
       roll_corr = list(ts = NA, cht = NA),
       roll_beta = list(ts = NA, cht = NA),
       roll_vol = list(ts = NA, cht = NA),
       roll_te = list(ts = NA, cht = NA),
       roll_sharpe = list(ts = NA, cht = NA),
       xl_out = NA)
}

create_bucket <- function(bucket, api_keys) {
  if ("SubTreeFileSystem" %in% class(bucket)) {
    return(bucket)
  } else {
    bucket <- init_bucket(api_keys)
    assign("bucket", bucket, envir = .GlobalEnv)
    return(bucket)
  }
}

init_bucket <- function(api_keys) {
  arrow::s3_bucket(
    "dtc-inv",
    access_key = api_keys$s3$access_key,
    secret_key = api_keys$s3$secret_key
  )
}

# label asset choices ----
create_asset_choices <- function(msl) {
  ix <- msl$SecType == "stock"
  stock_nm <- paste0(msl$DtcName[ix], " (", msl$Ticker[ix], ")")
  choices <- as.list(msl$DtcName)
  names(choices) <- msl$DtcName
  names(choices)[ix] <- stock_nm
  return(choices)  
}