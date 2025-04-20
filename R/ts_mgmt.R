ts_user_input <- function(id) {
  tagList(
    fluidRow(
      column(5,
       selectizeInput(
         inputId = NS(id, "select_folder"),
         label = "Select PM Folder",
         choices = NULL
        ),
       fileInput(
         inputId = NS(id, "upload"), 
         label = "Upload File"
        ),
       div(
         style = "margin-top:5px",
         actionButton(
           NS(id, "exe"),
           "Process Upload",
           class = "btn btn-primary"
         )
       )
      ),
      column(5,
       actionButton(
         inputId = NS(id, "add"),
         label = "Add to Analysis",
         class = "btn btn-success"
       ),
       br(),
       br(),
       downloadButton(
         outputId = NS(id, "download"), 
         label = "Download Template",
         class = "btn btn-primary"
        )
      )
    ),
    br(),
    reactableOutput(NS(id, "tbl_custom_ts"))    
  )
}

ts_server <- function(id, api_keys, rv) {
  moduleServer(id, function(input, output, session) {
    bucket <- arrow::s3_bucket(
      "dtc-inv",
      access_key = api_keys$s3$access_key,
      secret_key = api_keys$s3$secret_key
    )
    pm_opts <- create_pm_opts(api_keys, bucket)
    
    updateSelectizeInput(
      session = session,
      inputId = "select_folder",
      choices = pm_opts 
    )
    
    observeEvent(input$select_folder, {
      if (input$select_folder == "") {
        return(NULL)
      }
      dat <- try(read_parquet(bucket$path(
        paste0(input$select_folder, "/time-series/monthly.parquet"))))
      if ("try-error" %in% class(dat)) {
        dat <- data.frame()
      }
      rv$custom_ts <- dat
      rv$display_ts <- dat
    })  
    
    observeEvent(input$upload, {
      if (file.ext(input$upload$name) %in% c("xls", "xlsx")) {
        dat <- try(read_xts(input$upload$datapath))
        if ("try-error" %in% class(dat)) {
          showModal(
            modalDialog(
              title = "Error",
              "Unable to read time-series. Try downloading template for a 
              usable example.",
              easyClose = TRUE
            )
          )
          return(NULL)
        }
        freq <- guess_freq(dat)
        if (is.na(freq)) {
          showModal(
            modalDialog(
              title = "Error",
              "Cannot read frequency",
              easyClose = TRUE
            )
          )
          return(NULL)
        }
        if (freq %in% c("quarters", "years")) {
          showModal(
            modalDialog(
              title = "Error",
              "Frequency needs to be days, weeks, or months.",
              easyClose = TRUE
            )
          )
          return(NULL)
        }
        if (freq %in% c("days", "weeks")) {
          dat <- change_freq(dat)
        }
        dat <- xts_to_dataframe(dat)
        rv$new_ts <- TRUE
        rv$upload_ts <- dat
        rv$display_ts <- dat
      }
    })
    
    observeEvent(input$exe, {
      if (nrow(rv$upload_ts) == 0) {
        showModal(
          modalDialog(
            title = "Warning",
            "You need to upload a file first.",
            easyClose = TRUE
          )
        )
      }
      if (nrow(rv$custom_ts) == 0) {
        combo <- rv$upload_ts
      } else {
        combo <- try(xts_rbind(new = rv$upload_ts, old = rv$custom_ts,
                               is_xts = FALSE, backfill = TRUE))
        if ("try-error" %in% class(combo)) {
          showModal(
            modalDialog(
              title = "Error",
              "Could not add time-series",
              easyClose = TRUE
            )
          )
          return(NULL)
        }
        end <- ceiling_date(rv$dt_rng[2], "months") - 1
        minus_6_mo <- add_with_rollback(end, months(-6))
        if (zoo::index(combo)[1] > minus_6_mo) {
          showModal(
            modalDialog(
              title = "Warning",
              "Time-series needs at least 6 months of observations.",
              easyClose = TRUE
            )
          )
          return(NULL)
        }
        showModal(
          modalDialog(
            title = "Success",
            "Time-series has been added. You can add to analysis.",
            easyClose = TRUE
          )
        )
        rv$custom_ts <- combo
        arc_df <- xts_to_dataframe(combo)
        arc_df[[1]] <- as.character(arc_df[[1]])
        x <- try(write_parquet(arc_df,
          bucket$path(paste0(input$select_folder, 
                             "/time-series/monthly.parquet"))))
        if ("try-error" %in% class(x)) {
          x <- try(write_parquet(arc_df,
            bucket$path(paste0(input$select_folder, 
                               "/time-series/monthly.parquet"))))
          if ("try-error" %in% class(x)) {
            showModal(
              modalDialog(
                title = "Warning.",
                "There was an error saving your time-series, please try 
                again.",
                easyClose = TRUE
              )
            )
          }
        }
      }
    })
    
    observeEvent(input$add, {
      if (nrow(rv$custom_ts) == 0) {
        showModal(
          modalDialog(
            title = "Warning",
            "There are no returns to uplodad.",
            easyClose = TRUE
          )
        )
      }
      overlap <- colnames(rv$custom_ts) %in% colnames(rv$all_ret)
      if (any(overlap)) {
      }
      new <- rv$custom_ts
      if ("data.frame" %in% class(new)) {
        new <- dataframe_to_xts(new)
      }
      combo <- xts_rbind(new, rv$all_ret, backfill = TRUE)
      rv$all_ret <- combo
      xdf <- data.frame(
        DtcName = colnames(new),
        SecType = "custom time-series"
      )
      ix <- rv$msl$DtcName %in% colnames(new)
      rv$msl <- rob_rbind(rv$msl[!ix, ], xdf)
      showModal(
        modalDialog(
          title = "Success",
          "Returns have been added."
        )
      )
    })
    
    output$tbl_custom_ts <- renderReactable({
      if (nrow(rv$display_ts) > 0) {
        reactable(custom_ts_data(rv))
      }
    })
    
    output$download <- downloadHandler(
      filename = function() {"template.xlsx"},
      content = function(file) {
        x <- xts_to_dataframe(change_freq(r["2015/", 1:3]))
        rownames(x) <- NULL
        writexl::write_xlsx(x, path = file)
      }
    )
    
  })  
}

file.ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

custom_ts_data <- function(rv) {
  r <- dataframe_to_xts(rv$display_ts)
  dt <- ret_date_info(r)
  geo_ret <- rep(NA, ncol(r))
  vol <- geo_ret
  for (i in 1:length(geo_ret)) {
    geo_ret[i] <- calc_geo_ret(na.omit(r[, i]), "months")
    vol[i] <- calc_vol(na.omit(r[, i]), "months")
  }
  xdf <- data.frame(
    Name = colnames(r),
    Mapped = NA,
    Start = dt$Start,
    End = dt$End,
    Return = scales::percent(geo_ret, accuracy = 0.1),
    Volatility = scales::percent(vol, accuracy = 0.1)
  )
  if (rv$new_ts) {
    mapped <- colnames(r) %in% colnames(rv$custom_ts)
    xdf$Mapped <- mapped
  }
  return(xdf)
}