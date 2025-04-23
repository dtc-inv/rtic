pm_user_input <- function(id) {
  tagList(
    fluidRow(
      column(5,
        div(
          selectizeInput(
            inputId = NS(id, "select_folder"),
            label = "Select PM Folder",
            choices = NULL,
            multiple = FALSE
          )
        ),
        div(
          selectInput(
            inputId = NS(id, "select_port"),
            label = "Select Portfolio",
            choices = NULL,
            multiple = TRUE,
            selectize = FALSE,
            size = 20
          )
        )
      ),
      column(5,
        actionButton(
          inputId = NS(id, "add_port"),
          label = "Add to Investment Options",
          class = "btn btn-success"
        ),
        br(),
        br(),
        actionButton(
          inputId = NS(id, "delete_port"),
          label = "Delete",
          icon = icon("skull-crossbones", lib = "font-awesome"),
          class = "btn btn-danger"
        )
      )
    )
  )
}

pm_server <- function(id, ra, rv) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(
      session,
      "select_folder",
      choices = ra$list_libraries(),
      selected = "asotolongo"
    )
    observeEvent(input$select_folder, {
      if (input$select_folder != "") {
        xlib <- ra$get_library(input$select_folder)
        port <- try(xlib$read("portfolios"))
        if ("try-error" %in% class(port)) {
          opt <- ""
        } else {
          opt <- unique(port$data$CustPort)
        }
        if (length(opt) == 0) {
          opt <- ""
        }
        updateSelectInput(
          session,
          "select_port",
          choices = opt
        )
      }
    })
    observeEvent(input$delete_port, {
      if (input$select_port != "") {
        xlib <- ra$get_library(input$select_folder)
        port <- xlib$read("portfolios")$data
        ix <- port$CustPort %in% input$select_port
        port <- port[!ix, ]
        xlib$write("portfolios", port)
        opt <- unique(port$data$CustPort)
        if (length(opt) == 0) {
          opt <- ""
        }
        updateSelectInput(
          session,
          "select_port",
          choices = unique(port$data$CustPort)
        )
      }
    })
    observeEvent(input$add_port, {
      if (!is.null(input$select_port)) {
        xdf <- data.frame(
          DtcName = input$select_port,
          SecType = "pm-model")
        rv$msl <- rob_rbind(rv$msl, xdf)
        rv$msl$DtcName <- make.unique(rv$msl$DtcName)
        for (i in 1:length(input$select_port)) {
          xlib <- ra$get_library(input$select_folder)
          port <- xlib$read("portfolios")$data
          ix <- port$CustPort %in% input$select_port[i]
          port <- port[ix, ]
          asset_ret <- na.omit(rv$all_ret[, port$Asset])
          wgt_mat <- matrix(
            port$CapWgt,
            ncol = length(port$CapWgt),
            nrow = nrow(asset_ret), byrow = TRUE)
          port_ret <- rowSums(asset_ret * wgt_mat)
          port_ret <- xts(port_ret, zoo::index(asset_ret))
          colnames(port_ret) <- input$select_port[i]
          r <- xts_rbind(rv$all_ret, port_ret)
          colnames(r) <- make.unique(colnames(r))
          rv$all_ret <- r
        }
        showModal(
          modalDialog(
            title = "Success!",
            "You can now navigate back to analysis.",
            easyClose = TRUE
          )
        )
      }
    })
  })
}

pm_edit_ui <- function(id) {
  tagList(
    div(
      fluidRow(
        column(
          width = 3,
          selectizeInput(
            inputId = NS(id, "select_folder"),
            label = "Open PM Folder",
            choices = NULL,
            multiple = FALSE
          )
        ),
        column(
          width = 3,
          selectizeInput(
            inputId = NS(id, "select_port"),
            label = "Open Portfolio",
            choices = NULL,
            multiple = FALSE
          )
        ),
        column(
          width = 3,
          br(),
          div(
            style = "display:inline-block",
            actionButton(
              inputId = NS(id, "open_port"),
              label = "Open",
              class = "btn btn-primary"
            )
          ),
          div(
            style = "display:inline-block; margin-left:10px",
            actionButton(
              inputId = NS(id, "clear_port"),
              label = "Clear"
            )
          )
        )
      )
    ),
    div(
      fluidRow(
        column(
          width = 3,
          textInput(
            inputId = NS(id, "save_name"),
            label = "Portfolio Name"
          )
        ),
        column(
          width = 3,
          br(),
          actionButton(
            inputId = NS(id, "save"),
            label = "Save Portfolio",
            class = "btn btn-success"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        selectizeInput(
          inputId = NS(id, "select_asset"),
          label = "Select Assets",
          choices = NULL,
          multiple = TRUE)       
      ),
      column(
        width = 3,
        br(),
        actionButton(
          inputId = NS(id, "add_asset"),
          label = "Add Assets"
        )
      ),
      column(
        width = 3,
        radioButtons(
          inputId = NS(id, "sum_to_1"),
          label = "Force Weights to Equal 100%",
          choices = c(TRUE, FALSE),
          selected = TRUE,
          inline = TRUE))
    ),
    div(
      rHandsontableOutput(NS(id, "table"))
    )
  )  
}

pm_edit_server <- function(id, ra, rv) {
  moduleServer(id, function(input, output, session) {
    pm_opts <- ra$list_libraries()
    updateSelectizeInput(
      session,
      "select_folder",
      choices = pm_opts,
      selected = NULL
    )
    # select folder ----
    observeEvent(input$select_folder, {
      if (input$select_folder != "") {
        xlib <- ra$get_library(input$select_folder)
        port <- try(xlib$read("portfolios"))
        if ("try-error" %in% class(port)) {
          opt <- ""
        } else {
          opt <- unique(port$data$CustPort)
        }
        if (length(opt) == 0) {
          opt <- ""
        }
        updateSelectizeInput(
          session,
          "select_port",
          choices = opt,
          selected = ""
        )
      }
    })
    # open port ----
    observeEvent(input$open_port, {
      if (input$select_folder != "") {
        xlib <- ra$get_library(input$select_folder)
        port <- try(xlib$read("portfolios")$data)
        ix <- port$CustPort %in% input$select_port
        port_out <- port[ix, 2:3]
        port_out <- rbind(
          port_out, 
          data.frame(Asset = rep(NA, 10), CapWgt = rep(NA, 10))
        )
        rv$port <- port_out
        rv$old_port <- port
      } else {
        showModal(
          modalDialog(
            title = "Warning",
            "Select a portfolio before opening."
          )
        )
      }
    })
    # clear port ----
    observeEvent(input$clear_port, {
      rv$port <- data.frame(Asset = rep(NA, 10), CapWgt = rep(NA, 10))
    })
    # save port ----
    observeEvent(input$save, {
      if (input$save_name == "") {
        showModal(
          modalDialog(
            title = "Warning",
            "Please enter a portfolio name before saving.",
            easyClose = TRUE
          )
        )
        return()
      }
      port <- hot_to_r(input$table)
      port <- na.omit(port)
      port$CustPort <- input$save_name
      if (nrow(port) == 0) {
        showModal(
          modalDialog(
            title = "Warning", 
            "Portfolio did not have any valid rows. Missing values detected.",
            easyClose = TRUE
          )
        )
        return()
      } else {
        if (input$sum_to_1) {
          port$CapWgt <- port$CapWgt / sum(port$CapWgt)
        }
        ix <- rv$old_port$CustPort %in% input$save_name
        if (any(ix)) {
          rv$old_port <- rv$old_port[!ix, ]
        }
        port <- rbind(rv$old_port, port)
        xlib <- ra$get_library(input$select_folder)
        xlib$write("portfolios", port)
        showModal(
          modalDialog(
            title = "Success!",
            "Portfolio has been saved. You can now navigate to the Manage Tab
             to add it to the analysis.",
            easyClose = TRUE
          )
        )
      }
    })
    # update select asset ----
    observe({
      updateSelectizeInput(
        session,
        "select_asset",
        selected = NULL,
        choices = create_asset_choices(rv$msl),
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
      rv$port <- hot_to_r(input$table)
      new_funds <- data.frame(Asset = input$select_asset, CapWgt = 0)
      rv$port <- rbind(new_funds, rv$port)
      updateSelectizeInput(
        session,
        "select_asset",
        selected = NULL,
        choices = create_asset_choices(rv$msl),
        server = TRUE
      )
    })
    # render table ----
    output$table <- renderRHandsontable({
      rhandsontable(rv$port) |>
        hot_col(col = "Asset", type = "dropdown", source = rv$msl$DtcName,
                strict = TRUE) |>
        hot_col(col = "CapWgt", type = "numeric", format = "0.0000", 
                strict = TRUE) |>
        hot_cols(colWidths = c(350, 100))
    })
  })
}

rob_rbind <- function(df1, df2) {
  if (nrow(df1) == 0) {
    return(df2)
  }
  if (nrow(df2) == 0) {
    return(df1)
  }
  nm_union <- unique(c(colnames(df1), colnames(df2)))
  df1_miss <- !nm_union %in% colnames(df1)
  df2_miss <- !nm_union %in% colnames(df2)
  df1[, nm_union[df1_miss]] <- NA
  df2[, nm_union[df2_miss]] <- NA
  df2 <- df2[, colnames(df1)]
  rbind(df1, df2)
}
