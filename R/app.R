library(shiny)
library(bslib)
library(rtic)
library(shinythemes)
library(lubridate)
library(arrow)
library(reactable)
library(ggplot2)
library(plotly)
library(officer)
library(flextable)
library(rhandsontable)
library(tidyr)
library(readxl)
library(reticulate)
source("setup.R")
source("return_analytics.R")
source("display_analytics.R")
source("export.R")
source("port_mgmt.R")
source("ts_mgmt.R")
source("bucket-util.R")
load("api_keys.RData")

if (Sys.info()[["user"]] == "asotolongo") {
  use_python("C:/Users/asotolongo/AppData/Local/anaconda3/")  
}
adb <- try(import("arcticdb"))
if ("try-error" %in% class(adb)) {
  py_install("arcticdb")
  adb <- import("arcticdb")
}
base_url <- 's3://s3.us-east-1.amazonaws.com:dtc-rtic?'
s3_url <- paste0(
  base_url,
  "access=", api_keys$s3$access_key,
  "&secret=", api_keys$s3$secret_key
)
ac <- adb$Arctic(s3_url)
ret_lib <- ac$get_library("returns")
s3_url <- paste0(
  "s3://s3.us-east-1.amazonaws.com:return-analytics?",
  "access=", api_keys$s3$access_key,
  "&secret=", api_keys$s3$secret_key
)
ra <- adb$Arctic(s3_url)

met_lib <- ac$get_library("meta-tables")
msl <- met_lib$read("msl")$data


ui <- navbarPage(
  title = "Return Analytics",
  theme = shinytheme("flatly"),
  tabPanel(
    title = "Analysis",
    navlistPanel(
      "Navigate",
      widths = c(3, 9),
      tabPanel(
        "Setup",
        setup_asset_select_input("setup_asset_sel"),
        reactable_ui("tbl_asset")
      ),
      tabPanel(
        "Performance Statistics",
        da_date_range_ui("stats_date"),
        reactable_ui("stats")
      ),
      tabPanel(
        "Trailing Performance",
        da_date_range_ui("trail_perf_date"),
        reactable_ui("trail_perf")
      ),
      tabPanel(
        "Wealth Chart",
        da_date_range_ui("wealth_date"),
        br(),
        plotly_ui("wealth")
      ),
      tabPanel(
        "Risk & Return Chart",
        da_date_range_ui("capm_date"),
        br(),
        plotly_ui("capm")
      ),
      tabPanel(
        "Drawdowns",
        da_date_range_ui("drawdowns"),
        br(),
        plotly_ui("worst_dd"),
        plotly_ui("bench_dd"),
        plotly_ui("all_dd")
      ),
      tabPanel(
        "Rolling Stats",
        plotly_ui("roll_ret"),
        plotly_ui("roll_act_ret"),
        plotly_ui("roll_vol"),
        plotly_ui("roll_sharpe"),
        plotly_ui("roll_te"),
        plotly_ui("roll_beta"),
        plotly_ui("roll_corr")
      )
    )
  ), # end Analysis
  tabPanel(
    "Export",
    download_ui("out_xl", "Download Excel"),
    br(),
    br(),
    download_ui("out_pptx", "Download Powerpoint")
  ),
  tabPanel(
    "Port Mgmt",
    navlistPanel(
      "Options",
      widths = c(3, 9),
      tabPanel(
        "Manage",
        pm_user_input("pm_sel")
      ),
      tabPanel(
        "Create / Edit",
        pm_edit_ui("pm_edit")
      )
    )
  ),
  tabPanel(
    "Time-Series Mgmt",
    ts_user_input("ts_mgmt")
  ),
  tabPanel(
    "Help!",
    download_ui("out_intro", "Quick Start Intro Video"),
    download_ui("out_port_mgmt", "Port Mgmt Video"),
    download_ui("out_time_series", "Time-Series Mgmt Video"),
    br(),
    br(),
    uiOutput("userguide")
  )
)

server <- function(input, output, session) {
  rv <- setup_reactive_vals()
  rv$msl <- msl
  
  # setup ----
  rv <- setup_asset_select_server("setup_asset_sel", rv, ac)
  setup_tbl_asset_server("tbl_asset", rv)
  
  # custom ret / port ----
  pm_server("pm_sel", ra, rv)
  pm_edit_server("pm_edit", ra, rv)
  
  # date ranges ----
  da_date_range_server("stats_date", rv)
  da_date_range_server("trail_perf_date", rv)
  da_date_range_server("wealth_date", rv)
  da_date_range_server("capm_date", rv)
  da_date_range_server("drawdowns", rv)
  
  # charts ----
  da_plotly_server("wealth", rv, "wealth")
  da_plotly_server("capm", rv, "capm")
  da_plotly_server("worst_dd", rv, "worst_dd")
  da_plotly_server("bench_dd", rv, "bench_dd")
  da_plotly_server("all_dd", rv, "all_dd")
  # roll
  da_plotly_server("roll_ret", rv, "ret")
  da_plotly_server("roll_act_ret", rv, "act_ret")
  da_plotly_server("roll_vol", rv, "vol")
  da_plotly_server("roll_sharpe", rv, "roll_sharpe")
  da_plotly_server("roll_te", rv, "roll_te")
  da_plotly_server("roll_beta", rv, "roll_beta")
  da_plotly_server("roll_corr", rv, "roll_corr")
  
  # tables ---
  da_perf_stats_server("stats", rv)
  da_trail_perf_server("trail_perf", rv)
  
  # export ----
  download_excel_server("out_xl", rv)
  download_pptx_server("out_pptx", rv)
  
  # help ----
  download_help_port_mgmt("out_port_mgmt")
  download_help_time_series("out_time_series")
  download_help_intro("out_intro")
  output$userguide <- renderUI({
    tags$iframe(style = "height:11in; width:100%", 
                src = "user-guide.html")
  })
  
  # time-series ----
  ts_server("ts_mgmt", api_keys, rv)
}

shinyApp(ui, server)

