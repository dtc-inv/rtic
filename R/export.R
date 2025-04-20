

download_ui <- function(id, lbl) {
  downloadButton(NS(id, "download"), lbl)
}

download_help_port_mgmt <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function(){"Port Mgmt.mp4"},
      content = function(file) {
        file.copy("help-port-mgmt.mp4", file)
      }
    )
  })
}

download_help_time_series <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function(){"Time-Series.mp4"},
      content = function(file) {
        file.copy("help-time-series.mp4", file)
      }
    )
  })
}

download_help_intro <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function(){"Intro.mp4"},
      content = function(file) {
        file.copy("help-intro.mp4", file)
      }
    )
  })
}


download_excel_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() {paste0(Sys.Date(), ".xlsx")},
      content = function(file) {
        writexl::write_xlsx(rv$xl_out, path = file)
      }
    )
  })
}

download_pptx_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() {paste0(Sys.Date(), ".pptx")},
      content = function(file) {
        withProgress(message = "Exporting PowerPoint", {
          col <- set_plot_col(10)
          theme_set(
            theme(
              plot.title = element_text(
                family = "Source Sans Pro", 
                color = col[2],
                size = 8
              ),
              legend.position = "bottom",
              text = element_text(
                size = 7,
                family = "Source Sans Pro Light",
                color = "grey40"
              ),
              legend.key.size = unit(0.2, "cm"),
              panel.background = element_blank(),
              strip.background = element_blank()))
          set_flextable_defaults(font.size = 9, 
                                 font.family = "Sans Source Pro", padding = 0)
          pres <- read_pptx("template.pptx") |>
            # stats ----
            add_slide("ra_slide", master = "DTC-Theme-2021") |>
            ph_with("Performance Stats", 
                    ph_location_label("Text Placeholder 18")) |>
            ph_with(fmt_dt(rv$dt_rng), 
                    ph_location_label("Text Placeholder 3")) |>
            ph_with(
              flextable(rv$ra$stats$ts$a) |>
                width(j = 1, width = 1.75) |>
                width(j = 2:ncol(rv$ra$stats$ts$a), width = 0.75) |>
                height(i = 1:nrow(rv$ra$stats$ts$a), height = 0.175) |>
                bg(bg = col[2], part = "header"),
              location = ph_location(top = 1.28, left = 0.4)) |>
            ph_with(
              flextable(rv$ra$stats$ts$b) |>
                width(j = 1, width = 1.75) |>
                width(j = 2:ncol(rv$ra$stats$ts$b), width = 0.75) |>
                height(i = 1:nrow(rv$ra$stats$ts$a), height = 0.175) |>
                bg(bg = col[2], part = "header"),,
              location = ph_location(top = 4, left = 1.34)) |>
            # trail ----
            add_slide("ra_slide", master = "DTC-Theme-2021") |>
              ph_with("Trailing Performance", 
                      ph_location_label("Text Placeholder 18")) |>
            ph_with(fmt_dt(rv$dt_rng), 
                    ph_location_label("Text Placeholder 3")) |>
            ph_with(
              flextable(rv$ra$trail_perf$cht) |>
                height(i = 1:nrow(rv$ra$trail_perf$cht), height = 0.175) |>
                bg(bg = col[2], part = "header"),
              location = ph_location(top = 1.15, left = 2)) |>
            # wealth and capm
            add_slide("ra_slide", master = "DTC-Theme-2021") |>
            ph_with("Risk and Return", 
                    ph_location_label("Text Placeholder 18")) |>
            ph_with(fmt_dt(rv$dt_rng), 
                    ph_location_label("Text Placeholder 3")) |>
            ph_with(
              rv$ra$wealth$cht,
                location = ph_location(left = 0.34, top = 1.95, 
                                       height = 3.6, width = 4.5)) |>
            ph_with(
              rv$ra$capm$cht ,
              location = ph_location(left = 5.09, top = 1.95,
                                     height = 3.6, width = 4.5)) |>
            # drawdowns
            add_slide("ra_slide", master = "DTC-Theme-2021") |> 
            ph_with("Drawdowns",
                    ph_location_label("Text Placeholder 18")) |>
            ph_with(fmt_dt(rv$dt_rng), 
                    ph_location_label("Text Placeholder 3")) |>
            ph_with(
              rv$ra$worst_dd$cht,
              ph_location(left = 0.4, top = 1)) |>
            ph_with(
              rv$ra$bench_dd$cht,
              ph_location(left = 5.15, top = 1)) |>
            ph_with(
              rv$ra$all_dd$cht,
              ph_location(left = 2, top = 4.1, width = 6)
            ) |>
            # roll
            add_slide("ra_slide", master = "DTC-Theme-2021") |>
            ph_with("Rolling Stats",
                    ph_location_label("Text Placeholder 18")) |>
            ph_with(fmt_dt(rv$dt_rng), 
                    ph_location_label("Text Placeholder 3")) |>
            ph_with(rv$ra$roll_ret$cht, ph_location(left = 0.4, top = 1)) |>
            ph_with(rv$ra$roll_act_ret$cht, 
                    ph_location(left = 5.15, top = 1)) |>
            ph_with(rv$ra$roll_vol$cht, ph_location(left = 0.4, top = 4.1)) |>
            ph_with(rv$ra$roll_sharpe$cht, 
                    ph_location(left = 5.15, top = 4.1)) |>
            add_slide("ra_slide", master = "DTC-Theme-2021") |>
            ph_with("Rolling Stats",
                    ph_location_label("Text Placeholder 18")) |>
            ph_with(fmt_dt(rv$dt_rng), 
                    ph_location_label("Text Placeholder 3")) |>
            ph_with(rv$ra$roll_corr$cht, ph_location(left = 0.4, top = 1)) |>
            ph_with(rv$ra$roll_beta$cht, ph_location(left = 5.15, top = 1)) |>
            ph_with(rv$ra$roll_te$cht, ph_location(left = 0.4, top = 4.1))
          
          
          # write out pres
          print(pres, file)
        })
      }
    )
  })
}

fmt_dt <- function(x) {
  start <- format(x[1], "%b %Y")
  end <- format(x[2], "%b %Y")
  paste0(start, " to ", end)
}