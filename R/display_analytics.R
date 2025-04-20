# dates ----
da_date_range_ui <- function(id) {
  span(textOutput(NS(id, "text")), style = "color:grey; margin-bottom:5px")
}

da_date_range_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$text <- renderText({
      start <- format(rv$dt_rng[1], "%b %d, %Y")
      end <- format(rv$dt_rng[2], "%b %d, %Y")
      paste0(start, " to ", end)
    })
  })
}

# charts ----
plotly_ui <- function(id) {
  plotlyOutput(NS(id, "plot"))
}

da_plot_server <- function(id, cht) {
  moduleServer(id, function(input, output, sessionn) {
    output$plot <- renderPlotly({
      ggplotly(cht)
    })
  })
}

# tables ----
da_perf_stats_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- renderReactable({
      reactable(
        data = rv$ra$stats$cht,
        columns = list(
          Metric = colDef(minWidth = 140)
        ),
        pagination = FALSE
      )
    })
  })
}

da_trail_perf_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- renderReactable({
      reactable(
        data = rv$ra$trail_perf$cht,
        pagination = FALSE
      )
    })
  })
}

# charts ----
da_plotly_server <- function(id, rv, cht_type) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      if (cht_type == "ret") {
        ggplotly(rv$ra$roll_ret$cht)
      } else if (cht_type == "act_ret") {
        ggplotly(rv$ra$roll_act_ret$cht)
      } else if (cht_type == "vol") {
        ggplotly(rv$ra$roll_vol$cht)
      } else if (cht_type == "roll_sharpe") {
        ggplotly(rv$ra$roll_sharpe$cht)
      } else if (cht_type == "roll_te") {
        ggplotly(rv$ra$roll_te$cht)
      } else if (cht_type == "roll_beta") {
        ggplotly(rv$ra$roll_beta$cht)
      } else if (cht_type == "roll_corr") {
        ggplotly(rv$ra$roll_corr$cht)
      } else if (cht_type == "wealth") {
        ggplotly(rv$ra$wealth$cht)
      } else if (cht_type == "capm") {
        ggplotly(rv$ra$capm$cht)
      } else if (cht_type == "worst_dd") {
        ggplotly(rv$ra$worst_dd$cht)
      } else if (cht_type == "bench_dd") {
        ggplotly(rv$ra$bench_dd$cht)
      } else if (cht_type == "all_dd") {
        ggplotly(rv$ra$all_dd$cht)
      }
    })
  })
}

