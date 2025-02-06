#' @title Performance Table
#' @param rpt Reporter Object
#' @param freq Frequency of returns
#' @return flextable with formatted performance stats: alpha, beta, sharpe, TE,
#'   and up / down capture
#' @export
create_perf_tbl <- function(rpt, freq = "M") {
  col <- rpt$col
  combo <- rpt$ret_combo(freq)
  combo <- combo[[1]]
  fund <- combo$p
  bench <- combo$b
  rf <- combo$rf
  # the stats in performance table are a subset of the perf_summary output and
  # alpha
  res <- perf_summary(combo$p, combo$rf, combo$b, "months")
  perf_tbl <- data.frame(
    STATISTICS = c("Alpha", "Beta", "Sharpe", "Tracking Error", "Up Capture",
                   "Down Capture"),
    FUND = ""
  )
  rf_ann <- calc_geo_ret(combo$rf, "months")
  xbeta <- res[10, 2]
  b_ann <- res[1, 3]
  f_ann <- res[1, 2]
  perf_tbl[1, 2] <- scales::percent(
    (f_ann - rf_ann) - xbeta * (b_ann - rf_ann), 0.1
  )
  perf_tbl[2, 2] <- scales::number(xbeta, 0.01)
  perf_tbl[3, 2] <- scales::number(res[6, 2], 0.01)
  perf_tbl[4, 2] <- scales::percent(res[9, 2], 0.1)
  perf_tbl[5, 2] <- scales::percent(res[12, 2], 0.1)
  perf_tbl[6, 2] <- scales::percent(res[13, 2], 0.1)
  # output in flextable
  flextable(perf_tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[4], part = "header") |>
    height(0.18, i = 1:nrow(perf_tbl)) |>
    width(j = 1, width = 1.2)
}

#' @title Trailing Periodic Performance
#' @param rpt Reporter Object
#' @param freq Frequency of returns
#' @export
create_trail_perf_tbl <- function(rpt, freq = "M") {
  col <- rpt$col
  combo <- rpt$ret_combo(freq)[[1]]
  fund <- combo$p
  bench <- combo$b
  rf <- combo$rf
  dt <- eom_cal_perf_dt()[-1]
  cr <- matrix(nrow = 2, ncol = length(dt))
  for (i in 1:length(dt)) {
    if (i >= 3) {
      n <- as.numeric(gsub(" Yr", "", names(dt)[i]))
    } else {
      n <- 1
    }
    cr[1, i] <- prod(fund[paste0(dt[i], "/")] + 1)^(1/n)-1
    cr[2, i] <- prod(bench[paste0(dt[i], "/")] + 1)^(1/n)-1
  }
  n_obs <- nrow(fund)
  if (n_obs < 120) {
    cr[, 6] <- NA
  }
  if (n_obs < 60) {
    cr[, 5] <- NA
  }
  if (n_obs < 36) {
    cr[, 4] <- NA
  }
  if (n_obs < 12) {
    cr[, 3] <- NA
  }
  cr <- data.frame(cr)
  colnames(cr) <- names(dt)
  cr$FUND <- c(colnames(fund), colnames(bench))
  cr <- cr[, c(ncol(cr), 1:(ncol(cr)-1))]
  
  yrs <- change_freq(xts_cbind(fund, bench), "years")
  yrs <- xts_to_dataframe(yrs)
  yrs <- yrs[order(yrs$Date, decreasing = TRUE), ]
  yrs <- yrs[1:7, ]
  df_yrs <- t(yrs[, -1])
  colnames(df_yrs) <- year(yrs$Date)
  cr <- cbind(cr, df_yrs)
  tot_row <- data.frame(
    RETURNS = paste0("+ / - ", colnames(bench)),
    cr[1, -1] - cr[2, -1]
  )
  colnames(tot_row) <- colnames(cr)
  cr <- rbind(cr, tot_row)
  cr[, -1] <- apply(cr[, -1], 2, scales::percent, accuracy = 0.1)
  flextable(cr) |> 
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[4], part = "header") |>
    width(1.75, j = 1) |>
    width(0.525, j = 2:ncol(cr)) |>
    height(0.18, i = 1:nrow(cr)) |> 
    vline(j = 7, border = fp_border(color = "grey")) |>
    hline(i = 2, border = fp_border(color = "grey"))
}

#' @title Create Characteristics Table
#' @param rpt Reporter Object
#' @param freq Frequency of returns
#' @export
create_char_tbl <- function(rpt) {
  col <- rpt$col
  char_tbl <- data.frame(
    CHARACTERISTICS = c("Dividend Yield", "TTM P/E Ratio", "TTM P/B Ratio"),
    FUND = ""
  )
  x <- rpt$fina_summ()
  char_tbl[1, 2] <- scales::percent(x[4, 2] / 100, 0.1)
  char_tbl[2, 2] <- scales::number(x[1, 2], 0.1)
  char_tbl[3, 2] <- scales::number(x[2, 2], 0.1)
  flextable(char_tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[4], part = "header") |>
    height(0.18, i = 1:nrow(char_tbl)) |>
    width(j = 1, width = 1.2)
}

#' @export
create_wealth_cht <- function(rpt, freq = "M") {
  col <- rpt$col
  combo <- rpt$ret_combo(freq)[[1]]
  fund <- combo$p
  bench <- combo$b
  viz_wealth_index(xts_cbind(fund, bench)) +
    scale_color_manual(values = col[c(1, 4)]) +
    xlab("") + labs(title = "CUMULATIVE PERFORMANCE") + 
    theme(
      plot.title = element_text(
        family = "La Gioconda TT", 
        color = col[4],
        size = 8
      ),
      legend.position = "bottom",
      text = element_text(
        size = 7,
        family = "Source Sans Pro Light",
        color = "grey40"
      ),
      legend.box.spacing = unit(-10, "pt"),
      panel.background = element_blank(),
      strip.background = element_blank()
    )
}

#' @export
create_capm_cht <- function(rpt, freq = "M", funds = TRUE, 
                            legend_loc = "right") {
  col <- rpt$col
  
  combo <- rpt$ret_combo(freq)[[1]]
  asset <- combo$xp
  bench <- combo$xb
  rf <- combo$xrf
  port <- combo$p
  
  if (funds) {
    plot_ret <- xts_cbind(asset, port)
  } else {
    plot_ret <- port
  }
  plot_ret <- xts_cbind(plot_ret, bench)
  plot_ret <- na.omit(plot_ret)
  plot_y <- calc_geo_ret(plot_ret, "months")
  plot_x <- calc_vol(plot_ret, "months")
  plot_dat <- data.frame(
    x = plot_x,
    y = plot_y,
    name = colnames(plot_ret)
  )
  plot_dat$Bench <- plot_dat$name %in% colnames(bench)
  plot_dat$shape <- ifelse(plot_dat$Bench, 17, 16)
  plot_dat$name <- factor(plot_dat$name, unique(plot_dat$name))
  ggplot(plot_dat, aes(x = x, y = y, col = name)) +
    geom_point(size = 3, shape =plot_dat$shape) +
    geom_vline(xintercept = plot_dat$x[plot_dat$Bench], color = "grey") + 
    geom_hline(yintercept = plot_dat$y[plot_dat$Bench], color = "grey") +
    scale_color_manual(values = col) +
    guides(shape = element_blank()) +
    labs(col = "", title = "RISK & RETURN") +
    xlab("Annualized Risk") + ylab("Annualized Return") + 
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      text = element_text(
        size = 7, 
        family = "Source Sans Pro Light",
        color = "grey40"
      ),
      plot.title = element_text(
        family = "La Gioconda TT", 
        color = col[4], 
        size = 8
      ),
      legend.position = legend_loc
    )
}


create_fund_capm_chart <- function(rpt, freq = "M", legend_loc = "right") {
  col <- rpt$col
  
  combo <- rpt$ret_combo(freq)[[1]]
  asset <- combo$xp
  bench <- combo$xb
  rf <- combo$xrf
  port <- combo$p
  
  plot_ret <- xts_cbind(combo$x, combo$b)
  plot_ret <- na.omit(plot_ret)
  plot_y <- calc_geo_ret(plot_ret, "months")
  plot_x <- calc_vol(plot_ret, "months")
  plot_dat <- data.frame(
    x = plot_x,
    y = plot_y,
    name = colnames(plot_ret)
  )
  plot_dat$Bench <- plot_dat$name %in% colnames(combo$b)
  plot_dat$shape <- ifelse(plot_dat$Bench, 17, 16)
  plot_dat$name <- factor(plot_dat$name, unique(plot_dat$name))
  capm_cht <- ggplot(plot_dat, aes(x = x, y = y, col = name)) +
    geom_point(size = 3, shape =plot_dat$shape) +
    geom_vline(xintercept = plot_dat$x[plot_dat$Bench], color = "grey") + 
    geom_hline(yintercept = plot_dat$y[plot_dat$Bench], color = "grey") +
    scale_color_manual(values = col) +
    guides(shape = element_blank()) +
    labs(col = "", title = "RISK & RETURN") +
    xlab("Annualized Risk") + ylab("Annualized Return") + 
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      text = element_text(
        size = 7, 
        family = "Source Sans Pro Light",
        color = "grey40"
      ),
      plot.title = element_text(
        family = "La Gioconda TT", 
        color = col[4], 
        size = 8
      ),
      legend.position = legend_loc
    )
}

#' @export
create_sector_cht <- function(rpt) {
  col <- rpt$col
  dat <- rpt$sector_summ("GicsMacro")
  sect <- pivot_longer(dat, -GicsMacro, names_to = "name", values_to = "value")
  colnames(sect)[1] <- "Sector"
  sect$Sector <- factor(sect$Sector, unique(sect$Sector))
  abbr <- data.frame(
    Sector = sort(unique(sect$Sector)),
    Abbr = c("Comm Serv.", "Cons Disc.", "Cons Stap.", "Energy", "Fina.", 
             "Health", "Indust.", "Tech", "Materials", "Real Estate", "Util.")
  )
  sect$lbl <- scales::percent(sect$value, 0.1)
  sect <- left_join(sect, abbr, by = "Sector")
  ggplot(sect, aes(y = value, x = Abbr, fill = name, label = lbl)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(
      aes(y = value + 0.025), 
      size = 1.75, 
      position = position_dodge(0.9),
      color = "grey40") +
    scale_fill_manual(values = col[c(4, 1)]) +
    xlab("") + ylab("") + labs(title = "SECTOR", fill = "") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(
            size = 6, 
            family = "Source Sans Pro Light", 
            color = "grey40"
          ),
          plot.title = element_text(
            family = "La Gioconda TT", 
            color = col[4], 
            size = 8
          ),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_text(
            size = 5, 
            family = "Source Sans Pro Light",
            color = "grey40"
          ),
          legend.key.size = unit(0.2, "in"),
          legend.text = element_text(size = 6, family = "Source Sans Pro Light"),
          legend.box.spacing = unit(-10, "pt"))
}

#' @export
create_country_cht <- function(rpt) {
  col <- rpt$col
  dat <- rpt$country_summ(rgn = TRUE)
  plotdat <- tidyr::pivot_longer(dat[, -1], -Region)
  plotdat$lbl <- scales::percent(plotdat$value, 0.1)
  ggplot(plotdat, aes(y = value, x = Region, fill = name, label = lbl)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(
      aes(y = value + 0.025), 
      size = 1.75, 
      position = position_dodge(0.9),
      color = "grey40") +
    scale_fill_manual(values = col[c(4, 1)]) +
    xlab("") + ylab("") + labs(title = "SECTOR", fill = "") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(
            size = 6, 
            family = "Source Sans Pro Light", 
            color = "grey40"
          ),
          plot.title = element_text(
            family = "La Gioconda TT", 
            color = col[4], 
            size = 8
          ),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_text(
            size = 5, 
            family = "Source Sans Pro Light",
            color = "grey40"
          ),
          legend.key.size = unit(0.2, "in"),
          legend.text = element_text(size = 6, family = "Source Sans Pro Light"),
          legend.box.spacing = unit(-10, "pt"))
}

#' @export
create_alloc_tbl <- function(dict) {
  wgt <- dict[dict$DataType == "Allocation", c("Field", "Value")]  
  style <- dict[dict$DataType == "Style", c("Field", "Value")]
  tbl <- left_join(wgt, style, by = "Field")
  colnames(tbl) <- c("ALLOCATION", "Target Weight", "Style")
  tbl <- tbl[, c(1, 3, 2)]
  tbl$`Target Weight` <- percent(as.numeric(tbl$`Target Weight`) / 100, 0.1)
  flextable(tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[4], part = "header") |>
    height(0.025, i = 1:nrow(tbl)) |>
    line_spacing(space = 0.5) |>
    line_spacing(space = 0.75, i = 1) |>
    hrule(rule = "exact") |>
    width(j = 1, width = 1.75) |>
    width(j = 2:3, width = 1.25)
}

#' @export
create_descr_tbl <- function(descr) {
  tbl <- data.frame(DESCRIPTION = descr$Description)
  flextable(tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[4], part = "header") |>
    width(j = 1, width = 4.25) |>
    border_remove()
}
