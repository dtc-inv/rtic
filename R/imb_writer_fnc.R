#' @title Performance Table
#' @param rpt Reporter Object
#' @param tm10 t minus 10 years date to truncate return start
#' @param freq Frequency of returns
#' @return flextable with formatted performance stats: alpha, beta, sharpe, TE,
#'   and up / down capture
#' @export
create_perf_tbl <- function(rpt, tm10, freq = "months") {
  col <- rpt$col
  combo <- rpt$ret_combo(freq = freq, date_start = tm10)
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
    color(i = 1, color = col[1], part = "header") |>
    color(part = "body", color = "black") |>
    height(0.18, i = 1:nrow(perf_tbl)) |>
    width(j = 1, width = 1.2)
}

#' @title Trailing Periodic Performance
#' @param rpt Reporter Object
#' @param freq Frequency of returns
#' @export
create_trail_perf_tbl <- function(rpt, freq = "months", p_or = NULL, 
                                  b_or = NULL) {
  dt <- eom_cal_perf_dt()[-1]
  col <- rpt$col
  combo <- rpt$ret_combo(freq = freq, date_end = dt[1])[[1]]
  fund <- combo$p
  bench <- combo$b
  rf <- combo$rf
  if (!is.null(p_or)) {
    fund <- cut_time(p_or, zoo::index(fund)[1], zoo::index(fund)[nrow(fund)])
  }
  if (!is.null(b_or)) {
    bench <- cut_time(b_or, zoo::index(bench)[1], 
                      zoo::index(bench)[nrow(bench)])
  }
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
    color(i = 1, color = col[1], part = "header") |>
    color(part = "body", color = "black") |>
    width(1.75, j = 1) |>
    width(0.525, j = 2:ncol(cr)) |>
    height(0.18, i = 1:nrow(cr)) |> 
    vline(j = 7, border = fp_border(color = "grey")) |>
    hline(i = 2, border = fp_border(color = "grey"))
}

#' @title Create Characteristics Table
#' @param rpt Reporter Object
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
    color(i = 1, color = col[1], part = "header") |>
    height(0.18, i = 1:nrow(char_tbl)) |>
    width(j = 1, width = 1.2)
}

#' @title Create Cumulative Return Chart
#' @param rpt Reporter Object
#' @param tm10 t minus 10 years date to truncate return start
#' @param freq return frequency
#' @param p_or portfolio return override
#' @param b_or benchmark return override
#' @export
create_wealth_cht <- function(rpt, tm10, freq = "months", p_or = NULL, 
                              b_or = NULL) {
  col <- rpt$col
  combo <- rpt$ret_combo(freq = freq, date_start = tm10)[[1]]
  fund <- combo$p
  bench <- combo$b
  dat <- xts_cbind(fund, bench)
  if (!is.null(p_or)) {
    fund <- p_or
  }
  if (!is.null(b_or)) {
    bench <- b_or
  }
  if (!is.null(p_or) | !is.null(b_or)) {
    dat <- xts_cbind_inter(fund, bench)
    dat <- dat$ret
  }
  viz_wealth_index(dat) +
    scale_color_manual(values = col[c(1, 4)]) +
    xlab("") + labs(title = "CUMULATIVE PERFORMANCE") + 
    guides(color = guide_legend(nrow = 2, keyheight = 5, 
                                default.unit = "pt")) +
    theme(
      plot.title = element_text(
        family = "La Gioconda TT", 
        color = col[1],
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
create_capm_cht <- function(rpt, tm10, freq = "months", funds = TRUE,
                            adj_scale = TRUE, legend_loc = "right") {
  col <- rpt$col
  combo <- rpt$ret_combo(freq = freq, date_start = tm10)[[1]]
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
  res <- ggplot(plot_dat, aes(x = x, y = y, col = name)) +
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
        color = col[1], 
        size = 8
      ),
      legend.position = legend_loc
    )
  if (adj_scale) {
    x_max <- max(plot_dat$x + 0.01, 0.04)
    x_min <- 0
    y_max <- max(plot_dat$y)
    if (y_max < 0) {
      y_min <- min(y_max - 0.01, -0.04)
      y_max <- -y_min
    } else {
      y_max <- y_max + 0.01
      y_min <- -0.02
    }
    res <- res + 
      scale_x_continuous(limits = c(x_min, x_max), labels = scales::percent) +
      scale_y_continuous(limits = c(y_min, y_max), labels = scales::percent)
  }
  if (legend_loc == "bottom") {
    res <- res +
      guides(color = guide_legend(nrow = 2, keyheight = 5, 
                                  default.unit = "pt"))
  }
  return(res)
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
            color = col[1], 
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
          legend.text = element_text(size = 6, 
                                     family = "Source Sans Pro Light"),
          legend.box.spacing = unit(-10, "pt"))
}

#' @export
create_country_cht <- function(rpt, lgnd_pos = "bottom") {
  col <- rpt$col
  dat <- rpt$country_summ(rgn = TRUE)
  rgn <- group_by(dat[, -1], Region) |>
    summarize_all(sum, na.rm = TRUE)
  rgn[, 2:ncol(rgn)] <- apply(
    rgn[, 2:ncol(rgn)], 
    2, 
    function(x) {x / sum(x, na.rm = TRUE)})
  plotdat <- tidyr::pivot_longer(rgn, -Region)
  plotdat$lbl <- scales::percent(plotdat$value, 0.1)
  ggplot(plotdat, aes(y = value, x = Region, fill = name, label = lbl)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(
      aes(y = value + 0.025), 
      size = 1.75, 
      position = position_dodge(0.9),
      color = "grey40") +
    scale_fill_manual(values = col[c(4, 1)]) +
    xlab("") + ylab("") + labs(title = "REGION", fill = "") +
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
            color = col[1], 
            size = 8
          ),
          legend.position = lgnd_pos,
          axis.text.y = element_blank(),
          axis.text.x = element_text(
            size = 5, 
            family = "Source Sans Pro Light",
            color = "grey40"
          ),
          legend.key.size = unit(0.2, "in"),
          legend.text = element_text(size = 6, 
                                     = "Source Sans Pro Light"),
          legend.box.spacing = unit(-10, "pt"))
}

#' @export
create_alloc_tbl <- function(dict, col) {
  wgt <- dict[dict$DataType == "Allocation", c("Field", "Value")]  
  style <- dict[dict$DataType == "Style", c("Field", "Value")]
  if (nrow(style) > 0) {
    tbl <- left_join(wgt, style, by = "Field")
    colnames(tbl) <- c("ALLOCATION", "Target Weight", "Style")
    tbl <- tbl[, c(1, 3, 2)]
  } else {
    tbl <- wgt
    colnames(tbl) <- c("ALLOCATION", "Target Weight")
  }
  tbl$`Target Weight` <- scales::percent(as.numeric(tbl$`Target Weight`) / 100, 
                                         0.1)
  flextable(tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[1], part = "header") |>
    color(part = "body", color = "black") |>
    height(0.025, i = 1:nrow(tbl)) |>
    line_spacing(space = 0.5) |>
    line_spacing(space = 0.75, i = 1) |>
    hrule(rule = "exact") |>
    width(j = 1, width = 1.75) |>
    width(j = 2:ncol(tbl), width = 1.25)
}

#' @export
create_descr_tbl <- function(descr, col) {
  tbl <- data.frame(DESCRIPTION = descr$Description)
  flextable(tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[1], part = "header") |>
    color(part = "body", color = "black") |>
    width(j = 1, width = 4.25) |>
    border_remove()
}

#' @export
create_bar_wgt_cht <- function(dict, col, lgnd_pos = "right") {

  dict <- filter(dict, DataType == "Allocation")
  dict$Field <- factor(dict$Field, unique(dict$Field))
  dict$Value <- as.numeric(dict$Value)
  x <- dict
  x <- x[order(x$Field, decreasing = TRUE), ]
  dict$LblPos <- cumsum(x$Value)
  dict$Lbl <- scales::number(x$Value, 0.1)
  if (nrow(dict) > length(col)) {
    col <- c(col, 1:(nrow(dict)-length(col)))
  }
  ggplot(dict, aes(x = DataType, y = Value, fill = Field, label = Lbl)) +
    geom_bar(stat = "identity", position = position_stack()) +
    scale_fill_manual(values = col) +
    xlab("") + ylab("") + labs(fill = "Manager") +
    geom_text(aes(label = Lbl, group = Field, y = LblPos), col = "black", 
              size = 2) +
    coord_flip() +
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
            color = col[1], 
            size = 8
          ),
          legend.position = lgnd_pos,
          axis.text.y = element_blank(),
          axis.text.x = element_text(
            size = 5, 
            family = "Source Sans Pro Light",
            color = "grey40"
          ),
          legend.key.size = unit(0.2, "in"),
          legend.text = element_text(size = 6, 
                                     family = "Source Sans Pro Light"),
          legend.box.spacing = unit(-10, "pt"))
}

# equity ----

#' @title Write IMB Equity Slide
#' @param pres presentation object from officer
#' @param rpt report object
#' @param dict dictionary table from excel
#' @param descr description table from excel
#' @param locator list with chart and table locations
#' @param slide_title string for title
#' @param tm10 t-minus 10 years date
#' @param bar_cht_opt option for bar chart: sector or region
#' @return pres with added slide
#' @export
write_equity <- function(pres, rpt, dict, descr, locater, slide_title, tm10,
                         bar_cht_opt = c("sector", "region")) {
  
  bar_cht_opt <- bar_cht_opt[1]
  col <- rpt$col
  set_flextable_defaults(font.size = 8, font.color = "black")
  dtc_name <- rpt$port[[1]]$name
  dict <- dict[dict$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in dictionary"))
  }
  descr <- descr[descr$Page == dtc_name, ]
  trail_perf_ft <- create_trail_perf_tbl(rpt)
  perf_stat_ft <- create_perf_tbl(rpt, tm10)
  char_ft <- create_char_tbl(rpt)
  wealth_cht <- create_wealth_cht(rpt, tm10)
  capm_cht <- create_capm_cht(rpt, tm10)
  if (bar_cht_opt == "sector") {
    bar_cht <- create_sector_cht(rpt)
  } else if (bar_cht_opt == "region") {
    bar_cht <- create_country_cht(rpt)
  } else {
    warning("bar_cht_option misspecified, plotting sector chart")
    bar_cht <- create_sector_cht(rpt)
  }
  
  alloc_tbl <- create_alloc_tbl(dict, col)
  descr_tbl <- create_descr_tbl(descr, col)
  
  pres <- add_slide(pres, layout = "Body Slide", master = "DTC-Theme-2021") |>
    ph_with(slide_title, ph_location_label("Text Placeholder 18")) |>
    ph_with(
      trail_perf_ft, 
      ph_location(
        left = locater$trail_perf_ft["left"],
        top = locater$trail_perf_ft["top"])) |>
    ph_with(
      perf_stat_ft, 
      ph_location(
        left = locater$perf_stat_ft["left"], 
        top = locater$perf_stat_ft["top"])) |>
    ph_with(
      char_ft, 
      ph_location(
        left = locater$char_ft["left"], 
        top = locater$char_ft["top"])) |>
    ph_with(
      wealth_cht, 
      ph_location(
        left = locater$wealth_cht["left"], 
        top = locater$wealth_cht["top"], 
        height = locater$wealth_cht["height"], 
        width = locater$wealth_cht["width"])) |>
    ph_with(
      capm_cht, 
      ph_location(
        left = locater$capm_cht["left"], 
        top = locater$capm_cht["top"], 
        height = locater$capm_cht["height"],
        width = locater$capm_cht["width"])) |>
    ph_with(
      bar_cht, 
      ph_location(
        left = locater$sect_cht["left"], 
        top = locater$sect_cht["top"], 
        width = locater$sect_cht["width"], 
        height = locater$sect_cht["height"])) |>
    ph_with(
      alloc_tbl, 
      ph_location(
        left = locater$alloct_tbl["left"], 
        top = locater$alloct_tbl["top"], 
        height = locater$alloct_tbl["height"])) |>
    ph_with(
      descr_tbl, 
      ph_location(
        left = locater$descr_tbl["left"], 
        top = locater$descr_tbl["top"]))
  
  return(pres)
}

# bond ----

#' @title Write IMB Bond Slide
#' @param pres presentation object from officer
#' @param rpt report object
#' @param dict dictionary table from excel
#' @param descr description table from excel
#' @param locator list with chart and table locations
#' @param slide_title string for title
#' @param tm10 t-minus 10 years date
#' @param pie_type plot quality or sector pie chart
#' @return pres with added slide
#' @export
write_bond <- function(pres, rpt, dict, descr, locater,
                       slide_title, tm10,
                       pie_type = c("Quality", "Sector"),
                       is_ctf = FALSE) {
  
  col <- rpt$col
  pie_type <- pie_type[1]
  set_flextable_defaults(font.size = 8, font.color = "black")
  dtc_name <- rpt$port[[1]]$name
  dict <- dict[dict$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in dictionary"))
  }
  descr <- descr[descr$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in description"))
  }
  trail_perf_ft <- create_trail_perf_tbl(rpt) |>
    width(2.25, j = 1)
  perf_stat_ft <- create_perf_tbl(rpt, tm10)
  wealth_cht <- create_wealth_cht(rpt, tm10)
  capm_cht <- create_capm_cht(rpt, tm10, funds = FALSE, legend_loc = "bottom")
  
  char_tbl <- dict[dict$DataType == "Characteristics", 3:4]
  char_tbl <- as.data.frame(char_tbl)
  colnames(char_tbl) <- c("CHARACTERISTICS", "FUND")
  is_per <- char_tbl$CHARACTERISTICS %in% c("Expense Ratio", "SEC Yield", "YTM")
  is_cur <- char_tbl$CHARACTERISTICS %in% "AUM (MMs)"
  is_num <- char_tbl$CHARACTERISTICS %in% "Duration"
  char_tbl[is_per, 2] <- scales::percent(as.numeric(char_tbl[is_per, 2]), 0.01)
  char_tbl[is_cur, 2] <- scales::number(as.numeric(char_tbl[is_cur, 2]), 
                                        big.mark = ",")
  char_tbl[is_num, 2] <- scales::number(as.numeric(char_tbl[is_num, 2]), 0.1)
  char_ft <- flextable(char_tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[1], part = "header") |>
    height(0.18, i = 1:nrow(char_tbl)) |>
    width(j = 1, width = 1.2)
  
  pie_tbl <- dict[dict$DataType == pie_type, ]
  pie_tbl$Value <- as.numeric(pie_tbl$Value)
  pie_tbl$Label <- scales::percent(pie_tbl$Value / 100, 0.1)
  pie_cht <- ggplot(pie_tbl, aes(x = "", y = Value, fill = Field, label = 
                                   Label)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = col) +
    labs(fill = "", title = toupper(pie_type)) +
    geom_text(position = position_stack(0.5), col = "white", size = 2) +
    theme(
      plot.title = element_text(
        family = "La Gioconda TT", 
        color = col[1], 
        size = 8
      ),
      legend.text = element_text(
        size = 6, 
        family = "Source Sans Pro Light", 
        color = "grey40"
      ),
      legend.key.size = unit(0.2, "in"),
      legend.box.spacing = unit(-10, "pt")
    )
  
  bar_tbl <- dict[dict$DataType == "Maturity", ]
  bar_tbl$Value <- as.numeric(bar_tbl$Value) / 100
  bar_tbl$Lbl <- scales::percent(bar_tbl$Value, 0.1)
  bar_tbl$Field <- factor(bar_tbl$Field, unique(bar_tbl$Field))
  sect_cht <- ggplot(bar_tbl, aes(x = Field, y = Value, label = Lbl, 
                                  fill = Field)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c(col, "grey", "seagreen", "brown")) +
    geom_text(
      aes(y = Value + 0.025), 
      size = 1.75, 
      position = position_dodge(0.9),
      color = "grey40") +
    xlab("") + ylab("") + labs(title = "MATURITY", fill = "") +
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
            color = col[1], 
            size = 8
          ),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(
            size = 5, 
            family = "Source Sans Pro Light",
            color = "grey40"
          ))
  
  descr_tbl <- create_descr_tbl(descr, col)
  
  if (is_ctf) {
    descr_top <- 0.5
  } else {
    descr_top <- locater$descr_tbl["top"]
  }
  
  pres <- add_slide(pres, layout = "Body Slide", master = "DTC-Theme-2021") |>
    ph_with(slide_title, ph_location_label("Text Placeholder 18")) |>
    ph_with(
      pie_cht,
      ph_location(
        left = locater$pie_cht["left"],
        top = locater$pie_cht["top"],
        height = locater$pie_cht["height"],
        width = locater$pie_cht["width"]
      )
    ) |>
    ph_with(
      trail_perf_ft, 
      ph_location(
        left = locater$trail_perf_ft["left"],
        top = locater$trail_perf_ft["top"])) |>
    ph_with(
      perf_stat_ft, 
      ph_location(
        left = locater$perf_stat_ft["left"], 
        top = locater$perf_stat_ft["top"])) |>
    ph_with(
      char_ft, 
      ph_location(
        left = locater$char_ft["left"], 
        top = locater$char_ft["top"])) |>
    ph_with(
      wealth_cht, 
      ph_location(
        left = locater$wealth_cht["left"], 
        top = locater$wealth_cht["top"], 
        height = locater$wealth_cht["height"], 
        width = locater$wealth_cht["width"])) |>
    ph_with(
      capm_cht, 
      ph_location(
        left = locater$capm_cht["left"], 
        top = locater$capm_cht["top"], 
        height = locater$capm_cht["height"],
        width = locater$capm_cht["width"])) |>
    ph_with(
      sect_cht, 
      ph_location(
        left = locater$sect_cht["left"], 
        top = locater$sect_cht["top"], 
        width = locater$sect_cht["width"], 
        height = locater$sect_cht["height"])) |>
    ph_with(
      descr_tbl, 
      ph_location(
        left = locater$descr_tbl["left"], 
        top = descr_top))
  
  if (is_ctf) {
    alloc_tbl <- create_alloc_tbl(dict, col)
    pres <- ph_with(
      pres,
      alloc_tbl,
      ph_location(
        left = locater$alloct_tbl["left"],
        top = locater$alloct_tbl["top"],
        height = locater$alloct_tbl["height"]
      ))
  }
  
  return(pres)
}

# multi-strat ----

#' @export
write_multi_strat <- function(pres, rpt, dict, descr, locater,
                              slide_title, tm10) {
  
  cash_plus_2 <- rpt$rf + 0.02 / 252
  colnames(cash_plus_2) <- "Cash + 2%"
  cash_plus_2 <- change_freq(cash_plus_2)
  col <- rpt$col
  set_flextable_defaults(font.size = 8, font.color = "black")
  dtc_name <- rpt$port[[1]]$name
  dict <- dict[dict$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in dictionary"))
  }
  descr <- descr[descr$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in description"))
  }
  trail_perf_ft <- create_trail_perf_tbl(rpt, b_or = cash_plus_2)
  perf_stat_ft <- create_perf_tbl(rpt, tm10)
  capm_cht <- create_capm_cht(rpt, tm10, funds = FALSE, legend_loc = "bottom")
  
  wealth_cht <- create_wealth_cht(rpt, tm10, b_or = cash_plus_2)
  descr_tbl <- create_descr_tbl(descr, col)
  alloc_tbl <- create_alloc_tbl(dict, col)
  
  pres <- add_slide(pres, layout = "Body Slide", master = "DTC-Theme-2021") |>
    ph_with(slide_title, ph_location_label("Text Placeholder 18")) |>
    ph_with(
      alloc_tbl,
      ph_location(
        left = locater$alloct_tbl["left"],
        top = locater$alloct_tbl["top"])) |>
    ph_with(
      trail_perf_ft, 
      ph_location(
        left = locater$trail_perf_ft["left"],
        top = locater$trail_perf_ft["top"])) |>
    ph_with(
      perf_stat_ft, 
      ph_location(
        left = locater$perf_stat_ft["left"], 
        top = locater$perf_stat_ft["top"])) |>
    ph_with(
      wealth_cht, 
      ph_location(
        left = locater$wealth_cht["left"], 
        top = locater$wealth_cht["top"], 
        height = locater$wealth_cht["height"], 
        width = locater$wealth_cht["width"])) |>
    ph_with(
      capm_cht, 
      ph_location(
        left = locater$capm_cht["left"], 
        top = locater$capm_cht["top"], 
        height = locater$capm_cht["height"],
        width = locater$capm_cht["width"])) |>
    ph_with(
      descr_tbl, 
      ph_location(
        left = locater$descr_tbl["left"], 
        top = locater$descr_tbl["top"]))
  
  return(pres)
}

#' @export
write_pdf <- function(pres, rpt, dict, descr, locater,
                              slide_title, tm10, lgnd_pos = "bottom") {
  
  cash_plus <- rpt$rf + 0.04 / 252
  colnames(cash_plus) <- "Cash + 4%"
  cash_plus <- change_freq(cash_plus)
  col <- rpt$col
  set_flextable_defaults(font.size = 8, font.color = "black")
  dtc_name <- rpt$port[[1]]$name
  dict <- dict[dict$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in dictionary"))
  }
  descr <- descr[descr$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in description"))
  }
  trail_perf_ft <- create_trail_perf_tbl(rpt, b_or = cash_plus)
  perf_stat_ft <- create_perf_tbl(rpt, tm10)
  capm_cht <- create_capm_cht(rpt, tm10, funds = FALSE, legend_loc = "bottom")
  
  wealth_cht <- create_wealth_cht(rpt, tm10, b_or = cash_plus)
  descr_tbl <- create_descr_tbl(descr, col)
  alloc_cht <- create_bar_wgt_cht(dict, col, lgnd_pos)
  
  pres <- add_slide(pres, layout = "Body Slide", master = "DTC-Theme-2021") |>
    ph_with(slide_title, ph_location_label("Text Placeholder 18")) |>
    ph_with(
      alloc_cht,
      ph_location(
        left = locater$alloct_tbl["left"],
        top = 0.77,
        height = 2.2,
        width = 4.9)) |>
    ph_with(
      trail_perf_ft, 
      ph_location(
        left = locater$trail_perf_ft["left"],
        top = locater$trail_perf_ft["top"])) |>
    ph_with(
      perf_stat_ft, 
      ph_location(
        left = locater$perf_stat_ft["left"], 
        top = locater$perf_stat_ft["top"])) |>
    ph_with(
      wealth_cht, 
      ph_location(
        left = locater$wealth_cht["left"], 
        top = locater$wealth_cht["top"], 
        height = locater$wealth_cht["height"], 
        width = locater$wealth_cht["width"])) |>
    ph_with(
      capm_cht, 
      ph_location(
        left = locater$capm_cht["left"], 
        top = locater$capm_cht["top"], 
        height = locater$capm_cht["height"],
        width = locater$capm_cht["width"])) |>
    ph_with(
      descr_tbl, 
      ph_location(
        left = locater$descr_tbl["left"], 
        top = locater$descr_tbl["top"]))
  
  return(pres)
}