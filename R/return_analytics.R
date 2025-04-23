#' @export
return_analytics <- function(rv) {

  freq <- rv$freq
  withProgress(message = "Calculation Running", {
    theme_set(
      theme(
        plot.title = element_text(
          family = "Source Sans Pro", 
          color = rgb(0, 48, 87, maxColorValue = 255),
          size = 14
        ),
        legend.position = "bottom",
        text = element_text(
          size = 10,
          family = "Source Sans Pro Light",
          color = "grey40"
        ),
        legend.key.size = unit(0.2, "cm"),
        panel.background = element_blank(),
        strip.background = element_blank()))

    n_steps <- 6

    asset <- cut_time(rv$ra$asset, rv$dt_rng[1], rv$dt_rng[2])
    colnames(asset) <- make.unique(colnames(asset))
    bench <- cut_time(rv$ra$bench, rv$dt_rng[1], rv$dt_rng[2])
    rf <- cut_time(rv$ra$rf, rv$dt_rng[1], rv$dt_rng[2])
    r <- cbind.xts(asset, bench, check.names = FALSE)
    colnames(r) <- make.unique(colnames(r))
    if (nrow(r) < 6) {
      showModal(
        modalDialog(
          title = "Error",
          "Not enough observations of data.",
          easyClose = TRUE
        )
      )
      return(NULL)
    }

    start <- zoo::index(r)[1]
    end <- zoo::index(r)[nrow(r)]
    rv$dt_rng[1] <- start
    rv$dt_rng[2] <- end

    rv$ra$ret <- cbind.xts(r, rf, check_names = FALSE)

    # stats ----
    a <- freq_to_scaler(freq)
    cum_ret <- apply(r+1, 2, prod) - 1
    ann_ret <- calc_geo_ret(r, freq)
    vol <- calc_vol(r, freq)
    sharpe <- calc_sharpe_ratio(r, rf, freq)
    down_vol <- calc_down_vol(r, freq)
    best_month <- apply(r, 2, max)
    worst_month <- apply(r, 2, min)
    worst_dd <- calc_max_drawdown(r)
    pos_mo <- colSums(apply(r, 2, \(x) {x >= 0})) / nrow(r)
    neg_mo <- 1 - pos_mo
    xbeta <- calc_uni_beta(r, bench, rf)[1:ncol(r)]
    risk_prem <- ann_ret - calc_geo_ret(rf, freq)
    beta_adj <- risk_prem[ncol(r)] * xbeta[1:(ncol(r)-1)]
    xalpha <- c(risk_prem[1:(ncol(r)-1)] - beta_adj, 0)
    xcor <- cor(r)[, ncol(r)]
    up_capt <- c(calc_up_capture(asset, bench, freq), 1)
    down_capt <- c(calc_down_capture(asset, bench, freq), 1)
    te <- c(apply(asset - bench[, rep(1, ncol(asset))], 2, sd) * sqrt(a), 0)
    ir <- c((ann_ret[1:ncol(asset)] - ann_ret[(ncol(asset)+1)]) /
              te[1:ncol(asset)], 0)

    incProgress(1/n_steps)

    perf_df <- data.frame(
      Name = factor(colnames(r), colnames(r)),
      Cumulative.Return = scales::percent(cum_ret, 0.01),
      Annualized.Return = scales::percent(ann_ret, 0.01),
      Volatility = scales::percent(vol, 0.01),
      Sharpe.Ratio = scales::number(sharpe, 0.01),
      Downside.Risk = scales::percent(down_vol, 0.01),
      Best.Month = scales::percent(best_month, 0.01),
      Worst.Month = scales::percent(worst_month, 0.01),
      Worst.Drawdown = scales::percent(worst_dd, 0.01),
      Positive.Months = scales::percent(pos_mo, 0.01),
      Negative.Months = scales::percent(neg_mo, 0.01),
      Beta = scales::number(xbeta, 0.01),
      Alpha = scales::percent(xalpha, 0.01),
      Correlation = scales::percent(xcor, 0.01),
      Up.Capture = scales::percent(up_capt, 0.01),
      Down.Capture = scales::percent(down_capt, 0.01),
      Tracking.Error = scales::percent(te, 0.01),
      Info.Ratio = scales::number(ir, 0.01),
      row.names = NULL
    )
    colnames(perf_df) <- gsub("\\.", " ", colnames(perf_df))
    cht <- t(perf_df[, -1])
    cht <- data.frame(Metric = rownames(cht), cht, row.names = NULL)
    colnames(cht)[2:ncol(cht)] <- colnames(r)
    rv$ra$stats$cht <- cht
    a <- perf_df[, 1:11]
    b <- perf_df[, c(1, 12:ncol(perf_df))]
    rv$ra$stats$ts <- list(a = a, b = b)
    # trailing performance ----
    trail_perf <- create_trail_perf_tbl(rv$dt_rng[2], r, freq)
    trail_perf[, -1] <- apply(trail_perf[, -1], 2,
                              scales::percent, accuracy = 0.01)
    rv$ra$trail_perf$cht <- trail_perf

    incProgress(2 / n_steps)

    # wealth ----
    wealth <- ret_to_price(r) * 100
    dat <- xts_to_tidy(wealth)
    dat$name <- factor(dat$name, unique(dat$name))
    dat$value <- round(dat$value, 1)
    rv$ra$wealth$ts <- wealth
    rv$ra$wealth$cht <- line_plot(dat)

    incProgress(3 / n_steps)

    # CAPM ----
    dat <- data.frame(
      name = colnames(r),
      vol = round(vol, 4),
      ret = round(ann_ret, 4)
    )
    rv$ra$capm$cht <- capm_plot(dat)
    rv$ra$capm$ts <- dat
    incProgress(4 / n_steps)

    # Drawdowns ----
    bench_dd <- find_drawdowns(bench)
    bench_dd <- bench_dd[order(bench_dd$Drawdown), ]
    dd_start <- bench_dd[1:6, "StartDate"]
    dd_end <- bench_dd[1:6, "TroughDate"]
    dat <- list()
    for (i in 1:6) {
      rng <- paste0(dd_start[i], "/", dd_end[i])
      dat[[i]] <- apply(r[rng]+1, 2, prod) - 1
    }
    dat <- data.frame(do.call(rbind, dat))
    colnames(dat) <- colnames(r)
    dat$Range <- paste0(
      format(dd_start, "%b %Y"),
      " to ",
      format(dd_end, "%b %Y")
    )
    dat <- tidyr::pivot_longer(dat, -Range)
    rv$ra$worst_dd$cht <- worst_dd_plot(worst_dd)
    rv$ra$worst_dd$ts <- worst_dd
    rv$ra$bench_dd$cht <- bench_dd_plot(dat)
    rv$ra$bench_dd$ts <- dat
    dd <- calc_drawdown(r)
    rv$ra$all_dd$ts <- dd
    dat <- xts_to_tidy(dd)
    dat$name <- factor(dat$name, unique(dat$name))
    dat$value <- round(dat$value, 3)
    all_dd <- line_plot(dat) +
      labs(title = "Drawdowns") +
      scale_y_continuous(labels = scales::percent)
    rv$ra$all_dd$cht <- all_dd
    rv$ra$all_dd$ts <- dd

    incProgress(5 / n_steps)

    # Rolling Analysis ----
    a <- freq_to_scaler(freq)
    minus_36_mo <- add_with_rollback(end, months(-36))
    if (start < minus_36_mo) {
      rr <- roll_ret(r, a, freq = freq)
      rar <- roll_ret(asset, a, bench, freq = freq)
      rr2 <- roll_r2(asset, bench, a)
      rcor <- xts(apply(rr2, 2, sqrt), zoo::index(rr2))
      rbeta <- roll_beta(asset, bench, rf, a)
      rvol <- roll_vol(r, a, freq = freq)
      rte <- roll_vol(asset, a, bench, freq)
      rrp <- roll_ret(r, a, rf, freq = freq)
      rshp <- rrp / rvol

      rv$ra$roll_ret$ts <- rr
      cht <- roll_plot(rv$ra$roll_ret$ts) +
        labs(title = "Rolling 12 Month Returns")
      rv$ra$roll_ret$cht <- cht
      rv$ra$roll_act_ret$ts <- rar
      rv$ra$roll_act_ret$cht <- roll_plot(rar) +
        labs(title = "Rolling 12 Month Active Returns vs. Benchmark")
      rv$ra$roll_corr$ts <- rcor
      rv$ra$roll_corr$cht <- roll_plot(rcor) +
        labs(title = "Rolling 3 Year Correlation") +
        scale_y_continuous(labels = scales::number)
      rv$ra$roll_beta$ts <- rbeta
      rv$ra$roll_beta$cht <- roll_plot(rbeta) +
        labs(title = "Rolling 3 Year Beta to Benchmark") +
        scale_y_continuous(labels = scales::number)
      rv$ra$roll_te$ts <- rte
      rv$ra$roll_te$cht <- roll_plot(rte) +
        labs(title = "Rolling 12 Month Tracking Error")
      rv$ra$roll_vol$ts <- rvol
      rv$ra$roll_vol$cht <- roll_plot(rvol) +
        labs(title = "Rolling 12 Month Volatility")
      rv$ra$roll_sharpe$ts <- rshp
      rv$ra$roll_sharpe$cht <- roll_plot(rshp) +
        labs(title = "Rolling 12 Month Sharpe Ratio") +
        scale_y_continuous(labels = scales::number)
    } else {
      showModal(
        modalDialog(
          title = "Warning",
          "You need at least 36 months of data for rolling calcs.",
          easyClose = TRUE
        )
      )
    }

    # excel export ----
    rv$xl_out <- list(
      returns = xts_to_dataframe(rv$ra$ret),
      perf_stats = rv$ra$stats$cht,
      trail_perf = rv$ra$trail_perf$cht,
      wealth = xts_to_dataframe(rv$ra$wealth$ts),
      bench_drawdowns = rv$ra$bench_dd$ts,
      all_drawdowns = xts_to_dataframe(rv$ra$all_dd$ts),
      roll_ret = xts_to_dataframe(rv$ra$roll_ret$ts),
      roll_active_ret = xts_to_dataframe(rv$ra$roll_act_ret$ts),
      roll_corr = xts_to_dataframe(rv$ra$roll_corr$ts),
      roll_beta = xts_to_dataframe(rv$ra$roll_beta$ts),
      roll_vol = xts_to_dataframe(rv$ra$roll_vol$ts),
      roll_te = xts_to_dataframe(rv$ra$roll_te$ts),
      roll_sharpe = xts_to_dataframe(rv$ra$roll_sharpe$ts)
    )


    # end analysis ----
    showModal(
      modalDialog(
        title = "Success!",
        "You can now view the analysis.",
        easyClose = TRUE
      )
    )
    return(rv)
  })
}

# perf functions ----

line_plot <- function(dat, col = NULL) {
  if (is.null(col)) {
    col <- set_plot_col(length(unique(dat$name)))
  }
  ggplot(dat, aes(x = Date, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = col) +
    labs(color = "", title = "Growth of $100") +
    xlab("") + ylab("") +
    theme(legend.position = "bottom")
}

capm_plot <- function(dat, col = NULL) {
  if (is.null(col)) {
    col <- set_plot_col(length(unique(dat$name)))
  }
  ggplot(dat, aes(x = vol, y = ret, color = name)) +
    geom_point(size = 3) +
    scale_color_manual(values = col) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(color = "", title = "Risk and Return Trade-off") +
    xlab("Volatility") + ylab("Return") +
    theme(legend.position = "bottom")
}

cal_perf_dt <- function(as_of = NULL) {
  if (is.null(as_of)) {
    as_of <- floor_date(Sys.Date(), "months") - 1
    m1 <- as_of
  } else {
    m1 <- floor_date(as_of, "months") - 1
  }
  m3 <- add_with_rollback(as_of, months(-2))
  m6 <- add_with_rollback(as_of, months(-5))
  ytd <- as.Date(paste0(year(as_of), "-01-01"))
  y1 <- add_with_rollback(as_of, months(-11))
  y3 <- add_with_rollback(as_of, months(-35))
  y5 <- add_with_rollback(as_of, months(-59))
  y7 <- add_with_rollback(as_of, months(-83))
  y10 <- add_with_rollback(as_of, months(-119))
  y15 <- add_with_rollback(as_of, months(-179))
  y20 <- add_with_rollback(as_of, months(-239))
  dt <- c(m1, m3, m6, ytd, y1, y3, y5, y7, y10, y15, y20)
  names(dt) <- c("1 Month", "3 Month", "6 Month", "YTD", "1 Year",
                 "3 Year", "5 Year", "7 Year", "10 Year", "15 Year",
                 "20 Year")
  return(dt)
}

create_trail_perf_tbl <- function(as_of, r, freq) {
  dt <- cal_perf_dt(as_of)
  res <- list()
  for (i in 1:length(dt)) {
    if (dt[i] < min(zoo::index(r))) {
      miss <- rep(NA, ncol(r))
      names(miss) <- colnames(r)
      res[[names(dt)[i]]] <- miss
    } else {
      if (i %in% 1:5) {
        res[[names(dt)[i]]] <- apply(r[paste0(dt[i], "/")]+1, 2, prod) - 1
      } else {
        res[[names(dt)[i]]] <- calc_geo_ret(r[paste0(dt[i], "/")], freq)
      }
    }
  }
  xdf <- data.frame(Period = names(dt), do.call(rbind, res),
                    row.names = NULL)
  colnames(xdf)[-1] <- colnames(r)
  ann_ret <- change_freq(r, "years")
  adf <- data.frame(Period = year(zoo::index(ann_ret)), ann_ret,
                    row.names = NULL)
  colnames(adf)[-1] <- colnames(r)
  rbind(xdf, adf)
}

worst_dd_plot <- function(worst_dd) {
  col <- set_plot_col(length(worst_dd))
  dat <- data.frame(Name = names(worst_dd), Drawdown = worst_dd)
  dat$Name <- factor(dat$Name, unique(dat$Name))
  dat$Drawdown <- round(dat$Drawdown, 3)
  ggplot(dat, aes(x = Name, y = Drawdown, fill = Name)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = col) +
    labs(fill = "", title = "Worst Drawdowns For Each Investment") +
    xlab("") + ylab("Worst Drawdown") +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.x = element_blank())
}

bench_dd_plot <- function(dat) {
  dat$name <- factor(dat$name, unique(dat$name))
  dat$value <- round(dat$value, 3)
  dat$Range <- factor(dat$Range, unique(dat$Range))
  col <- set_plot_col(length(unique(dat$name)))
  ggplot(dat, aes(x = name, y = value, fill = name)) +
    geom_bar(stat = "identity") +
    facet_wrap(.~Range) +
    scale_fill_manual(values = col) +
    labs(fill = "", title = "Worst Benchmark Drawdowns") +
    xlab("") + ylab("") +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.x = element_blank())
}

roll_plot <- function(x) {
  col <- set_plot_col(ncol(x))
  dat <- xts_to_tidy(x)
  dat$name <- factor(dat$name, unique(dat$name))
  dat$value <- round(dat$value, 3)
  ggplot(dat, aes(x = Date, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = col) +
    scale_y_continuous(labels = scales::percent) +
    xlab("") + ylab("") +
    labs(color = "") +
    theme(legend.position = "bottom")
}

set_plot_col <- function(n) {
  dtc_col <- c(
    rgb(0, 48, 87, maxColorValue = 255), # navy blue
    rgb(113, 158, 139, maxColorValue = 255), # pine
    rgb(144, 148, 150, maxColorValue = 255), # light charcoal
    rgb(222, 137, 88, maxColorValue = 255), # gold
    rgb(192, 109, 89, maxColorValue = 255), # terra cotta
    rgb(124, 94, 119, maxColorValue = 255), # amethyst
    rgb(124, 126, 127, maxColorValue = 255), # deep charcoal
    rgb(172, 202, 205, maxColorValue = 255), # mist
    rgb(197, 82, 101, maxColorValue = 255), # berry
    rgb(222, 137, 88, maxColorValue = 255), # orange
    rgb(141, 169, 180, maxColorValue = 255) # ocean
  )
  if (n > length(dtc_col)) {
    dtc_col <- cbind(dtc_col, 1:(n - length(dtc_col)))
  }
  return(dtc_col)
}
