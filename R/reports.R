#' @export
port_to_perf_summary <- function(p, bench = NULL, rf = NULL) {
  if (is.null(bench) && !is.null(p$benchmark)) {
    bench <- p$benchmark$port_ret
  }
  if (is_null_dim(p$rf)) {
    rf <- rf_from_const(
      zoo::index(p$asset_ret[1]),
      zoo::index(p$asset_ret[nrow(p$asset_ret)]),
      0.025
    )
  } else {
    rf <- p$rf
  }
  perf_summary(p$asset_ret, rf, bench)
}

#' @export
perf_summary <- function(asset, rf, bench = NULL, freq = "days") {
  if (!is.null(bench)) {
    combo <- xts_cbind(asset, bench)
  } else {
    combo <- asset
  }
  hist_cov <- cov(combo) * freq_to_scaler(freq)
  if (nrow(combo) >= freq_to_scaler(freq)) {
    geo_ret <- calc_geo_ret(combo, freq)
  } else {
    geo_ret <- apply(combo + 1, 2, prod) - 1
  }
  vol <- calc_vol(combo, freq)
  down_vol <- calc_down_vol(combo, freq)
  max_dd <- calc_max_drawdown(combo)
  sharpe <- calc_sharpe_ratio(combo, rf, freq)
  sortino <- calc_sortino_ratio(combo, rf, freq)
  recov <- geo_ret / -max_dd
  if (is.null(bench)) {
    x <- rbind(geo_ret, vol, down_vol, max_dd, sharpe, sortino, recov)
    xdf <- data.frame(
      Metric = c("Geometric Return", "Volatility", "Downside Vol",
                 "Worst Drawdown", "Sharpe Ratio", "Sortino Ratio", "Recovery"),
      x,
      row.names = NULL
    )
    colnames(xdf) <- c("Metric", colnames(asset))
    return(xdf)
  } else {
    a_cov <- list()
    for (i in 1:ncol(bench)) {
      a_ret <- asset - bench[, rep(i, ncol(asset))]
      a_cov[[i]] <- cov(a_ret) * freq_to_scaler(freq)
    }
    bench_pad_na <- rep(NA, length(a_cov))
    te <- c(sqrt(diag(a_cov[[1]])), bench_pad_na)
    up_capt <- c(calc_up_capture(asset, bench[, 1]), bench_pad_na)
    down_capt <- c(calc_down_capture(asset, bench[, 1]), bench_pad_na)
    xbeta <- hist_cov[, (ncol(asset) + 1)] / hist_cov[(ncol(asset) + 1),
                                                      (ncol(asset) + 1)]
    act_ret <- c(geo_ret[1:ncol(asset)] - geo_ret[(ncol(asset)+1)],
                 bench_pad_na)
    n_bench <- ncol(bench)
    if (n_bench > 1) {
      for (i in 2:n_bench) {
        te <- rbind(te, c(sqrt(diag(a_cov[[i]])), bench_pad_na))
        up_capt <- rbind(
          up_capt,
          c(calc_up_capture(asset, bench[, i]),bench_pad_na)
        )
        down_capt <- rbind(
          down_capt,
          c(calc_down_capture(asset, bench[, i]), bench_pad_na)
        )
        xbeta <- rbind(
          xbeta,
          hist_cov[, (ncol(asset) + i)] /
            hist_cov[(ncol(asset) + i), (ncol(asset) + i)]
        )
        act_ret <- rbind(
          act_ret,
          c(geo_ret[1:ncol(asset)] - geo_ret[(ncol(asset)+i)], bench_pad_na)
        )
      }
    }
    x <- rbind(geo_ret, act_ret, vol, down_vol, max_dd, sharpe, sortino, recov,
               te, xbeta, act_ret / te, up_capt, down_capt)
    metric <- c(
      "Geometric Return",
      paste0("Active Return: Bench ", 1:n_bench),
      "Volatility",
      "Downside Vol",
      "Worst Drawdown",
      "Sharpe Ratio",
      "Sortino Ratio",
      "Recovery",
      paste0("Tracking Error: Bench ", 1:n_bench),
      paste0("Beta: Bench ", 1:n_bench),
      paste0("Info Ratio", 1:n_bench),
      paste0("Up Capture", 1:n_bench),
      paste0("Down Capture", 1:n_bench)
    )
    xdf <- data.frame(
      Metric = metric,
      x,
      row.names = NULL
    )
    colnames(xdf) <- c("Metric", colnames(asset),
                       paste0("Bench ", 1:n_bench, ": ", colnames(bench)))
    return(xdf)
  }
}

#' @title Helper function to set color palette
#' @return list of colors
#' @export
dtc_col <- function() {
  c(rgb(0, 48, 87, maxColorValue = 255),     # navy blue
    rgb(204, 159, 38, maxColorValue = 255),  # gold
    rgb(197, 82, 101, maxColorValue = 255),  # berry
    rgb(113, 158, 139, maxColorValue = 255), # pine
    rgb(124, 126, 127, maxColorValue = 255), # deep charcoal
    rgb(124, 94, 119, maxColorValue = 255),  # amethyst
    rgb(222, 137, 88, maxColorValue = 255),  # orange
    rgb(141, 169, 180, maxColorValue = 255), # ocean
    rgb(192, 109, 89, maxColorValue = 255))  # terra cotta
}

#' @title Set n number of assets to plot to DTC color palette
#' @param n number of assets / items to plot
#' @return list of colors
#' @details if the number of assets exceed the number of colors in the DTC
#'   palette, 1:n colors will be generated based on numbers
#' @export
set_plot_col <- function(n) {
  col <- dtc_col()[1:n]
  if (n > length(col)) {
    col <- c(col, 1:(n - length(col)))
  }
  return(col)
}

#' @title Plot Drawdowns
#' @param x xts object with time-series of returns
#' @export
viz_drawdowns <- function(x) {
  dd <- calc_drawdown(x)
  dat <- xts_to_tidy(dd)
  col <- set_plot_col(ncol(x))
  ggplot(dat, aes(x = Date, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = col) +
    scale_y_continuous(labels = scales::percent) +
    ylab("") +
    labs(color = "", title = "Drawdowns") +
    theme_light()
}

#' @title Plot Cumulative Returns
#' @param x xts object with time-series of returns
#' @param init_val initial starting value of wealth
#' @export
viz_wealth_index <- function(x, init_val = 100) {
  wi <- ret_to_price(x) * init_val
  dat <- xts_to_tidy(wi)
  dat$name <- factor(dat$name, unique(dat$name))
  col <- set_plot_col(ncol(x))
  ggplot(dat, aes(x = Date, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = col) +
    scale_y_continuous(labels = scales::number) +
    ylab("") +
    labs(color = "", title = "Cumulative Wealth") +
    theme_light()
}

#' @title Plot Rolling Volatility
#' @param x xts object with time-series of returns
#' @param n number of periods for rolling window
#' @param freq string to represent frequency for scaling
#' @export
viz_roll_vol <- function(x, n, freq) {
  col <- set_plot_col(ncol(x))
  x <- xts_to_dataframe(x)
  rv <- slider::slide(x[, -1], ~apply(.x, 2, sd), .complete = TRUE,
                      .before = (n-1))
  rv <- do.call('rbind', rv) * sqrt(freq_to_scaler(freq))
  rv <- data.frame(Date = x$Date[(n):nrow(x)], rv)
  colnames(rv) <- colnames(x)
  dat <- pivot_longer(rv, cols = -Date)
  ggplot(dat, aes(x = Date, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = col) +
    scale_y_continuous(labels = scales::percent) +
    ylab("") +
    labs(color = "",
         title = paste0("Rolling ", n, " ", freq, " Volatility")) +
    theme_light()
}

#' @title Plot Rolling Beta
#' @param x xts object of assets to calculate beta for
#' @param b xts object of one benchmark to calculate beta to
#' @param rf xts object of risk-free rate
#' @param n number of rolling periods to use in the calculation
#' @param freq string to represent frequency for chart title
#' @export
viz_roll_beta <- function(x, b, rf, n, freq) {
  if (ncol(b) > 1) {
    b <- b[, 1]
    warning('more than one benchmark column provided, taking first column')
  }
  if (ncol(rf) > 1) {
    rf <- rf[, 1]
    warning('more than one rf column provided, taking first column')
  }
  xbeta <- roll_beta(x, b, rf, n)
  dat <- xts_to_tidy(xbeta)
  col <- set_plot_col(ncol(xbeta))
  ggplot(dat, aes(x = Date, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = col) +
    scale_y_continuous(labels = scales::number) +
    ylab("") +
    labs(color = "",
         title = paste0("Rolling ", n, " ", freq, " Beta to ", colnames(b))) +
    theme_light()
}


#' @title Plot Risk / Reward Trade-offs
#' @param x xts of asset returns
#' @param b optional xts of benchmark returns for relative trade-off
#' @param risk_type character to specify vol, down-vol, or drawdown, if b
#'   is entered then TE, down-TE, and underperformance are calculated
#' @param period character representing frequency to annualized vol
#' @export
viz_trade_off <- function(x, b = NULL,
                          risk_type = c("vol", "down-vol", "drawdown"),
                          period = "days") {
  col <- set_plot_col(ncol(x))
  risk_type <- risk_type[1]
  if (!is.null(b)) {
    res <- clean_asset_bench_rf(x, b)
    x <- res$x - res$b[, rep(1, ncol(x))]
  }
  if (risk_type == "vol") {
    risk <- calc_vol(x, period)
  } else if (risk_type == "down-vol") {
    risk <- calc_down_vol(x, period)
  } else if (risk_type == "drawdown") {
    risk <- abs(calc_max_drawdown(x))
  } else {
    stop("risk_type must be vol, down-vol, or drawdown")
  }
  ret <- calc_geo_ret(x, period)
  dat <- data.frame(Asset = colnames(x), Return = ret, Risk = risk)
  ggplot(dat, aes(x = Risk, y = Return, col = Asset, label = Asset)) +
    geom_point(size = 3) +
    ggrepel::geom_text_repel() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = col) +
    theme_light() +
    theme(legend.position = "none")
}


#' @export
viz_roll_style <- function(x, b, n = 63, freq = "days") {
  col <- set_plot_col(ncol(res))
  res <- roll_style(x, b, n)
  dat <- xts_to_tidy(res)
  ggplot(dat, aes(x = Date, y = value, fill = name)) +
    geom_area() +
    ylab("") + xlab("") +
    scale_y_continuous(labels = scales::percent) +
    labs(fill = "",
         title = paste0("Rolling ", n, " ", freq, " Style Analysis")) +
    scale_fill_manual(values = col) +
    theme_light()
}

#' @export
viz_equity_style <- function(x, b, n) {
  combo <- clean_asset_bench_rf(x, b)
  res <- te_min_qp(combo$x[, 1], combo$b)
  asset <- data.frame(
    Label = colnames(x)[1],
    X = res$solution[2] + res$solution[4] - res$solution[1] - res$solution[3],
    Y = res$solution[1] + res$solution[2] - res$solution[3] - res$solution[4]
  )
  if (ncol(combo$x) > 1) {
    for (i in 2:ncol(combo$x)) {
      res <- te_min_qp(combo$x[, i], combo$b)
      xdf <- data.frame(
        Label = colnames(x)[i],
        X = res$solution[2] + res$solution[3] - res$solution[1] - res$solution[4],
        Y = res$solution[1] + res$solution[2] - res$solution[3] - res$solution[4]
      )
      asset <- rbind(asset, xdf)
    }
  }
  dat <- rbind(asset, eq_style_mat())
  dat$Col <- c(colnames(combo$x), rep("zzz", 4))
  col <- set_plot_col(ncol(combo$x))
  ggplot(dat, aes(x = X, y = Y, label = Label, col = Col)) +
    geom_point(size = 3) +
    scale_color_manual(values = c(col, alpha("black", 0.5))) +
    ggrepel::geom_text_repel() +
    xlab("") + ylab("") +
    theme(legend.position = "none")

}

eq_style_mat <- function() {
  data.frame(
    Label = c("Large Value", "Large Growth", "Small Value", "Small Growth"),
    X = c(-1, 1, -1, 1),
    Y = c(1, 1, -1, -1)
  )
}

#' @export
clean_ret_const <- function(x, a = 0) {
  x[is.na(x)] <- a
  return(x)
}

#' @export
run_cluster <- function(ret, k_group) {

  clv_res <- ClustVarLV::CLV(ret, method = 2)
  group_res <- summary(clv_res, k_group)
  group_nm <- rownames(group_res$groups[[1]])
  if (k_group > 1) {
    for (i in 2:k_group) {
      group_nm <- c(group_nm, rownames(group_res$groups[[i]]))
    }
  }
  p <- psych::pca(ret[, group_nm], k_group)
  p2 <- psych::pca(ret, k_group)
  group_df <- data.frame(Manager = colnames(ret),
                         Group = ClustVarLV::get_partition(clv_res, k_group),
                         P1 = p2$loadings[, 1],
                         P2 = p2$loadings[, 2])
  group_df$Group <- factor(group_df$Group, unique(group_df$Group))
  g_biplot <- ggplot(group_df, aes(x = P2, y = P1, color = Group, label = Manager)) +
    geom_point() +
    geom_segment(aes(x = 0, y = 0, xend = P2, yend = P1)) +
    ggrepel::geom_text_repel(size = 3) +
    xlab('Component 2 Loading') + ylab('Component 1 Loading') +
    theme(legend.position = 'none')
  p_loadings <- data.frame(Manager = rownames(p$loadings), p$loadings[,])
  p_load_group <- merge(p_loadings, group_df[, c('Manager', 'Group')])
  plotdf <- tidyr::pivot_longer(p_load_group, -one_of('Manager', 'Group'))
  plotdf$Group <- factor(plotdf$Group, unique(plotdf$Group))
  plotdf$Manager <- factor(plotdf$Manager, unique(plotdf$Manager))
  plotdf$name <- factor(plotdf$name, unique(plotdf$name))
  plotdf <- plotdf[order(plotdf$Group, decreasing = TRUE), ]
  g_load <- ggplot(plotdf, aes(x = Manager, y = value, fill = Group)) +
    scale_x_discrete(limits = unique(plotdf$Manager)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    coord_flip() +
    facet_wrap(. ~ name) +
    theme(legend.position = 'none')
  res <- list()
  res$biplot <- g_biplot
  res$loadings <- g_load
  return(res)
}

#' @export
eom_cal_perf_dt <- function(as_of = NULL, eom = TRUE) {
  if (is.null(as_of)) {
    as_of <- lubridate::floor_date(Sys.Date(), "months") - 1
  }
  dt <- c(
    as_of,
    add_with_rollback(as_of, months(-1)),
    add_with_rollback(as_of, months(-3)),
    as_of - years(1),
    as_of - years(3),
    as_of - years(5),
    as_of - years(10)
  )
  if (eom) {
    dt <- eo_month(ceiling_date(dt, "months"))
  }
  names(dt) <- c("As of", "1 Mo", "3 Mo", "1 Yr", "3 Yr", "5 Yr", "10 Yr")
  return(dt)
}

#' @export
ctf_daily_est <- function(ac, dtc_name, msl = NULL, sum_to_1 = TRUE, 
                          m_seq = TRUE) {
  tbl_hold <- read_hold(ac, dtc_name, FALSE)
  tbl_hold$TimeStamp <- as.Date(tbl_hold$TimeStamp)
  dt <- unique(tbl_hold$TimeStamp)
  if (m_seq) {
    month_seq <- seq.Date(min(dt), max(dt), "months")
    tbl_hold <- filter(tbl_hold, TimeStamp %in% month_seq)
  }
  if (is.null(msl)) {
    lib <- ac$get_library("meta-tables")
    tbl_msl <- lib$read("msl")$data
  }
  is_dup <- duplicated(paste0(tbl_hold$Name, tbl_hold$TimeStamp))
  tbl_hold <- tbl_hold[!is_dup, ]
  res <- merge_msl(tbl_hold, tbl_msl, FALSE)
  tbl_miss <- res$miss
  is_dup <- duplicated(paste0(res$inter$DtcName, res$inter$TimeStamp))
  if (any(is_dup)) {
    warning("duplicate values found after msl merge")
    res$inter <- res$inter[!is_dup, ]
  }
  rebal_wgt <- tidyr::pivot_wider(
    data = res$inter, 
    id_cols = TimeStamp,
    values_from = CapWgt,
    names_from = DtcName)
  rebal_wgt <- xts(rebal_wgt[, -1], as.Date(rebal_wgt$TimeStamp))
  asset_ret <- read_ret(colnames(rebal_wgt), ac)
  res <- clean_rebal_ret(asset_ret, rebal_wgt)
  asset_ret <- res$asset_ret
  rebal_wgt <- res$reb_wgt
  asset_ret[is.na(asset_ret)] <- 0
  rebal_wgt[is.na(rebal_wgt)] <- 0
  reb <- Rebal$new(rebal_wgt, asset_ret, name = dtc_name, rebal_freq = "BH")
  reb$align_rebal_wgt()
  reb$rebal(sum_to_1)
  out <- list()
  out$tbl_miss <- tbl_miss
  out$res <- res
  out$reb <- reb
  return(out)
}

#' @export
clean_rebal_ret <- function(asset_ret, rebal_wgt) {
  is_miss <- setdiff(colnames(rebal_wgt), colnames(asset_ret))
  if (identical(is_miss, colnames(rebal_wgt))) {
    stop("no intersection of rebal_wgt and asset_ret columns")
  }
  if (length(is_miss) > 0) {
    miss <- is_miss
  } else {
    miss <- NA
  }
  inter <- intersect(colnames(rebal_wgt), colnames(asset_ret))
  asset_ret <- asset_ret[, inter]
  rebal_wgt <- rebal_wgt[, inter]
  reb_start <- zoo::index(rebal_wgt)[1]
  if (zoo::index(asset_ret[1]) > reb_start) {
    stop("rebal_wgt start before asset_ret")
  }
  asset_ret <- asset_ret[format(reb_start, "%Y-%m/")]
  r <- ret_date_info(asset_ret)
  w <- ret_date_info(rebal_wgt)
  is_miss <- r$Start > w$Start | r$End < w$End
  if (any(is_miss)) {
    miss <- c(miss, r$Name[is_miss])
    miss <- na.omit(miss)
  } else {
    miss <- NA
  }
  res <- list()
  res$miss <- miss
  res$asset_ret <- asset_ret
  res$reb_wgt <- rebal_wgt
  res$r <- r
  res$w <- w
  return(res)
}

miss_ret <- function(ac) {
  lib <- get_all_lib(ac)
  tbl_msl <- lib$`meta-tables`$read("msl")$data
  r <- read_ret(tbl_msl$DtcName, ac)  
  date_info <- ret_date_info(r)
  is_miss <- is.na(date_info$Start)
  miss <- date_info$Name[is_miss]
  is_miss <- !tbl_msl$DtcName %in% date_info$Name
  miss <- c(miss, tbl_msl$DtcName[is_miss])
  return(miss)
}

# update AllocatoR
ret_date_info <- function(x) {
  sdate <- rep(NA, ncol(x))
  edate <- sdate
  for (i in 1:ncol(x)) {
    dt <- zoo::index(na.omit(x[, i]))
    if (length(dt) == 0) {dt <- NA}
    sdate[i] <- dt[1]
    edate[i] <- dt[length(dt)]
  }
  nm <- colnames(x)
  if (is.null(nm)) {
    nm <- 1:ncol(x)
  }
  data.frame(Name = nm, Start = as.Date(sdate), End = as.Date(edate))
}

ctf_daily_est_mv <- function(ac, dtc_name, as_of = NULL) {
  if (is.null(as_of)) {
    as_of <- last_us_trading_day()
  }
  lib <- get_all_lib(ac)
  tx <- lib$transactions$read(dtc_name)$data
  tx$TradeDate <- as.Date(tx$TradeDate)
  cf <- filter(tx, Action != "EndingValue") |>
    group_by(TradeDate) |>
    summarize(Value = sum(Value))
  if (nrow(cf) > 0) {
    ix <- cf$TradeDate %in% as_of
    if (any(ix)) {
      cf <- cf$Value[ix]
    } else {
      cf <- 0
    }
  } else {
    cf <- 0
  }
  t_1 <- prev_trading_day(as_of, 1)
  beg_val_log <- tx$TradeDate == t_1 & tx$Action == "EndingValue"
  if (all(beg_val_log == FALSE)) {
    stop("previous trading day beginning market value not found")
  }
  beg_val <- tx$Value[beg_val_log]
  if (length(beg_val) > 1) {
    warning("more than 1 entry found for prev trading day beg mv")
    beg_val <- beg_val[1]
  }
  end_val_log <- tx$TradeDate == as_of & tx$Action == "EndingValue"
  if (all(end_val_log == FALSE)) {
    stop("previous trading day ending market value not found")
  }
  end_val <- tx$Value[end_val_log]
  if (length(end_val) > 1) {
    warning("more than 1 entry found for prev trading day end mv")
    end_val <- end_val[1]
  }
  # modified deitz assuming cash flow is traded mid-day
  ret <- (end_val - beg_val - cf) / (beg_val + 0.5 * cf)
}
