


#' @title Calculate Weighted Harmonic Mean
#' @param w weight vector
#' @param x data vector
#' @export
wgt_harmonic_mean <- function(w, x) {
  sum(w, na.rm = TRUE) / sum(w / x, na.rm = TRUE)
}

#' @title Calculate Weighted Harmonic Mean of a Ratio in a data.frame
#' @param tbl_hold data.frame
#' @param x column name that contains the ratio
#' @param w column name that contains the weights
#' @details excludes missing and negative ratio and reconstitutes the weights
#' @export
calc_wgt_multiple <- function(tbl_hold, x = "PE", w = "CapWgt") {
  miss <- is.na(tbl_hold[, x])
  tbl_hold <- tbl_hold[!miss, ]
  neg_earn <- tbl_hold[, x] < 0
  tbl_hold <- tbl_hold[!neg_earn, ]
  tbl_hold[, w] <- tbl_hold[, w] / sum(tbl_hold[, w], na.rm = TRUE)
  wgt_harmonic_mean(tbl_hold[, w], tbl_hold[, x])
}

# port ----

#' @title Cluster around latents
#' @param cor_mat correlation matrix
#' @export
pca_hclust <- function(cor_mat) {
  p <- princomp(covmat = cor_mat)
  meas <- diag(p$sdev) %*% t(p$loadings[,])
  dist_res <- dist(t(meas), method = 'euclidean')
  hclust(dist_res)
}


#' @title Calculate risk cluster weights
#' @param hc tree from hclust output
#' @param vol volatility of assets used in hc calculation
#' @param k number of clusters
#' @export
risk_cluster_wgt <- function(hc, vol, k = 2) {

  n_assets <- max(hc$order)
  memb <- cutree(hc, k)
  xcor <- diag(1, n_assets, n_assets)
  for (i in 1:k) {
    xcor[memb == i, memb == i] <- 1
  }
  vol <- matrix(vol, ncol = 1)
  xcov <- vol %*% t(vol) * xcor
  mu_vec <- vol * 0.25
  cov_inv <- MASS::ginv(xcov)
  (cov_inv %*% mu_vec) /
    (matrix(1, ncol = length(hc$order), nrow = 1) %*% cov_inv %*% mu_vec)[1]
}


# MC sim ----

#' @export
boot_strap <- function(x, n) {
  is_vec <- is.null(dim(x))
  if (is_vec) {
    n_row <- length(x)
  } else {
    n_row <- nrow(x)
  }
  rand_row <- round(runif(n, 1, n_row), 0)
  if (is_vec) {
    return(x[rand_row])
  } else {
    return(x[rand_row, ])
  }
}


#' @export
block_boot_strap <- function(x, n, block) {
  x <- as.vector(x)
  boot_length <- ceiling(n / block)
  boot_vec <- rep(NA, block * boot_length)
  rand_row <- round(runif(boot_length, 0, length(x) - block + 1), 0)
  j <- 1
  for (i in 1:boot_length) {
    rand_block <- rand_row[i]:(rand_row[i] + (block - 1))
    boot_vec[j:(j + (block - 1))] <- x[rand_block]
    j <- j + block
  }
  boot_vec[1:n]
}



# find n pcs ----

#' @export
sig_group_sim <- function(ret) {

  xcor <- cor(ret)
  lamda <- eigen(xcor)$values

  noise_lamda <- matrix(nrow = 1000, ncol = length(lamda))
  for (i in 1:1000) {
    noise_ret <- apply(ret, 2, sample, size = nrow(ret), replace = FALSE)
    noise_lamda[i, ] <- eigen(cor(noise_ret))$values
  }
  noise_95 <- apply(noise_lamda, 2, quantile, probs = 0.95)
  return(sum(lamda > noise_95))
}

# spline daily / monthly ret ----




#' @export
summary_stats <- function(fund, bench, rf, period) {

  n_funds <- ncol(fund)
  combo <- na.omit(cbind(fund, bench, rf))
  a <- freq_to_scaler(period)
  if (nrow(combo) > a) {
    geo_ret <- calc_geo_ret(combo[, 1:(n_funds+1)], period)
  } else {
    geo_ret <- apply(combo[, 1:(n_funds+1)] + 1, 2, prod) - 1
  }
  xcov <- cov(combo[, 1:(n_funds+1)])
  vol <- sqrt(diag(xcov) * a)
  xvar <- PerformanceAnalytics::VaR(combo[, 1:(n_funds+1)]) * sqrt(a / 12)
  up_capt <- calc_up_capture(combo[, 1:(n_funds+1)], combo[, n_funds + 1], period)
  down_capt <- calc_down_capture(combo[, 1:(n_funds+1)], combo[, n_funds + 1], period)
  xbeta <- xcov[, n_funds + 1] / xcov[n_funds + 1, n_funds + 1]
  ar <- calc_geo_ret(combo[, 1:(n_funds+1)] - combo[, rep(n_funds + 1, n_funds+1)],
                     period)
  acov <- cov(combo[, 1:(n_funds+1)] - combo[, rep(n_funds + 1, n_funds+1)])
  te <- sqrt(diag(acov) * a)
  ir <- ar / te
  rf_geo <- calc_geo_ret(combo[, n_funds + 2], period)
  sharpe <- (geo_ret - rf_geo) / vol
  sortino <- calc_sortino_ratio(combo[, 1:(n_funds + 1)], combo[, n_funds + 2],
                                period)
  dd <- calc_drawdown(combo[, 1:(n_funds + 1)])
  maxdd <- apply(dd, 2, min)
  calmar <- geo_ret / -maxdd

  df <- data.frame(
    Geo.Ret = f_percent(geo_ret, 2),
    Volatility = f_percent(vol, 2),
    Beta = f_num(xbeta, 2),
    VaR.Month.95 = f_percent(as.numeric(xvar), 2),
    Max.Drawdown = f_percent(maxdd, 2),
    Sharpe.Ratio = f_num(sharpe, 2),
    Sortino.Ratio = f_num(sortino, 2),
    Calmar = f_num(calmar, 2),
    TE = f_percent(te, 2),
    Info.Ratio = f_num(ir, 2),
    Up.Capture = f_percent(up_capt, 2),
    Down.Capture = f_percent(down_capt, 2)
  )
  df <- t(df)
  colnames(df) <- c(colnames(fund), colnames(bench))
  return(df)
}

# roll ----

