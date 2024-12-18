# splines ----
#' @export
monthly_spline <- function(m_ret, q_ret, thresh = 0.00001) {
  q_dt <- zoo::index(q_ret)
  if (floor_date(q_dt)[1] < zoo::index(m_ret)[1]) {
    warning("quarterly time-series starts before monthly")
    q_dt <- q_dt[q_dt > zoo::index(m_ret)[1]]
    if (length(q_dt) == 0) {
      stop("no overlapping dates found")
    }
  }
  m_adj <- m_ret
  for (i in 1:length(q_dt)) {
    q_start <- floor_date(q_dt[i], "quarters")
    rng <- paste0(q_start, "/", q_dt[i])
    m_ret_i <- m_ret[rng]
    q_ret_i <- q_ret[q_dt[i]]
    lx <- -0.9
    ux <- 0.95
    for (j in 1:1000) {
      mx <- (lx + ux) / 2
      if (abs(test_diff(m_ret_i + mx, q_ret_i)) < thresh) {
        break
      }
      if (test_diff(m_ret_i + mx, q_ret_i) > 0) {
        ux <- mx
      } else {
        lx <- mx
      }
    }
    m_adj[rng] <- m_ret[rng] + mx
  }
  return(m_adj)
}

test_diff <- function(a, b) {
  (prod(a+1)-1) - b
}
