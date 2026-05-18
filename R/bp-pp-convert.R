bp_pp_convert <- function(bp, ttm_margins, ttm_shares, tls_margins) {
  pp <- (bp - ttm_shares * sum(ttm_margins * bp)) * (1 + tls_margins) * (1 + ttm_margins)
  return(pp)
}