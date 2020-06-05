dz_paired = function(t, n, a = 0.05)
{
  if (missing(t)) {
    stop("Be sure to include your t-value from your dependent t-test.")
  }
  if (missing(n)) {
    stop("Be sure to include your sample size value n.")
  }
  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }
  d <- t/sqrt(n)
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 -
                                                         a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit/sqrt(n)
  dhigh <- ncpboth$Upper.Limit/sqrt(n)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2
  if (p < 0.001) {
    reportp = "< .001"
  }
  else {
    reportp = paste("= ", apa(p, 3, F), sep = "")
  }
  output = list(d = d, dlow = dlow, dhigh = dhigh, n = n,
                df = (n - 1), t = t, p = p, estimate = paste("$d_z$ = ",
                                                             apa(d, 2, T), ", ", (1 - a) * 100, "\\% CI [", apa(dlow,
                                                                                                                2, T), ", ", apa(dhigh, 2, T), "]", sep = ""),
                statistic = paste("$t$(", (n - 1), ") = ", apa(t, 2,
                                                               T), ", $p$ ", reportp, sep = ""))
  return(output)
}

ds_ind = function(t, n1, n2, a = 0.05)
{
  if (missing(t)) {
    stop("Be sure to include the t-value found from your t-test.")
  }
  if (missing(n1)) {
    stop("Be sure to include the sample size n1 for group 1.")
  }
  if (missing(n2)) {
    stop("Be sure to include the sample size n2 for group 2.")
  }
  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }
  d <- 2 * t/sqrt(n1 + n2 - 2)
  ncpboth <- conf.limits.nct(t, (n1 - 1 + n2 - 1), conf.level = (1 -
                                                                   a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit/sqrt(((n1 * n2)/(n1 + n2)))
  dhigh <- ncpboth$Upper.Limit/sqrt(((n1 * n2)/(n1 + n2)))
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2
  if (p < 0.001) {
    reportp = "< .001"
  }
  else {
    reportp = paste("= ", apa(p, 3, F), sep = "")
  }
  output = list(d = d, dlow = dlow, dhigh = dhigh, n1 = n1,
                n2 = n2, df = (n1 - 1 + n2 - 1), t = t, p = p, estimate = paste("$d_s$ = ",
                                                                                apa(d, 2, T), ", ", (1 - a) * 100, "\\% CI [", apa(dlow,
                                                                                                                                   2, T), ", ", apa(dhigh, 2, T), "]", sep = ""),
                statistic = paste("$t$ = (", (n1 - 1 + n2 - 1), ") = ",
                                  apa(t, 2, T), ", $p$ ", reportp, sep = ""))
  return(output)
}
