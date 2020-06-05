#' Correlation Coefficient
#'
#'Provides magnitude-based inferences upon given \emph{r} value and sample size. Unlike the old functions, this follows the hypothesis testing guidelines set forth by
#'
#'@param r Pearson correlation coefficient
#'@param n sample size (number of pairs)
#'@param conf.level  Confidence interval level. Defaults to \code{0.95}
#'@param low_eqbound Lower equivalence bounds (e.g., -0.1) expressed in raw units
#'@param high_eqbound Upper equivalence bounds (e.g., 0.1) expressed in raw units
#'@param mech_decisions List containing options for mechanistic inference. Options include strong_alpha, mod_alpha, and weak_alpha.
#'@param plot (optional) logical indicator specifying to print associated plot. Defaults to \code{FALSE}
#'@details This function back calculates multiple one-tailed t-tests in order to peform equivalence and minimal effects tests for a correlation. All testing and confidence intervals are calculated through Fisher's z-transformation. Results will differ from the stats cor.test function which utilizes a t-transformation.
#'@examples mbi_corr_rev(.40, 25, 0.95)
#'@importFrom stats qnorm pnorm pt
#'@importFrom pbmcapply pbmclapply
#'@importFrom concurve curve_mean ggcurve
#'@importFrom ggplot2 geom_vline labs xlab
#'@export

mbi_corr_rev <- function(r, n, conf.level = .95,
                         low_eqbound = -.1,
                         high_eqbound = .1,
                         mech_decisions = list(
                           strong_alpha = .005,
                           moderate_alpha = .05,
                           weak_alpha = .25),
                         plot = mbir_options("plot"),
                         verbose = mbir_options("verbose")) {
  if (is.character(r) == TRUE || is.factor(r) == TRUE || is.character(n) ==
      TRUE || is.factor(n) == TRUE) {
    error <- "Sorry, data must be numeric or integer values."
    stop(error)
  }
  if (n < 4) {
    error <- "Sorry, not enough data."
    stop(error)
  }
  if (length(r) > 1) {
    error <- "Please enter only one effect size."
    stop(error)
  }

  if (r > 1 || r < -1) {
    error <- "Please double check. Correlation cannot surpass 1."
    stop(error)
  }

  if (abs(high_eqbound) >= 1) {
    error <- "Please double check. Smallest effect size of interest cannot surpass 1."
    stop(error)
  }

  if (low_eqbound <= -1 ) {
    error <- "Sorry, the smallest effect size of interest cannot be less than or equal to -1"
    stop(error)
  }

  if (low_eqbound > high_eqbound) {
    error = "low_eqbound must be lower than the high_eqbound"
    stop(error)
  }

  conf.range = 1 - (1 - conf.level) / 2

  #Z-transformation for the swc correlation
  #low_eq_z <- (0.5*log((1 + low_eqbound)/(1 - low_eqbound)))
  #high_eq_z <- (0.5*log((1 + high_eqbound)/(1 - high_eqbound)))

  z1 <- ((log((1 + r)/(1 - r))/2) - (log((1 + low_eqbound)/(1 -
                                                                low_eqbound))/2))/(sqrt(1/(n - 3)))
  z2 <- ((log((1 + r)/(1 - r))/2) - (log((1 + high_eqbound)/(1 -
                                                                 high_eqbound))/2))/(sqrt(1/(n - 3)))

  r_z = 0.5 * r * log((1 + r) / (1 - r))
  z_se = 1 / sqrt(n - 3)

  zLL <- (r_z) - qnorm(conf.range) * z_se
  zUL <- (r_z) + qnorm(conf.range) * z_se
  rLL <- (exp(1)^(2 * zLL) - 1)/(exp(1)^(2 * zLL) + 1)
  rUL <- (exp(1)^(2 * zUL) - 1)/(exp(1)^(2 * zUL) + 1)

  p1 <- 1 - pnorm(z1)
  p2 <- pnorm(z2)

  p_low_eq = p1
  p_low_met = 1 - p1

  p_high_eq = p2
  p_high_met = 1 - p2

  p_eq <- max(p1, p2)

  pttest <- 2 * (1 - pt(abs(r) * sqrt(n - 2)/sqrt(1 - abs(r)^2),
                        n - 2))

  if (p_eq < mech_decisions$weak_alpha) {
    #equivalence loop
    eq_pvals = c(p_low_eq, p_high_eq)
    p_eq = max(eq_pvals)
    if (p_eq < mech_decisions$strong_alpha) {
      conclusion = "Data is strongly compatible with equivalence"
    } else if (p_eq < mech_decisions$moderate_alpha) {
      conclusion = "Data is moderately compatible with equivalence"
    } else {conclusion = "Data is weakly compatible with equivalence"}
  } else if (p_high_met < mech_decisions$weak_alpha) {
    if (p_high_met < mech_decisions$strong_alpha) {
      conclusion = "Data is strongly compatible with a positive correlation"
    } else if (p_high_met < mech_decisions$strong_alpha) {
      conclusion = "Data is moderately compatible with a positive correlation"
    } else {conclusion = "Data is weakly compatible with a positive correlation"}

  } else if (p_low_met < mech_decisions$weak_alpha) {
    if (p_low_met < mech_decisions$strong_alpha) {
      conclusion = "Data is strongly compatible with a negative correlation"
    } else if (p_low_met < mech_decisions$strong_alpha) {
      conclusion = "Data is moderately compatible with a negative correlation"
    } else {conclusion = "Data is weakly compatible with a negative correlation"}

  } else {
    direction = ifelse(point > 0, "positive", "negative")
    direction = ifelse(point == 0, "" , direction)
    if (p_low_eq < p_low_met && p_high_eq < p_high_met) {
      eq_ambig = "equivalence"
    } else {
      eq_ambig = "non-equivalence"
    }
    conclusion = paste0("Inconclusive; ambiguous ", direction," correlation favoring ", eq_ambig)
  }

  result_table = data.frame(
    hypothesis = c(high_eqbound,low_eqbound),
    t.value = c(t_high, t_low),
    df = c(df1,df1),
    p.equivalence = c(p_high_eq, p_low_eq),
    p.MET = c(p_high_met, p_low_met))

  es_table = data.frame(
    r.type = "Pearson",
    r.est = r,
    r.LL = rLL,
    r.UL = rUL,
    conf.level = conf.level
  )

  steps = 10000
  intrvls <- (1:(steps)) / steps

  res_LL <- pbmclapply(intrvls, FUN = function(i) {

    i_conf.range = 1 - (1 - i) / 2

    zLL <- (r_z) - qnorm(i_conf.range) * z_se
    rLL <- (exp(1)^(2 * zLL) - 1)/(exp(1)^(2 * zLL) + 1)

  }, mc.cores = getOption("mc.cores", 1L))

  res_UL <- pbmclapply(intrvls, FUN = function(i) {

    i_conf.range = 1 - (1 - i) / 2

    zUL <- (r_z) + qnorm(i_conf.range) * z_se
    rUL <- (exp(1)^(2 * zUL) - 1)/(exp(1)^(2 * zUL) + 1)

  }, mc.cores = getOption("mc.cores", 1L))

  df <- data.frame(do.call(rbind, res_LL), do.call(rbind, res_UL))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
  df$intrvl.level <- intrvls
  df$cdf <- (abs(df$intrvl.level / 2)) + 0.5
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  class(df) <- c("data.frame", "concurve")
  densdf <- data.frame(c(df$lower.limit, df$upper.limit))
  colnames(densdf) <- "x"
  densdf <- head(densdf, -1)
  class(densdf) <- c("data.frame", "concurve")
  curve_vals = list(df,densdf)
  curve_plot = ggcurve(curve_vals[[1]], type = "c")

  curve_plot = curve_plot +
    geom_vline(xintercept = low_eqbound,
               alpha = .3,
               linetype = "dashed",
               color = "red") +
    geom_vline(xintercept = high_eqbound,
               alpha = .3,
               linetype = "dashed",
               color = "red") +
    labs(title = paste("Pearson Correlation Coefficient = ", round(point, digits = 4),
                       ", ", 100 * (conf.level),
                       "% CI [", round(rLL, digits = 4), "; ",
                       round(rUL, digits = 4), "] ", sep = ""),
         subtitle = paste("Equivalence bounds ", signif(low_eqbound),
                          " and ",
                          signif(high_eqbound), sep = ""))

  if (plot == TRUE) {
    curve_plot
  }

  if (verbose == TRUE) {
    conclusion
  }

  rval <- list(es_table = es_table,
               result_table = result_table,
               inference = conclusion,
               cons_plot = curve_plot)
}
