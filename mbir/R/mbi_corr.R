#' Magnitude Based Inference for a Correlation
#'
#'Provides magnitude-based inferences for the association between given data vectors. Evaluates normality assumption, performs either Pearson or Spearman correlation and subsequently estimates magnitude-based inferences.
#'
#'@param x,y numeric vectors of data values
#'@param conf.int (optional) confidence level of the interval. Defaults to \code{0.90}
#'@param auto (character) logical indicator specifying if user wants function to programmatically detect statistical procedures. Defaults to \code{TRUE}
#'@param method (character) if \code{auto = F}, logical indicator specifying which correlation to execute (\code{pearson, spearman, kendall}). Defaults to \code{"pearson"}.
#'@param swc (optional) number indicating smallest worthwhile change. Defaults to \code{0.1}
#'@param plot (optional) logical indicator specifying to print associated plot. Defaults to \code{FALSE}
#'@return Associated effect size measure, \emph{r}, and respective confidence intervals.
#'@details Refer to vignette for further information.
#'@examples a <- rnorm(25, 80, 35)
#'@examples b <- rnorm(25, 100, 35)
#'
#'@examples mbi_corr(a, b)
#'@importFrom stats qnorm sd na.omit cor.test
#'@importFrom concurve ggcurve
#'@importFrom pbmcapply pbmclapply
#'@importFrom ggplot2 geom_vline
#'@export

mbi_corr <- function(x, y,
                     conf.level = mbir_options("conf.level"),
                     method = "pearson",
                     low_eqbound = -.1,
                     high_eqbound = .1,
                     mech_decisions = mbir_options("mech_decisions"),
                     plot = mbir_options("plot")) {



  if (length(x) != length(y) || sum(is.na(x)) > 0 || sum(is.na(y)) > 0) {
    error <- "Sorry, data must be same length and complete cases."
    stop(error)
  }
  if (is.character(x) == TRUE || is.factor(x) == TRUE || is.character(y) == TRUE || is.factor(y) == TRUE) {
    error <- "Sorry, data must be numeric or integer values."
    stop(error)
  }
  if (length(x) < 4 || length(y) < 4) {
    error <- "Sorry, not enough data."
    stop(error)
  }

  #Variance and normality checks were removed.
  if (length(x) != length(y)) {
    error = "Length of data (x & y) are not equal. Check your data"
    stop(error)
  }
  full <- append(x, y)
  cor_val = cor.test(full$x,full$y,
                     alternative = "two.sided",
                     method = method,
                     conf.level = conf.level,
                     continuity = FALSE)
  r = cor_val$estimate
  z1 <- ((log((1 + r)/(1 - r))/2) - (log((1 + low_eqbound_r)/(1 -
                                                                low_eqbound_r))/2))/(sqrt(1/(n - 3)))
  z2 <- ((log((1 + r)/(1 - r))/2) - (log((1 + high_eqbound_r)/(1 -
                                                                 high_eqbound_r))/2))/(sqrt(1/(n - 3)))
  p1 <- 1 - pnorm(z1)
  p2 <- pnorm(z2)
  ptost <- max(p1, p2)

  p_low_eq = p1
  p_high_eq = p2

  p_high_met = 1 - p_high_eq
  p_low_met = 1 - p_low_eq

  steps = 10000
  intrvls <- (1:(steps)) / steps

  results <- pbmclapply(intrvls, FUN = function(i) {
    cor_loop = cor.test(full$x,full$y,
                       alternative = "two.sided",
                       method = method,
                       conf.level = i,
                       continuity = FALSE)
    cor_ci = cor_loop$conf.int

  }, mc.cores = getOption("mc.cores", 1L))
  #Seperate lists
  results_LL = list()
  results_UL = list()
  for (i in 1:length(results)) {
    results_LL[[i]] = results[[i]][1]
    results_UL[[i]] = results[[i]][2]
  }
  #Reform into data.frame
  df <- data.frame(do.call(rbind, results_LL), do.call(rbind, results_UL))
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

  #conf_lines = c(1 - mech_decisions$weak_alpha*2,
  #               1 - mech_decisions$moderate_alpha*2,
  #               1 - mech_decisions$strong_alpha*2)
  curve_plot = ggcurve(curve_vals[[1]], type = "c", levels = conf_level)

  if (p_low_eq < mech_decisions$weak_alpha &&
      p_high_eq < mech_decisions$weak_alpha) {
    #equivalence loop
    eq_pvals = c(p_low_eq, p_high_eq)
    highest_eq_pval = max(eq_pvals)
    if (highest_eq_pval < mech_decisions$strong_alpha) {
      conclusion = "Data is strongly compatible with equivalence"
    } else if (highest_eq_pval < mech_decisions$moderate_alpha) {
      conclusion = "Data is moderately compatible with equivalence"
    } else {
      conclusion = "Data is weakly compatible with equivalence"
    }
  } else if (p_high_met < mech_decisions$weak_alpha) {
    if (p_high_met < mech_decisions$strong_alpha) {
      conclusion = "Data is strongly compatible with a positive correlation"
    } else if (p_high_met < mech_decisions$strong_alpha) {
      conclusion = "Data is moderately compatible with a positive correlation"
    } else {
      conclusion = "Data is weakly compatible with a positive correlation"
    }

  } else if (p_low_met < mech_decisions$weak_alpha) {
    if (p_low_met < mech_decisions$strong_alpha) {
      conclusion = "Data is strongly compatible with a negative correlation"
    } else if (p_low_met < mech_decisions$strong_alpha) {
      conclusion = "Data is moderately compatible with a negative correlation"
    } else {
      conclusion = "Data is weakly compatible with a negative correlation"
    }

  } else {
    direction = ifelse(r > 0, "positive", "negative")
    direction = ifelse(r == 0, "" , direction)
    if (p_low_eq < p_low_met && p_high_eq < p_high_met) {
      eq_ambig = "equivalence"
    } else {
      eq_ambig = "non-equivalence"
    }
    conclusion = paste0("Inconclusive; ambiguous ",
                        direction,
                        " correlation favoring ",
                        eq_ambig)
  }

  #Creates plots of MBI *Note will not print if normal=FALSE
  if (plot == TRUE) {

  }

  #Save List of values
  rval <- list(result_table = result_table,
               inference = conclusion,
               cons_plot = curve_plot)
}
