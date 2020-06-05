#'Magnitude Based Inference for Differences in Two Correlations
#'
#'Provides statistical inference upon the difference between two \emph{independent} correlations
#'
#'@param r1 correlation of group 1
#'@param n1 sample size of group 1
#'@param r2 correlation of group 2
#'@param n2 sample size of group 2
#'@param mech_decisions List containing options for mechanistic inference. Options include strong_alpha, mod_alpha, and weak_alpha.
#'@param conf.level (optional) confidence level of the interval. Defaults to \code{0.95}
#'@param plot (optional) logical indicator specifying to print associated plot. Defaults to \code{FALSE}
#'@details For more options in calculating the differences in correlation coefficients see the \code{cocor} R package and their associated website <http://comparingcorrelations.org/>
#'@references Zou GY. (2007). Toward using confidence intervals to compare correlations. \emph{Psychological Methods}, 12, 399-413.
#'Diedenhofen, B. & Musch, J. (2015). cocor: A Comprehensive Solution for the Statistical Comparison of Correlations. PLoS ONE, 10(4): e0121945. doi: 10.1371/journal.pone.0121945 Available: http://dx.doi.org/10.1371/journal.pone.0121945
#'@examples corr_diff(r1 = 0.20, n1 = 71, r2 = 0.55, n2 = 46)
#'@importFrom stats qnorm
#'@importFrom concurve ggcurve
#'@importFrom ggplot2 geom_vline labs
#'@importFrom cocor cocor.indep.groups
#'@export

corr_diff <- function(r1, n1, r2, n2,
                      mech_decisions = list(
                        strong_alpha = .005,
                        moderate_alpha = .05,
                        weak_alpha = .25),
                      conf.level = .95,
                      plot=mbir_options("plot"),
                      verbose = mbir_options("verbose"))
{
  if (is.character(r1) == TRUE || is.factor(r1) == TRUE ||
      is.character(n1) == TRUE || is.factor(n1) == TRUE) {
    error <- "Sorry, data must be numeric or integer values."
    stop(error)
  }
  if (is.character(r2) == TRUE || is.factor(r2) == TRUE ||
      is.character(n2) == TRUE || is.factor(n2) == TRUE) {
    error <- "Sorry, data must be numeric or integer values."
    stop(error)
  }
  if (length(r1) > 1 || length(n1) > 1 || length(r2) > 1 ||
      length(n2) > 1) {
    error <- "Please enter only one effect size."
    stop(error)
  }
  conf.int = conf.level

  steps = 10000
  intrvls <- (1:(steps)) / steps

  res_LL <- pbmclapply(intrvls, FUN = function(i) {
    #diff <- r2 - r1
    #zcrit <- abs(qnorm((1 - i)/2))
    #r1.z <- 0.5 * log((1 + r1)/(1 - r1))
    #r1.sd <- 1/sqrt(n1 - 3)
    #r1.ll <- r1.z - zcrit * r1.sd
    #r1.ul <- r1.z + zcrit * r1.sd
    #r2.z <- 0.5 * log((1 + r2)/(1 - r2))
    #r2.sd <- 1/sqrt(n2 - 3)
    #r2.ll <- r2.z - zcrit * r2.sd
    #r2.ul <- r2.z + zcrit * r2.sd

    #diff.LL <- diff - sqrt((r2 - r2.ll)^2 + (r1.ul - r1)^2)
    test_indcor = cocor.indep.groups(r1, r2, n1, n2,
                                     conf.level = i)
    LL = test_indcor@zou2007$conf.int[1]


  }, mc.cores = getOption("mc.cores", 1L))

  res_UL <- pbmclapply(intrvls, FUN = function(i) {
    #diff <- r2 - r1
    #zcrit <- abs(qnorm((1 - i)/2))
    #r1.z <- 0.5 * log((1 + r1)/(1 - r1))
    #r1.sd <- 1/sqrt(n1 - 3)
    #r1.ll <- r1.z - zcrit * r1.sd
    #r1.ul <- r1.z + zcrit * r1.sd
    #r2.z <- 0.5 * log((1 + r2)/(1 - r2))
    #r2.sd <- 1/sqrt(n2 - 3)
    #r2.ll <- r2.z - zcrit * r2.sd
    #r2.ul <- r2.z + zcrit * r2.sd
    test_indcor = cocor.indep.groups(r1, r2, n1, n2,
                                     conf.level = i)
    UL = test_indcor@zou2007$conf.int[2]

    #diff.UL <- diff + sqrt((r2.ul - r2)^2 + (r1 - r1.ll)^2)


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
  curve_plot = ggcurve(curve_vals[[1]], type = "c",
                       levels = conf.level)



  #diff <- r2 - r1
  #zcrit <- abs(qnorm((1 - conf.int)/2))
  #r1.z <- 0.5 * log((1 + r1)/(1 - r1))
  #r1.sd <- 1/sqrt(n1 - 3)
  #r1.ll <- r1.z - zcrit * r1.sd
  #r1.ul <- r1.z + zcrit * r1.sd
  #r2.z <- 0.5 * log((1 + r2)/(1 - r2))
  #r2.sd <- 1/sqrt(n2 - 3)
  #r2.ll <- r2.z - zcrit * r2.sd
  #r2.ul <- r2.z + zcrit * r2.sd
  #diff.UL <- diff + sqrt((r2.ul - r2)^2 + (r1 - r1.ll)^2)
  #diff.LL <- diff - sqrt((r2 - r2.ll)^2 + (r1.ul - r1)^2)
  #z.diff <- abs(r1.z - r2.z)
  #z.diff.sd <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
  #z <- z.diff/z.diff.sd
  #p <- 2 * (1 - pnorm(z))
  #dir <- ifelse(r2 > r1, ">", "<")
  level <- paste(as.character(100 * conf.int), "%", sep = "")

  #cat("   Test of Two Correlations:\n")
  #cat("   diff = ", diff, "\n", sep = "")
  #cat("   ", level, " CI ", "[", round(diff.LL, digits = 2),
  #    ", ", round(diff.UL, digits = 2), "]\n", sep = "")
  #cat("   p value = ", round(p, digits = 2), "\n\n", sep = "")
  #Save inference for list

  test_indcor = cocor.indep.groups(r1, r2, n1, n2,
                                   conf.level = conf.int)

  diff = test_indcor@diff
  diff.LL = test_indcor@zou2007$conf.int[1]
  diff.UL = test_indcor@zou2007$conf.int[2]
  p = test_indcor@fisher1925$p.value
  z = test_indcor@fisher1925$statistic

  curve_plot = curve_plot +
    labs(title = paste("Difference in Correlation = ", round(point, digits = 4),
                       ", ", 100 * (conf.level),
                       "% CI [", round(diff.LL, digits = 4), "; ",
                       round(diff.UL, digits = 4), "] ", sep = ""))

if (df[which(df$pvalue == mech_decisions$weak_alpha),]$lower.limit < 0 && df[which(df$pvalue == mech_decisions$weak_alpha),]$lower.limit > 0) {
    if (df[which(df$pvalue == mech_decisions$strong_alpha),]$lower.limit < 0 && df[which(df$pvalue == mech_decisions$strong_alpha),]$lower.limit > 0) {
      conclusion = paste("Inference: Strong Evidence Present, r2 ", dir, " r1", sep = "")
    } else if (df[which(df$pvalue == mech_decisions$moderate_alpha),]$lower.limit < 0 && df[which(df$pvalue == mech_decisions$moderate_alpha),]$lower.limit > 0) {
      conclusion = paste("Inference: Moderate Evidence Present, r2 ", dir, " r1", sep = "")
    } else if (df[which(df$pvalue == mech_decisions$weak_alpha),]$lower.limit < 0 && df[which(df$pvalue == mech_decisions$weak_alpha),]$lower.limit > 0) {
      conclusion = paste("Inference: Weak Evidence Present, r2 ", dir, " r1", sep = "")
    } } else {
      conclusion = paste("Inference: Lacking Evidence of Difference, r2 = r1", sep = "")
    }



  #Creates plots of MBI *Note will not print if normal=FALSE
  if (plot == TRUE) {
    curve_plot
    #plot(NA, ylim = c(0, 1), xlim = c(diff.LL -
    #                                    (diff.UL - diff.LL)/10,
    #                                  (diff.UL) + (diff.UL - diff.LL)/10), bty = "l", yaxt = "n", ylab = "",
    #     xlab = "Difference in Correlations")
    #graphics::points(x = diff, y = 0.5, pch = 15, cex = 2)
    #graphics::abline(v = 0, lty = 2, col = "grey")
    #graphics::segments(diff.LL, 0.5, diff.UL, 0.5, lwd = 3)
    #graphics::title(main = paste(
    #  "difference = ", round(diff, digits = 2), " \n  ",
    #  100 * (conf.int), "% CI [", round(diff.LL, digits = 2),
    #  ";", round(diff.UL, digits = 2), "] ", " \n  ", inference,
    #  sep = ""), cex.main = 1)
  }

  if (verbose = TRUE) {
    print(conclusion)
  }

  result_table = data.frame(
    r.diff = diff,
    diff.LL = diff.LL,
    diff.UL = diff.UL,
    Fisher.z = z,
    p.value = p,
    conf.level = conf.level
  )

  #Save list of output
  rval <- list(result_table = result_table,
               inference = conclusion,
               cons_plot = curve_plot)
}

