#' MBI for Independent or Dependent Groups Comparison
#'
#'Performs two-sample difference of means analysis to produce magnitude-based inferences hypothesis tests.
#'
#' @param x,y numeric vectors of data values
#' @param paired logical indicator specifying if \code{x} and \code{y} are paired \code{(TRUE)} or independent \code{(FALSE)}
#' @param var.equal (optional) if \code{auto = F}, logical indicator specifying if homogeneity of variance assumed. Defaults to \code{TRUE}
#' @param conf.level Confidence level of the interval estimate. Default is 95\%, Ex: conf.level = 0.
#' @param low_eqbound Lower equivalence bounds (e.g., -0.5) expressed in raw units
#' @param high_eqbound Upper equivalence bounds (e.g., 0.5) expressed in raw units
#' @param mech_decisions List containing options for mechanistic inference. Options include strong_alpha, mod_alpha, and weak_alpha.
#' @param clin_decisions List containing options for clinical inference. Options include the use_direction (the direction of the effect that is considered positive or good), harm_alpha and benefit_alpha.
#' @param inference The type of MBI hypothesis testing to be performed; options are clinical or mechanistic.
#' @param plot logical indicator that when \code{(TRUE)} returns a consonance plot.
#' @param verbose logical indicator that when \code{(TRUE)} prints inference to the console.
#' @param plot (optional) logical indicator specifying to print associated plot. Defaults to \code{FALSE}
#'@return Associated effect size measures (\emph{d}, \emph{r}, odds ratio) and respective confidence intervals based upon which statistical test(s) performed.
#'@details This function calculates multiple one-tailed t-tests in order to peform equivalence and minimal effects tests and interprets the data based on magnitude based inference hypothesis testing scheme.
#'@references Aisbett, J., Lakens, D., & Sainani, K. (2020, May 3). Magnitude Based Inference in Relation to One-sided Hypotheses Testing Procedures. https://doi.org/10.31236/osf.io/pn9s3
#'@importFrom stats shapiro.test var.test t.test lm residuals.lm
#'@importFrom graphics plot hist
#'@importFrom MOTE d.dep.t.diff.t d.ind.t.t
#'@importFrom concurve curve_mean ggcurve
#'@importFrom ggplot2 geom_vline labs
#'@export

mbi_mu <- function(x,y,
                   paired = TRUE,
                   var.equal = TRUE,
                   low_eqbound,
                   high_eqbound,
                   conf.level = .95,
                   mech_decisions = list(
                     strong_alpha = .005,
                     moderate_alpha = .05,
                     weak_alpha = .25),
                   clin_decisions = list(
                     use_direction = "positive",
                     benefit_alpha = 0.25,
                     harm_alpha = .005
                   ),
                   inference = "mechanistic",
                   plot = mbir_options("plot"),
                   verbose = mbir_options("verbose")){

  if (is.character(x) == TRUE ||
      is.factor(x) == TRUE || is.character(y) ==
      TRUE || is.factor(y) == TRUE) {
    error <- "Sorry, data must be numeric or integer values."
    stop(error)
  }
  if (length(x) < 3 || length(y) < 3) {
    error <- "Sorry, not enough data."
    stop(error)
  }


  if (length(x) != length(y)) {
    max.len <- max(length(x), length(y))
    x <- c(x, rep(NA, max.len - length(x)))
    y <- c(y, rep(NA, max.len - length(y)))
  }

  if (low_eqbound > high_eqbound) {
    error = "low_eqbound must be lower than the high_eqbound"
    stop(error)
  }

  if (missing(low_eqbound) || missing(high_eqbound)) {
    error = "low_eqbound and high_eqbound must be provided"
    stop(error)
  }

  if (conf.level <= 0 || conf.level >= 1) {
    error = "conf.level must be a value between 0 and 1"
    stop(error)
  }

  dat = data.frame(x,y)

  NHST_t = t.test(x,y,
                  alternative = "two.sided",
                  mu = 0,
                  var.equal = var.equal,
                  paired = paired,
                  conf.level = conf.level)

  t_low_eq = t.test(x,y,
                    alternative = "greater",
                    mu = low_eqbound,
                    var.equal = var.equal,
                    paired = paired,
                    conf.level = conf.level)
  p_low_eq = t_low_eq$p.value

  t_low_met = t.test(x,y,
                    alternative = "less",
                    mu = low_eqbound,
                    var.equal = var.equal,
                    paired = paired,
                    conf.level = conf.level)
  p_low_met = t_low_met$p.value

  t_high_eq = t.test(x,y,
                    alternative = "less",
                    mu = high_eqbound,
                    var.equal = var.equal,
                    paired = paired,
                    conf.level = conf.level)
  p_high_eq = t_high_eq$p.value

  t_high_met = t.test(x,y,
                     alternative = "greater",
                     mu = high_eqbound,
                     var.equal = var.equal,
                     paired = paired,
                     conf.level = conf.level)
  p_high_met = t_high_met$p.value

  if (paired == TRUE) {
    dat$diff = dat$x - dat$y
    SMD = d.dep.t.diff.t(
      t = NHST_t$statistic,
      n = sum(!is.na(dat$diff)),
      a = (1 - conf.level)
    )
    mod = lm(diff ~ 1, data = dat)
    point = unname(coef(mod)[1])
    plot_outlier = plot(mod, which = 4)
    plot_QQ = plot(mod, which = 2)
    plot_resid = hist(residuals.lm(mod), freq = FALSE,
                      main = "Distribution of Residuals",
                      xlab = "Residuals")
    SMD_type = "Cohen's d[z]"

  } else {
    df_x = data.frame(val = x,grp = "0")
    df_y = data.frame(val = y,grp = "1")
    dat = rbind(df_x,df_y)
    dat$grp = as.factor(dat$grp)
    SMD = d.ind.t.t(
      t = NHST_t$statistic,
      n1 = sum(!is.na(x)),
      n2 = sum(!is.na(y)),
      a = (1 - conf.level)
    )
    mod = lm(val ~ grp, data = dat)
    point = unname(coef(mod)[2])
    plot_outlier = plot(mod, which = 4)
    plot_QQ = plot(mod, which = 2)
    plot_resid = hist(residuals.lm(mod), freq = FALSE,
                      main = "Distribution of Residuals",
                      xlab = "Residuals")
    SMD_type = "Cohen's d[s]"
  }

  #inference
  if (inference == "clinical") {
    if (clin_decisions$use_direction == "positive") {
      p_harm = p_low_eq
      p_benefit = p_high_met
    } else {
      p_harm = p_low_eq
      p_benefit = p_high_met
    }

    if (p_harm < clin_decisions$harm_alpha) {
      clin_harm = FALSE
    } else {
      clin_harm = TRUE
    }

    if (p_benefit < clin_decisions$benefit_alpha) {
      clin_minben = TRUE
    } else {
      clin_minben = FALSE
    }

    if (clin_harm == TRUE) {
      conclusion = "Do not use; Cannot rule out harm"
    } else if (clin_harm == FALSE && clin_minben == FALSE) {
      conclusion = "Possibly use; data incompatible with harm but not compatible with benefit"
    } else if (clin_harm == FALSE && clin_minben == TRUE) {
      conclusion = "Use; data incompatible with harm and compatible with benefit"
    }


  }

  if (inference == "mechanistic") {
    if (p_low_eq < mech_decisions$weak_alpha && p_high_eq < mech_decisions$weak_alpha) {
      #equivalence loop
      eq_pvals = c(p_low_eq, p_high_eq)
      highest_eq_pval = max(eq_pvals)
      if (highest_eq_pval < mech_decisions$strong_alpha) {
        conclusion = "Data is strongly compatible with equivalence"
      } else if (highest_eq_pval < mech_decisions$moderate_alpha) {
        conclusion = "Data is moderately compatible with equivalence"
      } else {conclusion = "Data is weakly compatible with equivalence"}
    } else if (p_high_met < mech_decisions$weak_alpha) {
      if (p_high_met < mech_decisions$strong_alpha) {
        conclusion = "Data is strongly compatible with a positive effect"
      } else if (p_high_met < mech_decisions$strong_alpha) {
        conclusion = "Data is moderately compatible with a positive effect"
      } else {conclusion = "Data is weakly compatible with a positive effect"}

    } else if (p_low_met < mech_decisions$weak_alpha) {
      if (p_low_met < mech_decisions$strong_alpha) {
        conclusion = "Data is strongly compatible with a negative effect"
      } else if (p_low_met < mech_decisions$strong_alpha) {
        conclusion = "Data is moderately compatible with a negative effect"
      } else {conclusion = "Data is weakly compatible with a negative effect"}

    } else {
      direction = ifelse(point > 0, "positive", "negative")
      direction = ifelse(point == 0, "" , direction)
      if (p_low_eq < p_low_met && p_high_eq < p_high_met) {
        eq_ambig = "equivalence"
      } else {
        eq_ambig = "non-equivalence"
      }
      conclusion = paste0("Inconclusive; ambiguous ", direction," effect favoring ", eq_ambig)
    }

  }

  result_table = data.frame(
    hypothesis = c(high_eqbound, low_eqbound),
    t.value = c(t_high_eq$statistic, t_low_eq$statistic),
    df = c(t_high_eq$parameter, t_low_eq$parameter),
    p.equivalence = c(p_high_eq, p_low_eq),
    p.MET = c(p_high_met, p_low_met)
  )

  es_table = data.frame(
    mu.diff = point,
    mu.LL = NHST_t$conf.int[1],
    mu.UL = NHST_t$conf.int[2],
    SMD_type = SMD_type,
    SMD = SMD$d,
    SMD.LL = SMD$dlow,
    SMD.UL = SMD$dhigh,
    conf.level = conf.level
  )

  #Plot with concurve

  curve_vals = curve_mean(x,y,data = dat,
                          paired = paired,
                          method = "default")

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
    labs(title = paste("Mean difference = ", round(point, digits = 3),
                       ", ", 100 * (conf.level),
                       "% CI [", signif(LL), "; ",
                       (UL), "] ", sep = ""),
         subtitle = paste("Equivalence bounds ", signif(low_eqbound),
                          " and ",
                          signif(high_eqbound), sep = ""))

  if (plot == TRUE) {
    curve_plot
  }

  if (verbose == TRUE) {
    print(inference)
    print(result_table)
    print(es_table)
  }

  rval = list(result_table = result_table,
              es_table = es_table,
              inference = conclusion,
              cons_plot = curve_plot,
              plot_outlier = plot_outlier,
              plot_QQ = plot_QQ,
              plot_resid = plot_resid)

}
