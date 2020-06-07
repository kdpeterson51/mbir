#' MBI for Mean Difference Reverse Engineered from Summary Statistics
#'
#'Provides magnitude-based interpretation using summary statistic (mean and standard error or confidence limits). Plots are based upon the functions in the \code{(concurve)} R package.
#'
#' @param point The point estimate of the mean difference from the analysis. Ex: 1.20
#' @param se The standard error of the point estimate. Optional: either se or confidence limits (UL & LL) must be entered. Ex: se = 0.05
#' @param LL The lower confidence limit from an analysis. Optional: either se or confidence limits (UL & LL) must be entered Ex: LL = 1.0
#' @param UL The upper confidence limit from an analysis. Optional: either se or confidence limits (UL & LL) must be entered Ex: UL = 1.4
#' @param conf.level Confidence level of the interval estimate. Default is 95\%, Ex: conf.level = 0.95
#' @param n Sample size; vector of sample sizes for independent samples or number of pairs for paired samples.
#' @param paired logical indicator specifying if comparison is from paired \code{(TRUE)} or independent \code{(FALSE)} samples.
#' @param low_eqbound Lower equivalence bounds (e.g., -0.5) expressed in raw units
#' @param high_eqbound Upper equivalence bounds (e.g., 0.5) expressed in raw units
#' @param mech_decisions List containing options for mechanistic inference. Options include strong_alpha, mod_alpha, and weak_alpha.
#' @param clin_decisions List containing options for clinical inference. Options include the use_direction (the direction of the effect that is considered positive or good), harm_alpha and benefit_alpha.
#' @param inference The type of MBI hypothesis testing to be performed; options are clinical or mechanistic
#' @param plot logical indicator that when \code{(TRUE)} returns a consonance plot.
#' @param verbose logical indicator that when \code{(TRUE)} prints inference to the console.
#'@return Returns a consonance plot (cons_plot), results from the MBI associated t-tests (result_table), and the resulting MBI hypothesis testing inference (inference)
#'@details This function back calculates multiple one-tailed t-tests in order to peform equivalence and minimal effects tests.
#'@references Aisbett, J., Lakens, D., & Sainani, K. (2020, May 3). Magnitude Based Inference in Relation to One-sided Hypotheses Testing Procedures. https://doi.org/10.31236/osf.io/pn9s3
#'
#'@importFrom stats pt qt
#'@importFrom graphics abline title segments points
#'@importFrom concurve curve_rev ggcurve
#'@importFrom ggplot2 geom_vline labs
#'@export

mbi_mu_rev <- function(point,
                       LL = NULL, UL = NULL,
                       se = NULL,
                       n,
                       low_eqbound,
                       high_eqbound,
                       paired = TRUE,
                       conf.level = mbir_options("conf.level"),
                       mech_decisions = mbir_options("mech_decisions"),
                       clin_decisions = mbir_options("clin_decisions"),
                       inference = "mechanistic",
                       plot = mbir_options("plot"),
                       verbose = mbir_options("verbose")) {
  # Error messages
  if (is.numeric(point) != TRUE) {
    stop("Error: 'point' must be a numeric value")
  }
  if (!is.null(LL) && is.numeric(LL) != TRUE) {
    stop("Error: 'LL' must be a numeric value")
  }
  if (!is.null(UL) && is.numeric(UL) != TRUE) {
    stop("Error: 'UL' must be a numeric value")
  }
  if (!is.null(se) && is.numeric(se) != TRUE) {
    stop("Error: 'se' must be a numeric value")
  }

  if (is.null(se) && (is.null(LL) & is.null(UL))) {
    stop("se or UL & LL must be entered")
  }

  if (!is.null(se) && !(is.null(LL) & is.null(UL))) {
    stop("se or UL & LL must be entered")
  }

  if (low_eqbound > high_eqbound) {
    error = "low_eqbound must be lower than the high_eqbound"
    stop(error)
  }

  if (missing(low_eqbound) || missing(high_eqbound)) {
    error = "low_eqbound and high_eqbound must be provided"
    stop(error)
  }

  if (missing(n)) {
    error = "a sample size 'n' must be provided"
  }


  if (!all(n == round(n))) {
    error = "n must be a vector of the sample size (integers)"
  }

  if (conf.level <= 0 || conf.level >= 1) {
    error = "conf.level must be a value between 0 and 1"
    stop(error)
  }

  if (paired == TRUE && length(n) != 1) {
    error = "n must have a length of 1 with paired samples"
    stop(error)
  }

  if (inference != "clinical" && inference != "mechanistic") {
    error = "inference must be 'clincal' or 'mechanisitic'"
    stop(error)
  }


  # Set defaults for concurve functions
  type = "c" # create consonance plot
  measure = "default" # compares means

  #
  conf.level_two = conf.level + (1 - conf.level)/2
  conf_range = qnorm(conf.level_two)


  if (!is.null(se)) {
    LL = point - conf_range*se
    UL = point + conf_range*se
  }

  if (is.null(se)) {
    se <- (UL - LL) / (2*conf_range)
  }

  if (paired == TRUE) {
    df1 = n - 1
  }

  if (paired == F && length(n) == 1) {
    df1 = 2*n - 2
  }

  if (paired == F && length(n) == 2) {
    df1 = n[1] + n[2] - 1
  }

  t = point/se
  t_low = (point - low_eqbound)/se
  t_high = (point - high_eqbound)/se

  p_nil = 2*pt(-abs(t), df = df1)
  p_low_eq = pt(t_low, df = df1, lower.tail = F)
  p_high_eq = pt(t_high, df = df1, lower.tail = T)

  p_low_met = pt(t_low, df = df1, lower.tail = T)
  p_high_met = pt(t_high, df = df1, lower.tail = F)


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
                        hypothesis = c(high_eqbound,low_eqbound),
                        t.value = c(t_high, t_low),
                        df = c(df1,df1),
                        p.equivalence = c(p_high_eq, p_low_eq),
                        p.MET = c(p_high_met, p_low_met))

  curve_vals = curve_rev(point = point, se = se,
                         type = type,
                         measure = measure,
                         conf.level = conf.level,
                         steps = 10000,
                         table = TRUE)

  #conf_lines = c(1 - mech_decisions$weak_alpha*2,
  #               1 - mech_decisions$moderate_alpha*2,
  #               1 - mech_decisions$strong_alpha*2)

  curve_plot = ggcurve(curve_vals[[1]], type = "c", levels = conf_level)
  curve_plot = curve_plot +
    geom_vline(xintercept = low_eqbound,
               alpha = .3,
               linetype = "dashed",
               color = "red") +
    geom_vline(xintercept = high_eqbound,
               alpha = .3,
               linetype = "dashed",
               color = "red") +
    labs(title = paste("Mean difference = ", signif(point),
                       ", ", 100 * (conf.level),
                       "% CI [", signif(LL), "; ",
                       signif(UL), "] ", sep = ""),
         subtitle = paste("Equivalence bounds ", signif(low_eqbound),
                          " and ",
                          signif(high_eqbound), sep = ""))


  if (plot == TRUE) {
  curve_plot
  }

  if (verbose == TRUE) {
    print(inference)
    print(result_table)
  }

  rval <- list(cons_plot = curve_plot,
               inference = conclusion,
               result_table = result_table)
}


