# unexported bootstrap functions

mu_ind = function(data, indices){
  d <- data[indices,] # allows boot to select sample
  t_NHST = t.test(data$x,data$y)
}

#Bootstrap general descriptives
bs <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample

  diff.mean <- mean(d$Diffperf)
  sl.mean <- mean(d$SLperf)
  alt.mean <- mean(d$ALTperf)
  cent.diff <- (diff.mean/sl.mean)*100

  diff.sd <- sd(d$Diffperf)
  sl.sd <- sd(d$SLperf)
  alt.sd <- sd(d$ALTperf)

  cor.prepost <- cor(d$ALTperf,d$SLperf)
  robust.cor <- robust.cor1$cor

  g <- (mean(d$Diffperf) / sd(d$Diffperf)) * (1 - (3 / (4 * (nrow(d) - 1) - 1)))

  trim.diff <- psych::winsor(d$Diffperf, trim = 0.1)

  akp <- (0.642*(mean(trim.diff)/sd(trim.diff)))

  result <- setNames(c(sl.mean,alt.mean, diff.mean, cent.diff,
                       sl.sd, alt.sd ,diff.sd,
                       cor.prepost,robust.cor,
                       g,akp),
                     c("SL.mean", "ALT.mean", "Diff.mean", "cent.diff",
                       "SL.sd", "ALT.sd", "Diff.sd",
                       "cor.prepost","robust.cor",
                       "Hedges","AKP.d"))

  return(result)
}
#Calculate CI for descriptives
bs.ci <- function(boots,types){

  boots.ci <- data.frame(
    conf = NA,
    order1 = NA,
    order2 = NA,
    l.ci = NA,
    h.ci = NA
  )

  boots.ci1 <- boot.ci(boots, type = types, index = 1)
  boots.ci[1,] <- boots.ci1$bca

  boots.ci2 <- boot.ci(boots, type = types, index = 2)
  boots.ci[2,] <- boots.ci2$bca

  boots.ci3 <- boot.ci(boots, type = types, index = 3)
  boots.ci[3,] <- boots.ci3$bca

  boots.ci4 <- boot.ci(boots, type = types, index = 4)
  boots.ci[4,] <- boots.ci4$bca

  boots.ci5 <- boot.ci(boots, type = types, index = 5)
  boots.ci[5,] <- boots.ci5$bca

  boots.ci6 <- boot.ci(boots, type = types, index = 6)
  boots.ci[6,] <- boots.ci6$bca

  boots.ci7 <- boot.ci(boots, type = types, index = 7)
  boots.ci[7,] <- boots.ci7$bca

  boots.ci8 <- boot.ci(boots, type = types, index = 8)
  boots.ci[8,] <- boots.ci8$bca

  boots.ci9 <- boot.ci(boots, type = types, index = 9)
  boots.ci[9,] <- boots.ci9$bca

  boots.ci10 <- boot.ci(boots, type = types, index = 10)
  boots.ci[10,] <- boots.ci10$bca

  boots.ci10 <- boot.ci(boots, type = types, index = 11)
  boots.ci[11,] <- boots.ci10$bca

  for(i in 1:11) {
    boots.ci$se[i] <- sd(boots$t[,i])
    boots.ci$bias[i] <- mean(boots$t[, i]) - unname(boots$t0[i])
    boots.ci$PercentBias[i] <- (mean(boots$t[, i]) - unname(boots$t0[i]))/mean(boots$t[, i])
  }
  boots.ci$estimate <- boots$t0
  boots.ci$measure <- names(boots$t0)

  boots.ci <- boots.ci[,10:1]

  return(boots.ci)
}
