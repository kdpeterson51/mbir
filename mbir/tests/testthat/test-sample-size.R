#context("Do the estiamtes from sample size functions agree with statistical test funcitons.")
library(mbir)

databugs <- read.csv("https://raw.githubusercontent.com/jamovi/jamovi/master/examples/Bugs%20(Ryan%2C%20Wilde%20%26%20Crist%2C%202013).csv")
databugs <- na.omit(databugs)



test_that("aipe_smd accurately estimates sufficient sample size for smd_test", {
  
  set.seed(0)
  
  ss_est <- aipe_smddd(moe = 0.8, paired = FALSE)
  
  samp1 <- sample(databugs$LDLF, ss_est$n); samp2 <- sample(databugs$LDHF, ss_est$n)
  
  eqs_test <- smd_test(x=samp1, y=samp2, paired=FALSE, auto=FALSE, plot= FALSE)
  
  # moe from smd_test
  moe <- (eqs_test$d.UL-eqs_test$d.LL)/2
  
  #Check smd_test  moe less than ss_est(moe = 0.8)
  expect_lt(moe, 0.8)
  
})

test_that("ss_smd accurately estimates sufficient sample size for smd_test", {
  
  set.seed(333)
  
  ss_est <- ss_smddd(exp = 8, con = 8, es = .2)
  
  samp1 <- sample(databugs$LDLF, 8); samp2 <- sample(databugs$LDHF, 8)
  
  eqs_test <- smd_test(x=samp1, y=samp2, paired=FALSE, auto=FALSE, plot= FALSE)
  
  #Check smd_test inference as unclear due to small n
  expect_that(eqs_test$Inference, matches("Unclear"))
  
})

test_that("ss_corr accurately estimates sufficient sample size for corr_test", {
  
  set.seed(333)
  
  ss_est <- ss_corr(n = 10, r = .2)
  
  samp1 <- sample(databugs$LDLF, 10); samp2 <- sample(databugs$LDHF, 10)
  
  eqs_test <- corr_test(x=samp1, y=samp2, auto=FALSE, plot= FALSE)
  
  #Check corr_test inference as unclear due to small n
  expect_that(eqs_test$inference, matches("Unclear"))
  
})

test_that("ss_odds accurately estimates sufficient sample size for smd_test", {
  
  set.seed(333)
  
  ss_est <- ss_oddsss(exp = 8, con = 8, or = 1.5)
  
  samp1 <- sample(databugs$LDLF, 8); samp2 <- sample(databugs$LDHF, 8)
  
  eqs_test <- smd_test(x=samp1, y=samp2, paired = FALSE, auto=FALSE, plot= FALSE)
  
  #Check corr_test inference as unclear due to small n
  expect_that(eqs_test$Inference, matches("Unclear"))
  
})

