
library("mbir")


df<-c(97.5,99.9,100.2,101,101.2,99.8)

test_that("Check if swc_ind regression statistics match known 'stats' package", {

  eq1_test <- swc_ind(x=df, swc=.1,type = "trend", ts=.5)

  eq1_lm <- stats::lm(df~c(1:length(df)))

  eq1_lm_sum <- summary(eq1_lm)

  expect_identical(eq1_test$slope, unname(round(eq1_lm$coefficients[2],3)))
  expect_identical(eq1_test$rsq, unname(round(eq1_lm_sum$adj.r.squared,3)))
  expect_identical(eq1_test$fstat, unname(round(eq1_lm_sum$fstatistic[1],3)))
})
