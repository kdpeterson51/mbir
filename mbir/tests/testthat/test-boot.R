#context("Do the boot_test and smd_test functions agree on significance detection.")
library(mbir)

databugs <- read.csv("https://raw.githubusercontent.com/jamovi/jamovi/master/examples/Bugs%20(Ryan%2C%20Wilde%20%26%20Crist%2C%202013).csv")
databugs <- na.omit(databugs)



test_that("bootstrap agrees on significance from smd_test", {
  
  eqp_test <- smd_test(x=databugs$LDLF, y=databugs$LDHF, paired=TRUE, auto=FALSE, plot= FALSE,swc=.6)
  
  eqp_boot <- boot_test(databugs$LDLF, databugs$LDHF)
  
  mbirp <- ifelse(eq2_test$p.value < 0.05, TRUE, FALSE)
  
  bootp <- ifelse(eqp_boot$b.LL < eqp_boot$med && eqp_boot$b.UL > eqp_boot$med, FALSE, TRUE)
  
  #Check smd_test p-value agree with boot_test null in relation to bootstrap confidence intervals
  expect_identical(mbirp, bootp)
  
})
