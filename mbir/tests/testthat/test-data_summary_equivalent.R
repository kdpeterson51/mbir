context("Do corr and corr_test or smd and smd_test functions give identical results")
library("mbir")

data <- read.csv("https://raw.githubusercontent.com/jasp-stats/jasp-desktop/development/Resources/Data%20Sets/Big%205%20(Dolan%2C%20Oort%2C%20Stoel%20%26%20Wicherts%2C%202009).csv", sep="")
#data <- read.csv("C:/Users/Daniel/Downloads/Big 5 (Dolan, Oort, Stoel & Wicherts, 2009).csv", sep="")
databugs <- read.csv("https://raw.githubusercontent.com/jamovi/jamovi/master/examples/Bugs%20(Ryan%2C%20Wilde%20%26%20Crist%2C%202013).csv")
databugs <- na.omit(databugs)

test_that("inference are identical using corr_test and corr", {
  eq1_test <- corr_test(x=data$Extraversion, y=data$Agreeableness,
                        auto=FALSE, plot= FALSE)

  eq1_corr <- corr(n = length(data$Extraversion),  r = eq1_test$corr.stat)



  #Check if p-values are equal
  expect_identical(eq1_test$inference, eq1_corr$inference)
  expect_identical(eq2_test$mbiPositive, eq2_smd$mbiPositive)
  expect_identical(eq2_test$mbiTrivial, eq2_smd$mbiTrivial)
  expect_identical(eq2_test$mbiNegative, eq2_smd$mbiNegative)


})

test_that("inference are identical using smd_test and smd", {
  eq2_test <- smd_test(x=databugs$LDLF, y=databugs$LDHF, paired=TRUE,
                        auto=FALSE,
                        plot= FALSE)

  eq2_smd <- smd(df = eq2_test$df,  es = eq2_test$d.stat,
                 p=eq2_test$p.value)



  #Check if p-values are equal
  expect_identical(eq2_test$inference, eq2_smd$Inference)
  expect_identical(eq2_test$mbiPositive, eq2_smd$mbiPositive)
  expect_identical(eq2_test$mbiTrivial, eq2_smd$mbiTrivial)
  expect_identical(eq2_test$mbiNegative, eq2_smd$mbiNegative)


})
