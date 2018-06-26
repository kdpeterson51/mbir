context("Do the corr_test and smd_test function calculate proper effect sizes")
library("mbir")
library("effsize")

data <- read.csv("https://raw.githubusercontent.com/jasp-stats/jasp-desktop/development/Resources/Data%20Sets/Big%205%20(Dolan%2C%20Oort%2C%20Stoel%20%26%20Wicherts%2C%202009).csv", sep="")
#data <- read.csv("C:/Users/Daniel/Downloads/Big 5 (Dolan, Oort, Stoel & Wicherts, 2009).csv", sep="")
databugs <- read.csv("https://raw.githubusercontent.com/jamovi/jamovi/master/examples/Bugs%20(Ryan%2C%20Wilde%20%26%20Crist%2C%202013).csv")
databugs <- na.omit(databugs)


test_that("correlation coefficient is correct from corr_test", {

  eq1_test <- corr_test(x=data$Extraversion, y=data$Agreeableness,
                        auto=FALSE, plot= FALSE)

  eq1_corr <- cor.test(data$Extraversion, data$Agreeableness)



  #Check if p-values are equal
  expect_identical(eq1_test$corr.stat, eq1_corr$estimate)



})

test_that("effect size is correct from smd_test", {

  eq2_test <- smd_test(x=databugs$LDLF, y=databugs$LDHF, paired=TRUE,
                                     auto=FALSE,
                                     plot= FALSE)

  eq2_ES <- cohen.d(databugs$LDLF, databugs$LDHF)



  #Check if p-values are equal
  expect_identical(eq2_test$d.stat, eq2_ES$estimate)



})
