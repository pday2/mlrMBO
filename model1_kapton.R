#!/usr/bin/env Rscript

library(ggplot2)
library(BBmisc)
library(mlr)
library(mlr3)
library(Rcpp)
library("devtools")

# load local mlrMBO copy
if (Sys.info()[1] == "Linux"){
  load_all("/home/muddy/Tresors/Documentsacer/_UWyo/mlrMBO/R")
} else {
  load_all("C:/Users/peter/My Tresors/Documentsacer/_UWyo/mlrMBO/R")
}

if (Sys.info()[1] == "Linux"){
  setwd("/home/muddy/Tresors/Documentsacer/_UWyo/mlrMBO_work")
} else {
  setwd("C:/Users/peter/My Tresors/Documentsacer/_UWyo/mlrMBO_work")
}

ps = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
  makeIntegerParam("pressure", lower = 20, upper = 1000)
)

ctrl = makeMBOControl(y.name = "ratio")
ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEI())
ctrl = setMBOControlTermination(ctrl, iters = 50)

data = read.csv("kapton.csv")

opt.state = initSMBO(par.set = ps, design = data, control = ctrl, minimize = FALSE, noisy = TRUE)

cat("\nProposed parameters:\n")

# start from ps and constrain power to one value
cs <- modifyParam(ps, id="power", lower = 444, upper = 444)
prop = suppressWarnings({proposePoints(opt.state, cs)})
print(prop$prop.points)
cat("Expected value (upper bound):\n")
cat(paste(prop$crit.components$mean, " (", prop$crit.components$mean + prop$crit.components$se, ")\n", sep = ""))
cat("\n")

# start from cs and constrain gas to Air
cs2 <- modifyParam(ps, cs, id="gas", lower = "Air")
prop = suppressWarnings({proposePoints(opt.state, cs2)})
print(prop$prop.points)
cat("Expected value (upper bound):\n")
cat(paste(prop$crit.components$mean, " (", prop$crit.components$mean + prop$crit.components$se, ")\n", sep = ""))
cat("\n")

# start from ps again and constrain pressure to a range
cs <- modifyParam(ps, id="pressure", lower = 55, upper = 88)
prop = suppressWarnings({proposePoints(opt.state, cs)})
print(prop$prop.points)
cat("Expected value (upper bound):\n")
cat(paste(prop$crit.components$mean, " (", prop$crit.components$mean + prop$crit.components$se, ")\n", sep = ""))
cat("\n")
