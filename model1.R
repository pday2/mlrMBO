#!/usr/bin/env Rscript

library(ggplot2)
library(BBmisc)
library(mlr)
library(mlr3)
library(Rcpp)
library("devtools")
# load local mlrMBO copy
print("MODEL1-------------------------------------------------------")
if (Sys.info()[1] == "Linux"){
  load_all("/home/muddy/Tresors/Documentsacer/_UWyo/mlrMBO_work/mlrMBO/R")
} else {
  load_all("C:/Users/peter/My Tresors/Documentsacer/_UWyo/mlrMBO_work/mlrMBO/R")
}

if (Sys.info()[1] == "Linux"){
  setwd("/home/muddy/Tresors/Documentsacer/_UWyo/mlrMBO")
} else {
  setwd("C:/Users/peter/My Tresors/Documentsacer/_UWyo/mlrMBO")
}

ps = makeParamSet(
  makeIntegerParam("pressure", lower = 10, upper = 100),
  makeIntegerParam("power", lower = 50, upper = 1500),
  makeIntegerParam("time", lower = 500, upper = 15000)
)

ctrl = makeMBOControl(y.name = "ratio")
ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 10, opt.focussearch.points = 10000, crit = makeMBOInfillCritEI())

data = read.csv("GO20_Full.csv")

opt.state = initSMBO(par.set = ps, design = data, control = ctrl, minimize = FALSE, noisy = TRUE)

cat("\nProposed parameters:\n")

# Constraint Set
cs = makeParamSet(
  makeIntegerParam("pressure", lower = 10, upper = 100),
  makeIntegerParam("power", lower = 150, upper = 150),
  makeIntegerParam("time", lower = 500, upper = 15000)
)

if (class(cs) != "ParamSet") {
  stopf("cs must be of class ParamSet")
}

prop = suppressWarnings({proposePoints(opt.state, cs)})
#prop = suppressWarnings({proposePoints(opt.state)})
#suppressWarnings({proposePoints(opt.state)})
print(prop$prop.points)
cat("Expected value (upper bound):\n")
cat(paste(prop$crit.components$mean, " (", prop$crit.components$mean + prop$crit.components$se, ")\n", sep = ""))
