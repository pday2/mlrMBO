# Update location of mlrMBO and setwd for data
library(testthat)
context("checkConstraint")

test_that("checkConstraint returns expected constraints", {

    library("devtools")
    # load local mlrMBO copy
    if (Sys.info()[1] == "Linux"){
      load_all("/home/muddy/Tresors/Documentsacer/_UWyo/mlrMBO/mlrMBO/R")
    } else {
      load_all("C:/Users/peter/My Tresors/Documentsacer/_UWyo/mlrMBO/mlrMBO/R")
    }



    ps = makeParamSet(
    makeIntegerParam("power", lower = 10, upper = 5555),
    makeIntegerParam("time", lower = 500, upper = 20210),
    makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
    makeIntegerParam("pressure", lower = 0, upper = 1000)
    )
    
    # Normal operation, all parameters and limits in ps and within limits
    c1 = makeParamSet(
              makeIntegerParam("power", lower = 10, upper = 5555),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
              
    # Normal operation, modify parameter starting from ps (no prior constraint set)
    # set power lower to 543 and upper to 654, leave other params as they are
    c01 = modifyParam(ps, id="pressure", lower=543, upper=654)
    c01_result = makeParamSet(
                     makeIntegerParam("power", lower = 10, upper = 5555),
                     makeIntegerParam("time", lower = 500, upper = 20210),
                     makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
                     makeIntegerParam("pressure", lower = 543, upper = 654))
                     
    # Normal operation, modify parameter starting from c1
    # set gas value to Nitrogen, leave other parameters as they are
    c001 = modifyParam(ps, c1, id="power", lower=2222, upper=2222)
    c001_result = makeParamSet(
              makeIntegerParam("power", lower = 2222, upper = 2222),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
    
    # power upper limit above ps limit, set to ps upper limit = 5555
    c2 = makeParamSet(
              makeIntegerParam("power", lower = 10, upper = 888888),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
    
    # missing power parameter, set power to ps limits
    c3 = makeParamSet(
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
    
    # mising power parameter, power will be set to ps limits
    # gas has extra/invalid value "Vacuum" list will ignore that value
    c4 = makeParamSet(
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon", "Vacuum")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
    c4_result = makeParamSet(
              makeIntegerParam("power", lower = 10, upper = 5555),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
    
    # mising power parameter, power will be set to ps limits
    # pressure lower limit invalid set to ps limit
    c5 = makeParamSet(
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = -3, upper = 50))
    c5_result = makeParamSet(
              makeIntegerParam("power", lower = 10, upper = 5555),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = 0, upper = 50))
    
    # modify c1 set set power lower to 444, missing upper, set to limit from ps
    c6 = modifyParam(ps, c1, id="power", lower = 444)
    c6_result = makeParamSet(
              makeIntegerParam("power", lower = 444, upper = 5555),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
    
    # modify c6 set set power upper to 2112, missing lower, set to limit from ps
    c7 = modifyParam(ps, c6, id="power", upper = 2112)
    c7_result = makeParamSet(
              makeIntegerParam("power", lower = 10, upper = 2112),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Air", "Argon")),
              makeIntegerParam("pressure", lower = 50, upper = 50))
    
    # incorrect/invalid parameter name, ignore and keep c7 set as is
    c8 = modifyParam(ps, c7, id="powrrr", upper = 1221)
    
    # extra/inavlid parameter "pressurrr" - ignore
    # extra/invalid parameter gas2 - ignore
    # pressure missing, set to ps limits
    # gas missing, set to ps values
    # set time to lower=777, upper=789
    c9 = makeParamSet(
              makeIntegerParam("power", lower = 200, upper = 555),
              makeIntegerParam("time", lower = 777, upper = 789),
              makeDiscreteParam("gas2", values = c("Air", "Argon")),
              makeIntegerParam("pressurrr", lower = 500, upper = 500))
    c9_result = makeParamSet(
              makeIntegerParam("power", lower = 200, upper = 555),
              makeIntegerParam("time", lower = 777, upper = 789),
              makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
              makeIntegerParam("pressure", lower = 0, upper = 1000))
    
    # power missing upper limit, set to ps upper limits
    # set time to 777
    # set gas to Nitrogen
    # set pressure to 500
    c10 = makeParamSet(
              makeIntegerParam("power", lower = 500),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Nitrogen")),
              makeIntegerParam("pressure", lower = 500, upper = 500))
    c10_result = makeParamSet(
              makeIntegerParam("power", lower = 500, upper = 5555),
              makeIntegerParam("time", lower = 777, upper = 777),
              makeDiscreteParam("gas", values = c("Nitrogen")),
              makeIntegerParam("pressure", lower = 500, upper = 500))
    


    expect_identical(checkConstraint(ps, c1), c1)
    expect_identical(checkConstraint(ps, c01), c01_result)
    expect_identical(checkConstraint(ps, c001), c001_result)
    expect_identical(checkConstraint(ps, c2), c1)
    expect_identical(checkConstraint(ps, c3), c1)
    expect_identical(checkConstraint(ps, c4), c4_result)
    expect_identical(checkConstraint(ps, c5), c5_result)
    expect_identical(checkConstraint(ps, c6), c6_result)
    expect_identical(checkConstraint(ps, c7), c7_result)
    expect_identical(checkConstraint(ps, c8), c7_result)
    expect_identical(checkConstraint(ps, c9), c9_result)
    expect_identical(checkConstraint(ps, c10), c10_result)
    

})
