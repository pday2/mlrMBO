library(testthat)
context("infillOptFocus")

test_that("infillOptFocus properly constrains parameters", {
    library(mlr)
    library(mlr3)
    
    data = read.csv(text = "power,time,gas,pressure,ratio
    1004,10624,Argon,660,2.65884223
    1105,12381,Air,860,0.12
    1108,9828,Nitrogen,820,0.953560013")

    ps = makeParamSet(
    makeIntegerParam("power", lower = 10, upper = 5555),
    makeIntegerParam("time", lower = 500, upper = 20210),
    makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
    makeIntegerParam("pressure", lower = 10, upper = 1000)
    )

    ctrl = makeMBOControl(y.name = "ratio")
    ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEI())
    ctrl = setMBOControlTermination(ctrl, iters = 50)

    opt.state = initSMBO(par.set = ps, design = data, control = ctrl, minimize = FALSE, noisy = TRUE)

    # Normal operation, modify limits of ps, set power to 1500
    c1 = modifyParam(ps, id="power", lower=1500, upper=1500)
    prop1 = suppressWarnings({proposePoints(opt.state, c1)})
    expect_equal(prop1$prop.points$power, 1500)

    # Normal operation, modify limits of ps, set time to 2112
    c2 = modifyParam(ps, id="time", lower=2112, upper=2112)
    prop2 = suppressWarnings({proposePoints(opt.state, c2)})
    expect_equal(prop2$prop.points$time, 2112)

    # Normal operation, modify limits of ps
    c3 = modifyParam(ps, id="gas", lower="Nitrogen")
    prop3 = suppressWarnings({proposePoints(opt.state, c3)})
    expect_equal(prop3$prop.points$gas, as.factor("Nitrogen"))

    # Normal operation, modify limits of ps, set range of pressure to 50-100
    c4 = modifyParam(ps, id="pressure", lower=50, upper=100)
    prop4 = suppressWarnings({proposePoints(opt.state, c4)})
    expect_gte(prop4$prop.points$pressure, 50)
    expect_lte(prop4$prop.points$pressure, 100)
    
    #Normal operation, modify limits of ps, change two parameters
    c5 = modifyParam(ps, id="pressure", lower=550, upper=550)
    c5 = modifyParam(c5, id="power", lower=760, upper=770)
    prop5 = suppressWarnings({proposePoints(opt.state, c5)})
    expect_equal(prop5$prop.points$pressure, 550)
    expect_gte(prop5$prop.points$power, 760)
    expect_lte(prop5$prop.points$power, 770)
    

})
