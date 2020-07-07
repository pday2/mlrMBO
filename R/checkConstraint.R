#' @title checkConstraint.R
#' @description Validate constraints and clean up. Remove invalid entries.
#'              Set param values outside limits to lower or upper limit in 
#'              parameter set.
#'
#' @param ps - with class = ParamSet
#' @param cs - with class = ParamSet
#' @export csOrdered - with class = ParamSet
#
# input:
#   ps - parameter set
#   constraint set - a parameter set with constrained values for focus search
#
# output:
#   constraint - ordered constraint set
#
# Only implemented for focus search in infillOptFocus.R
#
# usage example
#     cs = makeParamSet(
#         makeIntegerParam("power", lower = 333, upper = 333),
#         makeIntegerParam("time", lower = 800, upper = 810),
#         makeDiscreteParam("gas", values = c("Nitrogen", "Argon")),
#         makeIntegerParam("pressure", lower = 0, upper = 1000)
#     )
# or via modifyParam in ParamHelpers
#     cs = modifyParam(ps, id="power", lower = 444, upper = 444)
    

checkConstraint = function(ps, cs = NULL) {
    psParamIDs = getParamIds(ps)
    i = 1
    for (par in cs[["pars"]]) {
        id = par[["id"]]
        # if a cs parameter is not in ps remove it from cs
        if (!(id %in% psParamIDs)) {
            cat("Warning:",id,"not in parameter set, ignoring constraint\n")
            cs[["pars"]][[id]] <- NULL
        } else {
            # for parameters of type discrete
            if (par[["type"]] == "discrete") {
                for (value in par$values) {
                    # if parameter value is not in ps ignore constraint and use ps values
                    if (!(value %in% ps[["pars"]][[id]][["values"]][[value]])) {
                        cat("Warning:",value,"not a valid value for",par$id,"\b, using values from parameter set\n")
                        cs[["pars"]][[id]][["values"]] <- ps[["pars"]][[id]][["values"]]
                    }
                }
            # for parameters of type integer or numeric
            } else if (par[["type"]] == "integer" || par[["type"]] == "numeric") {
                if (par[["lower"]] > par[["upper"]]) {
                    print("Warning: lower limit must be less than or equal to upper limit")
                } else {
                    # if lower is not NULL and within valid limits then set to new lower
                    if (!(is.null(par[["lower"]]))) {
                        if (class(par[["lower"]]) == "numeric") {
                            if (par[["lower"]] < ps[["pars"]][[id]][["lower"]] || 
                                par[["lower"]] > ps[["pars"]][[id]][["upper"]]) {
                                    cat("Warning:", par[["lower"]], "outside limits, setting to lower limit in parameter set\n")
                                    cs[["pars"]][[id]][["lower"]] <- ps[["pars"]][[id]][["lower"]]
                            }
                        } else {
                                print("Warning: integer and numeric parameters must have class(value) = numeric")
                        }
                    }
                        # if upper is not NULL and within valid limits then set to new upper
                    if (!(is.null(par[["lower"]]))) {
                        if (class(par[["lower"]]) == "numeric") {
                            if (par[["upper"]] < ps[["pars"]][[id]][["lower"]] || 
                                par[["upper"]] > ps[["pars"]][[id]][["upper"]]) {
                                    cat("Warning:", par[["upper"]], "outside limits, setting to upper limit in parameter set\n")
                                    cs[["pars"]][[id]][["upper"]] <- ps[["pars"]][[id]][["upper"]]
                            }
                        } else {
                            print("Warning: integer and numeric parameters must have class(value) = numeric")
                        }
                    }
                }
            # for other types of parameters (ie NOT int, num or dis)
            } else {
                print("Warning: only types of integer, numeric & discrete currently supported for constraint set")
            }
        }
        i=i+1
    }
    # if a parameter in ps is not in cs copy it to cs so that all ps params are present in cs
    csParamIDs = getParamIds(cs)
    for (psID in psParamIDs) {
        if (!(psID %in% csParamIDs)) {
            cs[["pars"]][[psID]] <- ps[["pars"]][[psID]]
        }
    }
    # put parameters in correct order to match that of ps
    csOrdered <- makeParamSet()
    for (psID in psParamIDs) {
        csOrdered[["pars"]][[psID]] <- cs[["pars"]][[psID]]
    }

    return(csOrdered)
}
