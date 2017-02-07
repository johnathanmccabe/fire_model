

fire_model <- function(curve, deductible, excess, limit){

  retVal <- list(curve = curve,
                 deductible = deductible,
                 excess = excess,
                 limit = limit)

  class(retVal) <- "fire_model"

  return(retVal)
}


# predict.fire_model

