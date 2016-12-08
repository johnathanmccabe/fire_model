



#' This function calculates the loss cost given a set of inputs
#'
#' @param
#' @return
#' @details
#' @examples
calculate_loss_cost <- function(tiv,
                                base_rate,
                                uplift_factor,
                                state_factor,
                                occupancy_factor,
                                construction_factor,
                                protection_class_factor,
                                sprinkler_factor){

  retVal <- tiv *
    base_rate *
    uplift_factor *
    state_factor *
    occupancy_factor *
    construction_factor *
    protection_class_factor *
    sprinkler_factor

  return(retVal)
}
