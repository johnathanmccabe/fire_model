
#' This function return the band number that each of the values lie within.
#' 1 is the interval above the highest threshold down to n+1 where n is the
#' number of threshold boundaries. Right inclusive.
#'
#' @param thresholds A list of the threshold values for interval boundaries
#' @param value A numeric vector
#' @return A vector containing the
#' @details
#' @examples
.get_interval <- function(thresholds, value) {

  if(!is.numeric(thresholds)){
    return(rep(NA, length(value)))
  } else {

    th <- thresholds[desc(thresholds)]

    output <- length(th) + 1 - findInterval(value, th)
    return(output)
  }
}


#' This function computes the data quality score for occupancy
#'
#' @param server The database server
#' @param edm The EDM
#' @param account The account name
#' @param thresholds
#' @return
#' @details The score is based on the proportion of TIV that has been
#' classified as ambiguous in RMS
#' @examples rms_occupancy_score(edm = "RMS_EDM_QTE_16_09",
#' account = "CL16_CC844E16A000_07",
#' thresholds = c(0.7,0.9,0.95))
rms_occupancy_score <- function(server = "GBLONTPD111", edm, account, thresholds){

  qry <- paste0("Exposure_Analysis..sp_EDM_ANALYSIS_s2",
                "'", edm,"',",
                "'", account, "'")

  df <- Faraday.Pricing::run_query_on_sql_server(query = qry,
                                                 server = server,
                                                 database = edm) %>%
    filter(PERIL == "Windstorm")


  value <- sum(df$VALUEAMT[df$OCC_AMBIGUOUS == 0]) / sum(df$VALUEAMT)

  return(.get_interval(thresholds, value))

}

#' This function computes the data quality score for construction
#'
#' @param server The database server
#' @param edm The EDM
#' @param account The account name
#' @param thresholds
#' @return
#' @details The score is based on the proportion of TIV that has been
#' classified as ambiguous in RMS
#' @examples rms_construction_score(edm = "RMS_EDM_QTE_16_09",
#' account = "CL16_CC844E16A000_07",
#' thresholds = c(0.7,0.9,0.95))
rms_construction_score <- function(server = "GBLONTPD111", edm, account, thresholds){

  qry <- paste0("Exposure_Analysis..sp_EDM_ANALYSIS_s2",
                "'", edm,"',",
                "'", account, "'")

  df <- Faraday.Pricing::run_query_on_sql_server(query = qry,
                                                 server = server,
                                                 database = edm) %>%
    filter(PERIL == "Windstorm")


  value <- sum(df$VALUEAMT[df$BLDG_AMBIGUOUS == 0]) / sum(df$VALUEAMT)

  return(.get_interval(thresholds, value))

}

#
year_built_score <- function(server = "GBLONTPD111", edm, account, thresholds){
  qry <- paste0("Exposure_Analysis..sp_EDM_ANALYSIS_s1 ",
                "'", edm,"',",
                "'", account, "'")

  # fpath <- system.file("sql", "sp_s1.sql", package="fire.pricing")
  fpath <- "./inst/sql/sp_s1.sql"

  qry <- read_sql(fpath)
  qry <- gsub(pattern = "")


  qry <- "Exposure_Analysis..sp_EDM_ANALYSIS_s1 'RMS_EDM_QTE_16_09','CL16_QTE18569_01'"

  df <- Faraday.Pricing::get_data_from_sql_server(query = qry,
                                                 server = server,
                                                 database = edm) %>%
    filter(PERIL == "Windstorm")
}
#
#
# num_stories_score <- function(server = "GBLONTPD111", edm, account, thresholds){
#
# }
#
#
# secondary_modifiers_known <- name <- function(server = "GBLONTPD111", edm, account, thresholds){
#
# }
#
#
# geocoding_score <- function(server = "GBLONTPD111", edm, account, thresholds){
#
#
# }

#' Description
#'
#' @param server The SQL Server databse serevr
#' @return
#' @details
#' @examples
rms_data_quality_scores <- function(server = "GBLONTPD111", edm, account, thresholds){

  retVal = list(
    construction_score = rms_construction_score(server, edm, account, thresholds),
    occupancy_score = rms_occupancy_score(server, edm, account, thresholds)
  )

  return(retVal)

}



# t0 <- proc.time()
# rms_occupancy_score(edm = "RMS_EDM_QTE_16_09",
#                     account = "CL16_CC844E16A000_07",
#                     thresholds = c(0.7,0.9,0.95))
# print(proc.time() - t0)
#
# t0 <- proc.time()
# rms_construction_score(edm = "RMS_EDM_QTE_16_09",
#                        account = "CL16_CC844E16A000_07",
#                        thresholds = c(0.7,0.9,0.95))
# print(proc.time() - t0)
#
