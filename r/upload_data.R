library(openxlsx)
library(dplyr)
library(Faraday.Pricing)
SERVER = "gblontdv56\\PRICING,57066"
DATABASE = "fire_model"



wb = "../../excel/fire_model_assumption_upload_template.xlsx"

user_credential = "jmccabe"

upload_assumption_set_data <- function(wb, user_credential){

  assumption_desc <- openxlsx::read.xlsx(wb, sheet = "overview")[1,]

  ##delete existing
  assumption_data <- Faraday.Pricing::get_data_from_sql_server("SELECT * FROM dbo.assumption_set",
                                           server = SERVER,
                                           database = DATABASE)


  match.data <- assumption_data %>% dplyr::filter(description == assumption_desc)

  if(nrow(match.data) > 0){

    id = match.data$assumption_set_id[1]

    df <- data.frame(description = assumption_desc,
                     audit_dtm = as.character(Sys.time()),
                     audit_credential = user_credential,
                     assumption_set_id = id,
                     stringsAsFactors = F)
                     # )
    qry <- "UPDATE dbo.assumption_set SET description = '%desc%', audit_dtm = '%dtm%', audit_credential = '%cred%'
WHERE assumption_set_id = %id%"

    qry <- gsub(pattern = "%desc%", assumption_desc, x = qry)
    qry <- gsub(pattern = "%dtm%", Sys.time(), x = qry)
    qry <- gsub(pattern = "%cred%", user_credential, x = qry)
    qry <- gsub(pattern = "%id%", id, x = qry)

    Faraday.Pricing::run_query_on_sql_server(query = qry,
                                             server = SERVER,
                                             database = DATABASE)


  } else {

    id = max(c(0,assumption_data$assumption_set_id) + 1)

    df <- data.frame(description = assumption_desc,
                     audit_dtm = Sys.time(),
                     audit_credential = user_credential,
                     assumption_set_id = id)

    Faraday.Pricing::save_data_to_sql_server(data = df,
                                             server = SERVER,
                                             database = DATABASE,
                                             table = "dbo.assumption_set",
                                             append = T,
                                             include_pk = T)

  }

  return(id)

}


upload_location_info <- function(wb, user_credential, assumption_set_id){

  as_id <- as.integer(assumption_set_id)

  ## get distinct locations
  location_data <- openxlsx::read.xlsx(wb, sheet = "location", skipEmptyRows = T)

  if(nrow(location_data) > 0){

    ##delete existing
    Faraday.Pricing::run_query_on_sql_server("DELETE FROM dbo.location_factor where assumption_set_id = ?",
                                             server = SERVER,
                                             database = DATABASE,
                                             data = assumption_set_id)


    ## ensure codes are lower case
    location_data <- location_data %>%
      dplyr::mutate(location_code = tolower(location_code),
                    coverage_code = tolower(coverage_code))

    #get existing locations
    db_locs <- Faraday.Pricing::get_data_from_sql_server("SELECT * from dbo.location",
                                                      server = SERVER,
                                                      database = DATABASE,
                                                      stringsAsFactors = F) %>%
      dplyr::select(location_code, audit_dtm)

    new_locs <- location_data %>%
      dplyr::distinct(location_code, location_description) %>%
      dplyr::group_by(location_code) %>%
      dplyr::filter(location_description == location_description[1]) %>%
      ungroup() %>%
      dplyr::left_join(db_locs, by = c("location_code")) %>%
      dplyr::filter(is.na(audit_dtm)) %>%
      dplyr::mutate(audit_dtm = Sys.time(),
             audit_credential = user_credential)

    if(nrow(new_locs) > 0){
      Faraday.Pricing::save_data_to_sql_server(new_locs,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.location",
                                               append = T,
                                               verbose = F)

    }


    max_id <- Faraday.Pricing::get_data_from_sql_server("SELECT location_factor_id from dbo.location_factor",
                                                        server = SERVER,
                                                        database = DATABASE,
                                                        stringsAsFactors = F) %>%
      unlist() %>%
      as.integer() %>%
      max(c(0,.))

    #max_id <- max(c(0,max_id))


    loc_factors <- location_data %>%
      dplyr::select(location_code,
                    factor = location_factor,
                    coverage_code) %>%
      dplyr::mutate(audit_dtm = Sys.time(),
                    audit_credential = user_credential,
                    assumption_set_id = as_id,
                    location_factor_id = 1:nrow(.)+max_id)

    if(nrow(loc_factors) > 0){


      Faraday.Pricing::save_data_to_sql_server(data = loc_factors,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.location_factor",
                                               append = T,
                                               verbose = T,
                              include_pk = T)

    }

  }

}




id <- upload_assumption_set_data(wb, "jmccabe")
upload_location_info("../../excel/fire_model_assumption_upload_template.xlsx", "jmccabe", id)

upload_occupancy_info_from_template("../../excel/fire_model_assumption_upload_template.xlsx", "jmccabe", id)

upload_construction_info_from_template("../../excel/fire_model_assumption_upload_template.xlsx", "jmccabe", id)



