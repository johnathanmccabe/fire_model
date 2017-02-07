
upload_occupancy_info_from_template <- function(wb, user_credential, assumption_set_id){

  as_id <- as.integer(assumption_set_id)

  ## get distinct locations
  occupancy_data <- openxlsx::read.xlsx(wb, sheet = "occupancy", skipEmptyRows = T)

  if(nrow(occupancy_data) > 0){

    ##delete existing
    Faraday.Pricing::run_query_on_sql_server("DELETE FROM dbo.occupancy_factor where assumption_set_id = ?",
                                             server = SERVER,
                                             database = DATABASE,
                                             data = assumption_set_id)


    ## ensure codes are lower case
    occupancy_data <- occupancy_data %>%
      dplyr::mutate(occupancy_code = tolower(occupancy_code),
                    atc_code = tolower(atc_code))

    #get existing occupancies
    db_occs <- Faraday.Pricing::get_data_from_sql_server("SELECT * from dbo.occupancy",
                                                         server = SERVER,
                                                         database = DATABASE,
                                                         stringsAsFactors = F) %>%
      dplyr::select(atc_code, audit_dtm) %>%
      mutate(atc_code = as.character(atc_code))

    new_occs <- occupancy_data %>%
      dplyr::left_join(db_occs, by = c("atc_code")) %>%
      dplyr::filter(is.na(audit_dtm)) %>%
      dplyr::distinct(atc_code,
                      occupancy_description,
                      occupancy_group,
                      is_residential) %>%
      dplyr::mutate(audit_dtm = Sys.time(),
                    audit_credential = user_credential) %>%
      dplyr::mutate(is_residential = ifelse(is.na(is_residential), "N", is_residential),
                    is_residential = ifelse(toupper(is_residential) == "Y", "Y", "N"))

    ## UPLOAD NEW OCCUPANCIES
    if(nrow(new_occs) > 0){
      Faraday.Pricing::save_data_to_sql_server(new_occs,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.occupancy",
                                               append = T)

    }


    max_id <- Faraday.Pricing::get_data_from_sql_server("SELECT occupancy_factor_id from dbo.occupancy_factor",
                                                        server = SERVER,
                                                        database = DATABASE,
                                                        stringsAsFactors = F) %>%
      unlist() %>%
      as.integer() %>%
      max(c(0,.))

    #max_id <- max(c(0,max_id))

    sprinkler.factors <- occupancy_data %>%
      distinct(atc_code, sprinkler_factor)

    coverage_codes <- data.frame(column = c("buildings_factor", "contents_factor", "bi_factor"),
                                 coverage_code = c("b", "c", "bi"),
                                 stringsAsFactors = F)

    occ_factors <- occupancy_data %>%
      melt(id.vars = "atc_code",
           measure.vars = c("buildings_factor", "contents_factor", "bi_factor"),
           value.name = "occupancy_factor",
           stringsAsFactors = F) %>%
      dplyr::mutate(variable = as.character(variable)) %>%
      distinct() %>%
      dplyr::inner_join(coverage_codes, by = c("variable" = "column")) %>%
      dplyr::inner_join(sprinkler.factors, by = c("atc_code")) %>%
      dplyr::select(atc_code,
                    coverage_code,
                    occupancy_factor,
                    sprinkler_factor) %>%
      dplyr::mutate(audit_dtm = Sys.time(),
                    audit_credential = user_credential,
                    assumption_set_id = as_id,
                    occupancy_factor_id = 1:nrow(.) + max_id)

    ## UPLOAD NEW FACTORS
    if(nrow(occ_factors) > 0){


      Faraday.Pricing::save_data_to_sql_server(data = occ_factors,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.occupancy_factor",
                                               append = T,
                                               verbose = F,
                                               include_pk = T)

    }

    ##UPLOAD occupancy_scheme codes

    existing <- Faraday.Pricing::get_data_from_sql_server("SELECT * from dbo.occupancy_scheme",
                                                          server = SERVER,
                                                          database = DATABASE,
                                                          stringsAsFactors = F) %>%
      dplyr::select(occupancy_scheme, occupancy_code, atc_code, audit_dtm)

    occ_schemes <- occupancy_data %>%
      dplyr::distinct(occupancy_scheme,
               occupancy_code,
               atc_code) %>%
      dplyr::group_by(occupancy_scheme,
               occupancy_code) %>%
      dplyr::filter(atc_code == atc_code[1]) %>%
      ungroup() %>%
      dplyr::mutate_all(.funs = funs(tolower)) %>%
      distinct()

    new_schemes <- occ_schemes %>%
      dplyr::left_join(existing, by = c("occupancy_scheme", "occupancy_code", "atc_code")) %>%
      dplyr::filter(is.na(audit_dtm)) %>%
      mutate(audit_dtm = Sys.time(),
             audit_credential = user_credential)

    ## UPLOAD NEW FACTORS
    if(nrow(new_schemes) > 0){


      Faraday.Pricing::save_data_to_sql_server(data = new_schemes,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.occupancy_scheme",
                                               append = T,
                                               verbose = F,
                                               include_pk = T)

    }

  }

}

