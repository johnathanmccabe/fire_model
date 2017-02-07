
upload_construction_info_from_template <- function(wb, user_credential, assumption_set_id){

  as_id <- as.integer(assumption_set_id)

  ## get distinct locations
  construction_data <- openxlsx::read.xlsx(wb, sheet = "construction", skipEmptyRows = T) %>%
    rename(construction_scheme = building_scheme,
           construction_code = building_class)

  if(nrow(construction_data) > 0){

    ##delete existing
    Faraday.Pricing::run_query_on_sql_server("DELETE FROM dbo.construction_factor where assumption_set_id = ?",
                                             server = SERVER,
                                             database = DATABASE,
                                             data = assumption_set_id)


    ## ensure codes are lower case
    construction_data <- construction_data %>%
      dplyr::mutate(construction_code = tolower(construction_code),
                    construction_scheme = tolower(construction_scheme),
                    iso_code = tolower(iso_code),
                    description = iso_description)

    #get existing occupancies
    db_cons <- Faraday.Pricing::get_data_from_sql_server("SELECT * from dbo.construction",
                                                         server = SERVER,
                                                         database = DATABASE,
                                                         stringsAsFactors = F) %>%
      dplyr::select(iso_code, audit_dtm) %>%
      mutate(iso_code = as.character(iso_code))

    new_cons <- construction_data %>%
      dplyr::left_join(db_cons, by = c("iso_code")) %>%
      dplyr::filter(is.na(audit_dtm)) %>%
      dplyr::distinct(iso_code,
                      description) %>%
      dplyr::mutate(audit_dtm = Sys.time(),
                    audit_credential = user_credential)

    ## UPLOAD NEW OCCUPANCIES
    if(nrow(new_cons) > 0){
      Faraday.Pricing::save_data_to_sql_server(new_cons,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.construction",
                                               append = T)

    }


    max_id <- Faraday.Pricing::get_data_from_sql_server("SELECT construction_factor_id from dbo.construction_factor",
                                                        server = SERVER,
                                                        database = DATABASE,
                                                        stringsAsFactors = F) %>%
      unlist() %>%
      as.integer() %>%
      max(c(0,.))


    coverage_codes <- data.frame(column = c("buildings_factor", "contents_factor", "bi_factor"),
                                 coverage_code = c("b", "c", "bi"),
                                 stringsAsFactors = F)

    con_factors <- construction_data %>%
      melt(id.vars = "iso_code",
           measure.vars = c("buildings_factor", "contents_factor", "bi_factor"),
           value.name = "construction_factor",
           stringsAsFactors = F) %>%
      dplyr::mutate(variable = as.character(variable)) %>%
      distinct() %>%
      dplyr::inner_join(coverage_codes, by = c("variable" = "column")) %>%
      dplyr::select(iso_code,
                    coverage_code,
                    construction_factor) %>%
      dplyr::mutate(audit_dtm = Sys.time(),
                    audit_credential = user_credential,
                    assumption_set_id = as_id,
                    construction_factor_id = 1:nrow(.) + max_id)

    ## UPLOAD NEW FACTORS
    if(nrow(con_factors) > 0){


      Faraday.Pricing::save_data_to_sql_server(data = con_factors,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.construction_factor",
                                               append = T,
                                               verbose = F,
                                               include_pk = T)

    }

    ##UPLOAD construction_scheme codes

    existing <- Faraday.Pricing::get_data_from_sql_server("SELECT * from dbo.construction_scheme",
                                                          server = SERVER,
                                                          database = DATABASE,
                                                          stringsAsFactors = F) %>%
      dplyr::select(construction_scheme, construction_code, iso_code, audit_dtm) %>%
      mutate(iso_code = as.character(iso_code))

    con_schemes <- construction_data %>%
      dplyr::distinct(construction_scheme,
                      construction_code,
                      iso_code) %>%
      dplyr::group_by(construction_scheme,
                      construction_code) %>%
      dplyr::filter(iso_code == iso_code[1]) %>%
      ungroup() %>%
      dplyr::mutate_all(.funs = funs(tolower)) %>%
      distinct()

    new_schemes <- con_schemes %>%
      dplyr::left_join(existing, by = c("construction_scheme", "construction_code", "iso_code")) %>%
      dplyr::filter(is.na(audit_dtm)) %>%
      mutate(audit_dtm = Sys.time(),
             audit_credential = user_credential)

    ## UPLOAD NEW FACTORS
    if(nrow(new_schemes) > 0){


      Faraday.Pricing::save_data_to_sql_server(data = new_schemes,
                                               server = SERVER,
                                               database = DATABASE,
                                               table = "dbo.construction_scheme",
                                               append = T,
                                               verbose = F,
                                               include_pk = T)

    }

  }

}

