
#' Gets
#'
#' @param
#' @return
#' @details
#' @examples

get_rms_fire_engine_locations <- function(server, edm, account, peril = 5){

  fpath <- system.file("sql", "fire_engine_locations.sql", package="fire.pricing")
  # fpath <- "./inst/sql/fire_engine_locations.sql"

  qry <- Faraday.Pricing::read_sql(fpath)

  qry <- gsub(pattern = "&db&", replacement = edm, x = qry)
  qry <- gsub(pattern = "&account&", replacement = account, x = qry)
  qry <- gsub(pattern = "&peril&", replacement = peril, x = qry)


  retVal <- Faraday.Pricing::get_data_from_sql_server(query = qry,
                                     server = server,
                                     database = edm,
                                     user = "lmteam",
                                     password = "taolmr77",
                                     stringsAsFactors = F)


  return(retVal)
}



find_rms_account <- function(sequel_id, server, edms = NULL){

  fpath <- system.file("sql", "rms_account_group.sql", package="fire.pricing")
  # fpath <- "./inst/sql/rms_account_group.sql"

  qry <- Faraday.Pricing::read_sql(fpath)
  qry <- gsub(pattern = "&sequel_id&", replacement = sequel_id, x = qry)

  ##generate list if edms is null
  if(is.null(edms)){
    dbs <- .get_rms_edms_on_server(server)


  } else {
    dbs = edms
  }


  retVal <- do.call("rbind", lapply(dbs, function(db){

    retVal = tryCatch({
      query = gsub(pattern = "&db&", replacement = db, x = qry)

      retVal <- Faraday.Pricing::get_data_from_sql_server(query = query,
                                                          server = server,
                                                          database = db,
                                                          user = "lmteam",
                                                          password = "taolmr77",
                                                          stringsAsFactors = F)

      retVal$edm = db
      retVal$server = server

      return(retVal)
    },
      error = function(e){
        retVal = data.frame(ACCGRPID = integer()
                            , ACCGRPNUM = character()
                            , ACCGRPNAME = character()
                            , edm = character()
                            , server = character())
    },
      warning = function(w){
        retVal = data.frame(ACCGRPID = integer()
                            , ACCGRPNUM = character()
                            , ACCGRPNAME = character()
                            , edm = character()
                            , server = character())
    })

    return(retVal)
  }))

  return(retVal)
}



.get_rms_edms_on_server <- function(server){

  qry <- "SELECT name
FROM master.sys.databases"

  retVal <- Faraday.Pricing::get_data_from_sql_server(query = qry,
                                                      server = server,
                                                      database = "master")

  regex <- "RMS_EDM_QTE_[0-9_]+"

  retVal <- retVal$name[stringr::str_detect(string = retVal$name, pattern = regex)]

  return(retVal)
}

#' Get the year from a Sequel ID
#'
#' @param sequel_id The Sequel ID. This should be of the form 'AA000A00A000'
#' @return
#' @details
#' @examples
.get_year_from_sequel_id <- function(sequel_id){

  # regex <- "RMS_EDM_QTE_[0-9]{2}(_[0-9]{2})*"
  regex <- "[A-Za-z]{2}[0-9]{3}[A-Za-z]([0-9]{2})[A-Za-z][0-9]{3}"
  year <- stringr::str_match(string = sequel_id, pattern = regex)[,2]

  return(as.integer(year))
}

# xx <- get_rms_fire_engine_locations(edm = "RMS_EDM_QTE_16",
#                           server = "GBLONTPD111",
#                           account = "CL16_CG278N16A000_01")

# t0 <- proc.time()
# xx <- find_rms_account(sequel_id = "CC656B13A000", server = "GBLONTPD111") #, edms = "RMS_EDM_QTE_16_10")
# print(proc.time() - t0)
#
# yy <- get_rms_fire_engine_locations(edm = xx$edm[11],
#                                     server = "GBLONTPD111",
#                                     account = xx$ACCGRPNUM[11])
#
#
# install_svn("http://jmccabe:jmccabe@gblontas69:8081/svn/Faraday.Pricing.Calculation",
#             subdir = "R.Projects/faraday-pricing")

