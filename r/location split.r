require(RODBC)
require(data.table)
require(devtools)
require(Faraday.Pricing)

## set the max sir for clb / cla
max_sir <- 75000
## above 500k use excess curve
excess_threshold <- 500000

# Large loss cap
large_cap <- data.table(aatg = c("CLA", "CLA", "CLB", "CLB"), 
                        res = c("residential", "non residential", "residential", "non residential"),
                        cap = c(0.5e6, 1.5e6, 0.5e6, 2e6)) 

use_cap <- T

## function to copy data table to clip board
copy.table <- function(obj, size = 8192) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

## function to calculate the incremental curve
toinc <- function(d){
  t <- c(0, d)
  d <- c(d, 1) 
  
  ret <- d - t
  return(ret[1:length(ret) - 1])
}

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Connects to the database
ch <- odbcConnect("RMS")

## get the written exposure data
loc <- setDT(sqlQuery(ch, "SELECT   ACCNTNUM, LOCNUM, CASE WHEN LOCNAME IS NULL THEN '' ELSE LOCNAME END AS LOCNAME, CASE WHEN ADDRESSNUM IS NULL 
                      THEN '' ELSE ADDRESSNUM END AS ADDRESSNUM, CASE WHEN STREETNAME IS NULL THEN '' ELSE STREETNAME END AS STREETNAME, 
                      CASE WHEN CITY IS NULL THEN '' ELSE CITY END AS CITY, CASE WHEN STATECODE IS NULL THEN '' ELSE STATECODE END AS STATECODE, 
                      CASE WHEN POSTALCODE IS NULL THEN '' ELSE POSTALCODE END AS POSTALCODE, CASE WHEN COUNTY IS NULL THEN '' ELSE COUNTY END AS COUNTY, 
                      CNTRYSCHEME, CNTRYCODE, BLDGSCHEME, BLDGCLASS, OCCSCHEME, OCCTYPE, NUMBLDGS, CASE WHEN YEARBUILT IS NULL 
                      THEN '' ELSE Year(YEARBUILT) END AS YEARBUILT, CASE WHEN PC IS NULL THEN '' ELSE PC END AS PC, CASE WHEN FRSPRINKLERSYS IS NULL 
                      THEN '' ELSE FRSPRINKLERSYS END AS FRSPRINKLERSYS, FRCV1VCUR, 
CASE WHEN FRCV1VAL IS NULL THEN 0 ELSE FRCV1VAL END AS Buildings, 
                      CASE WHEN FRCV2VAL IS NULL THEN 0 ELSE FRCV2VAL END AS Contents, 
CASE WHEN FRCV3VAL IS NULL THEN 0 ELSE FRCV3VAL END AS BI, 
                      CASE WHEN FRCV1VAL IS NULL THEN 0 ELSE FRCV1VAL END + CASE WHEN FRCV2VAL IS NULL THEN 0 ELSE FRCV2VAL END + CASE WHEN FRCV3VAL IS NULL 
                      THEN 0 ELSE FRCV3VAL END AS TIV, upload_id
                      FROM            [ScheduleBaseData_OM].dbo.temploc
                      WHERE riskType = 'Written' and FRCV1VCUR is not NULL",stringsAsFactors = F))

## find the sequel id from the accntnum
## the uid is either sequel id or program id
loc[, uid := do.call('rbind', strsplit(ACCNTNUM,'_'))[, 2]]
loc[, yoa := do.call('rbind', strsplit(ACCNTNUM,'_'))[, 1]]

## Convert CLxx to 20xx
loc[, yoa := as.numeric(gsub('\\CL', '20', yoa))]
loc <- loc[!is.na(yoa)]

odbcClose(ch)

## select the exposure set with the latest upload id
loc[, latest_upload_id := max(upload_id), by = .(yoa, uid)]
loc <- loc[upload_id == latest_upload_id]

## trim and convert id to lower case
loc[, uid := trim(uid)]
loc[, uid := tolower(uid)]

gc()

## Get latest data from seuqel
clb_policy <- setDT(get_info_team_policy_info(2009, 2016, "aatg", c("CLB", "CLA"), 
                                              include_financials = T, output_ccy = 'USD', 
                                              exchange_rates = NULL, financial_data_as_at = Sys.Date()))

clb_policy[, sequel_id := trim(sequel_id)]
clb_policy[, program_reference := trim(program_reference)]

# uid is either program referene or sequel id
# format the uid
clb_policy[, uid := ifelse(program_reference == "", sequel_id, program_reference)]
clb_policy[, uid := tolower(uid)]

clb_policy <- clb_policy[, .(aatg, sequel_id, program_reference, uid, yoa, assured, limit, deductible, 
                             line_size = effective_line_premium, block_stat,
                             paid = paid_claim, incurred = paid_claim + os_claim,
                             uw_lr = uw_gaap_loss_ratio_pct,
                             prem = ifelse(yoa <= year(Sys.Date()) - 2, booked_premium + brokerage + commission, written_premium / (1 - uw_deductions_pct)))]

clb_policy[, uw_loss := prem * uw_lr]

clb_policy <- clb_policy[, .(line_size = max(line_size), deductible = max(deductible), limit = max(limit),
                             prem = sum(prem), paid = sum(paid), 
                             incurred = sum(incurred), uw_loss = sum(uw_loss)), 
                         by = .(aatg, uid, sequel_id, program_reference, yoa, block_stat)]

# Excess when > 5m
# clb_policy[, curve := ifelse(deductible < excess_threshold, 'Lloyds - Primary', 'Lloyds - Excess')]

loc <- merge(clb_policy, loc, by = c('yoa', 'uid'), all.x = T, allow.cartesian = T)

loc <- loc[!is.na(ADDRESSNUM)]

loc[incurred < 0, incurred := 0]

gc()


## get the mappings
## tidy up the reference
loc[, BLDGSCHEME := gsub(" ", "", BLDGSCHEME, fixed = TRUE)]
loc[, OCCSCHEME := gsub(" ", "", OCCSCHEME, fixed = TRUE)]

## group schemes
loc[, BLDGSCHEME := ifelse(BLDGSCHEME %in% c('RMSHU', 'RMSINS', 'RMSIIND'), 'RMS', BLDGSCHEME)]
loc[, OCCSCHEME := ifelse(OCCSCHEME %in% c('RMSHU', 'RMSINS', 'RMSIIND', 'RMS'), 'RMSIND', OCCSCHEME)]
loc[, OCCSCHEME := ifelse(OCCSCHEME %in% c('ISO', 'SIC'), 'ATC', OCCSCHEME)]

## get a host of reference tables
ch <- odbcConnect("Pricing_Dev")

sir_ref <- setDT(sqlQuery(ch, "SELECT [sir], [tiv]
                          ,[adjustment]
                          FROM [fire_engine].[dbo].[sir_curve]",stringsAsFactors = F))

# use the new curves elected by the underwriters
curve <- setDT(sqlQuery(ch, "SELECT [attachment]
                        ,[value]
                        ,[type]
                        FROM [fire_engine].[dbo].[fisrt_loss_curve_new]",stringsAsFactors = F))
setkey(curve, type, attachment)

curve[, inc_value := toinc(value), by = .(type)]

## Change this section for new / old basis..

pc_rates <- setDT(sqlQuery(ch, "SELECT [pc_code] as PC
      ,[const_ref]
                           ,[rate]
                           FROM [fire_engine].[dbo].[pc_ref]",stringsAsFactors = F))

# rates <- setDT(sqlQuery(ch, "SELECT [type]
#       ,[modifier]
#                         ,[state]
#                         ,[atc_code]
#                         ,[iso_const]
#                         ,[rate]
#                         FROM [fire_engine].[dbo].[rates]",stringsAsFactors = F))
# 
# const_code <- setDT(sqlQuery(ch, "SELECT [BLDGSCHEME]
#       ,[BLDGCLASS]
#                              ,[ISO_GROUP]
#                              ,[ISO_DESC]
#                              ,[BUILD_RATE]
#                              ,[CONTENT_RATE]
#                              ,[BI_RATE]
#                              FROM [fire_engine].[dbo].[construction]",stringsAsFactors = F))
# 
# occup_code <- setDT(sqlQuery(ch, "SELECT [OCCSCHEME]
#                              ,[OCCTYPE]
#                              ,[ATC_GROUP]
#                              ,[ATC_DESC]
#                              ,[GROUP]
#                              ,[BUILD_RATE]
#                              ,[CONTENT_RATE]
#                              ,[BI_RATE]
#                              ,[SPRINKLER]
#                              FROM [fire_engine].[dbo].[occupancy]",stringsAsFactors = F))

const_code <- setDT(sqlQuery(ch, "SELECT [BLDGSCHEME]
      ,[BLDGCLASS]
                             ,[ISO_GROUP]
                             ,[ISO_DESC]
                             ,[BUILD_RATE]
                             ,[CONTENT_RATE]
                             ,[BI_RATE]
                             FROM [fire_engine].[dbo].[new_construction]",stringsAsFactors = F))

occup_code <- setDT(sqlQuery(ch, "SELECT [OCCSCHEME]
                             ,[OCCTYPE]
                             ,[ATC_GROUP]
                             ,[ATC_DESC]
                             ,[GROUP]
                             ,[BUILD_RATE]
                             ,[CONTENT_RATE]
                             ,[BI_RATE]
                             ,[SPRINKLER]
                             FROM [fire_engine].[dbo].[new_occupancy]",stringsAsFactors = F))

rates <- setDT(sqlQuery(ch, "SELECT [type]
      ,[modifier]
      ,[state]
      ,[atc_code]
      ,[iso_const]
      ,[new_rate] as [rate]
  FROM [fire_engine].[dbo].[new_rates]",stringsAsFactors = F))

uplift <- 1.15

rate.building <- rates[modifier == 'base' & type == 'buildings', rate] / 100 * uplift
rate.content <- rates[modifier == 'base' & type == 'contents', rate] / 100 * uplift
rate.bi <- rates[modifier == 'base' & type == 'bi', rate] / 100 * uplift

state.building <- rates[modifier == 'state' & type == 'buildings', .(STATECODE = tolower(state), rate)] 
state.content <- rates[modifier == 'state' & type == 'contents', .(STATECODE = tolower(state), rate)] 
state.bi <- rates[modifier == 'state' & type == 'bi', .(STATECODE = tolower(state), rate)] 

odbcClose(ch)

## apply exchange rate
loc[, forex := get_exchange_rates(FRCV1VCUR, "USD")$rate]
loc[, forex := ifelse(is.na(forex), 1, forex)]

loc[, ":="(Buildings = Buildings * forex, 
           Contents = Contents * forex, 
           BI =  BI * forex, 
           TIV = TIV * forex)]

## cap TIV to $1billion
loc[, ":="(Buildings = ifelse(Buildings > 1e9, 1e9, Buildings), 
           Contents = ifelse(Contents > 1e9, 1e9, Contents),
           BI = ifelse(BI > 1e9, 1e9, BI))]

## tidy up the data
loc[, NUMBLDGS := ifelse(is.na(NUMBLDGS), 1, NUMBLDGS)]
loc[, NUMBLDGS := ifelse(NUMBLDGS == 0, 1, NUMBLDGS)]

## find the average value of TIVs
## remove large and zeros
building.avg <- mean(loc[Buildings < 1e8 & Buildings > 0, Buildings/NUMBLDGS])
content.avg <- mean(loc[Contents < 1e8 & Contents > 0, Contents/NUMBLDGS])
bi.avg <- mean(loc[BI < 1e8 & BI > 0, BI/NUMBLDGS])

## adjust TIV when TIV is zero
loc[, ":="(Buildings = ifelse(TIV <= 0, building.avg, Buildings), 
           Contents = ifelse(TIV <= 0, content.avg, Contents),
           BI = ifelse(TIV <= 0, bi.avg, BI))]

loc[, TIV := Buildings + Contents + BI]

## drop all locations where tiv < deductible
loc <- loc[TIV > deductible, ]

gc()

## assign the curves
loc[, curve := ifelse(OCCTYPE %in% c(1, 2, 3, 8, 23), "Primary 1", ifelse(OCCTYPE == 19, "Flat 2", "Primary 2"))]

## calculates the loss cost for each location
## set pc_code to High if its 0
## default value
loc[, PC := as.character(ifelse(PC == 0, "High", PC))]

## if the country is not US apply average rate
## remove the spaces, and to lower case
## if no state code, apply average
loc[, STATECODE := gsub(" ", "", STATECODE, fixed = TRUE)]
loc[, STATECODE := ifelse(CNTRYCODE == 'US', STATECODE, 'Average')]
loc[, STATECODE := ifelse(STATECODE == '', 'Average', STATECODE)]
loc[, STATECODE := tolower(STATECODE)]
loc[, STATECODE := ifelse(STATECODE %in% c('-1', 'a', '**'), 'Average', STATECODE)]

## calculate the base rate
loc[, ":="(buildings_rate = Buildings * rate.building, 
           contents_rate = Contents * rate.content, 
           bi_rate = BI * rate.bi)]

## making adjustments to base rate
## state adjustment
loc <- merge(loc, state.building, by = 'STATECODE', all.x = T)
loc[, buildings_rate := buildings_rate * ifelse(is.na(rate),1,rate)]
loc[, rate := NULL]

loc <- merge(loc, state.content, by = 'STATECODE', all.x = T)
loc[, contents_rate := contents_rate * ifelse(is.na(rate),1,rate)]
loc[, rate := NULL]

loc <- merge(loc, state.bi, by = 'STATECODE', all.x = T)
loc[, bi_rate := bi_rate * ifelse(is.na(rate),1,rate)]
loc[, rate := NULL]

## construction adj
loc <- merge(loc, const_code, by = c('BLDGSCHEME', 'BLDGCLASS'), all.x = T)

loc[, ":="(buildings_rate = buildings_rate * ifelse(is.na(BUILD_RATE),1,BUILD_RATE), 
           contents_rate = contents_rate * ifelse(is.na(CONTENT_RATE),1,CONTENT_RATE), 
           bi_rate = bi_rate * ifelse(is.na(BI_RATE),1,BI_RATE))]

loc[, ":="(BUILD_RATE = NULL, CONTENT_RATE = NULL, BI_RATE = NULL)]

## occupancy adj
loc <- merge(loc, occup_code, by = c('OCCSCHEME', 'OCCTYPE'), all.x = T)

loc[, ":="(buildings_rate = buildings_rate * ifelse(is.na(BUILD_RATE),1,BUILD_RATE), 
           contents_rate = contents_rate * ifelse(is.na(CONTENT_RATE),1,CONTENT_RATE), 
           bi_rate = bi_rate * ifelse(is.na(BI_RATE),1,BI_RATE))]

loc[, ":="(BUILD_RATE = NULL, CONTENT_RATE = NULL, BI_RATE = NULL)]

## this calculates the overall rate
## sum of building, content and bi
## multiple this by the written line
loc[, pure_lc := (bi_rate + buildings_rate + contents_rate) * line_size]

## apply protection code
loc[, const_ref := ifelse(ISO_GROUP == 0 | is.na(ISO_GROUP) , 0, ifelse(ISO_GROUP %in% c(1, 2, 3), 1, 2))]
loc <- merge(loc, pc_rates, by = c('PC', 'const_ref'), all.x = T)
loc[, pure_lc := pure_lc * rate]
loc[, rate := NULL]

## apply sprinkler rate
## sprinkler code 0 and 2 will not receive any benefit
## 0: absent
## 1: present
## 2: unknown
loc[FRSPRINKLERSYS == 1, pure_lc := pure_lc * SPRINKLER]

loc[is.na(pure_lc), pure_lc := buildings_rate + contents_rate + bi_rate]

## insert uid
## this uid is used to split and join tables
loc[, uid := 1:nrow(loc)]

## Allow for estimated excess and deductible
loc[, excess := ifelse(deductible > excess_threshold, deductible, 0)]
loc[, deductible := ifelse(deductible > excess_threshold, 0, deductible)]

loc[, limit := limit + excess]

# split by resendential and non residential
loc[ATC_DESC %in% c("Permanent Dwelling (multi family housing)",
                     "Temporary Lodging",
                     "Permanent Dwelling (single family housing)",
                     "Multi-Family Dwelling-Condominium Unit Owner",
                     "Multi-Family Dwelling-Homeowners Association"), res := "residential"]

loc[is.na(res), res := "non residential"]

loc <- merge(loc, large_cap, by = c("aatg", "res"), all.x = T)
loc[, cap := cap / line_size]

# cap large losses in the model
if(use_cap == T){
  # adjust the limit to the cap level
  loc[cap < (limit - excess), limit := cap + excess]
}

## use TIV per building
loc[, ":="(excess = excess / TIV * NUMBLDGS, 
           limit = limit / TIV * NUMBLDGS)]

loc[, ":="(excess = ifelse(excess > 1, 1, excess), limit = ifelse(limit > 1, 1, limit))]

###############################
# Calculates SIR
###############################

loc[deductible > max_sir, use_curve := T]
loc[deductible <= 500, adj := 1]

sir_list <- unique(sir_ref[, sir])

loc[deductible >= max_sir, ":="(sir_min = max_sir, sir_max = max_sir)]

loc[deductible < max_sir, ":="(sir_min = max(sir_list[sir_list <= deductible]), 
                                sir_max = min(sir_list[sir_list > deductible])), 
    by = 1:nrow(loc[deductible < max_sir])]

loc[is.infinite(sir_min), sir_min := 0]

# calculates sir by interpolating the sir table
loc[is.na(adj) & sir_min != sir_max, 
    ":="(rate_min = approx(sir_ref[sir == sir_min, tiv], sir_ref[sir == sir_min, adjustment], TIV, rule = 2) $ y, 
         rate_max = approx(sir_ref[sir == sir_max, tiv], sir_ref[sir == sir_max, adjustment], TIV, rule = 2) $ y),
    by = 1:nrow(loc[is.na(adj) & sir_min != sir_max])]

loc[is.na(adj) & sir_min != sir_max, 
    adj := (deductible - sir_min) * (rate_max - rate_min)/(sir_max - sir_min) + rate_min]

loc[is.na(adj) & sir_min == sir_max, 
    adj := approx(sir_ref[sir == max_sir, tiv], sir_ref[sir == max_sir, adjustment], TIV, rule = 2) $ y, 
    by = 1:nrow(loc[is.na(adj) & sir_min == sir_max])]

loc[, ":="(sir_min = NULL, sir_max = NULL, rate_min = NULL, rate_max = NULL)]

# functions to interpolate the curves
primary_1 <- curve[type == 'Primary 1', .(attachment, value)]
primary_2 <- curve[type == 'Primary 2', .(attachment, value)]
flat_2 <- curve[type == 'Flat 2', .(attachment, value)]

primary_1_estimate <- approxfun(primary_1[,attachment], primary_1[,value], rule = 2) 
primary_2_estimate <- approxfun(primary_2[,attachment], primary_2[,value], rule = 2) 
flat_2_estimate <- approxfun(flat_2[,attachment], flat_2[,value], rule = 2) 

# calculate sir for those that require curves
loc[use_curve == T & curve == 'Primary 1', excess_sir := primary_1_estimate(deductible / TIV) - 
      primary_1_estimate(max_sir / TIV)]
loc[use_curve == T & curve == 'Primary 2', excess_sir := primary_2_estimate(deductible / TIV) - 
      primary_2_estimate(max_sir / TIV)]
loc[use_curve == T & curve == 'Flat 2', excess_sir := flat_2_estimate(deductible / TIV) - 
      flat_2_estimate(max_sir / TIV)]

loc[is.na(excess_sir), excess_sir := 0]

loc[, adj := adj * (1 - excess_sir)]

loc[, ":="(excess_sir = NULL, use_curve = NULL)]

loc[, adj := ifelse(is.na(adj), 1, adj)]
loc[, pure_lc := pure_lc * adj]

## get the excess and primary curves and create estimate function
# excess_curve <- curve[type == 'Lloyds - Excess', .(attachment, value)]
# primary_curve <- curve[type == 'Lloyds - Primary', .(attachment, value)]

## use linear estimate the estimate the curve value
# excess_estimate <- approxfun(excess_curve[,attachment], excess_curve[,value], rule = 2)
# primary_estimate <- approxfun(primary_curve[,attachment], primary_curve[,value], rule = 2)

# interpolate the curves
loc[curve == 'Primary 1', g_excess := primary_1_estimate(excess)]
loc[curve == 'Primary 1', g_limit := primary_1_estimate(limit)]

loc[curve == 'Flat 2', g_excess := flat_2_estimate(excess)]
loc[curve == 'Flat 2', g_limit := flat_2_estimate(limit)]

loc[curve == 'Primary 2', g_excess := primary_2_estimate(excess)]
loc[curve == 'Primary 2', g_limit := primary_2_estimate(limit)]

## apply the curve to loss cost
loc[, pure_lc := pure_lc * (g_limit - g_excess)]

setwd("U:/FRE/Actuarial/2016 Work/Pricing Ultimates/Q2 2016/Comm Lines")

## split the premium and incurred / paid loss by location 
## format the data table 
d <- loc[, .(yoa, atg = aatg, sequel_id, sprinkler_code = FRSPRINKLERSYS, country = CNTRYCODE,
             state = STATECODE, construction = ISO_DESC, occupancy = ATC_DESC, res, pure_lc)]

d <- d[, .(pure_lc = sum(pure_lc)), 
       by = .(yoa, atg, sequel_id, sprinkler_code, country, state, construction, occupancy, res)]

# loss cost split
d <- d[, .(sprinkler_code, country, state, construction, occupancy, res, pure_lc, split = pure_lc / sum(pure_lc)), by = .(yoa, atg, sequel_id)]

d[, res_lc := sum(ifelse(res == 'residential', pure_lc, 0)), by = .(yoa, atg, sequel_id)]
d[, non_res_lc := sum(ifelse(res != 'residential', pure_lc, 0)), by = .(yoa, atg, sequel_id)]

d[, res_perc := res_lc / (res_lc + non_res_lc)]

d[atg == 'CLA', large_loss_cap := 0.5e6 * res_perc + (1 - res_perc) * 1.5e6]
d[atg == 'CLB', large_loss_cap := 0.5e6 * res_perc + (1 - res_perc) * 2e6]

d[, ":="(res_lc = NULL, non_res_lc = NULL, res_perc = NULL)]

write.csv(d, "location_split.csv", row.names = F)


###################################################################################
#### this bit of code converts exposure curve to frequency curve
###################################################################################

### modified function to include cat info and loss desc
get_claim_movements <- function(yoa_from,
                                yoa_to,
                                reporting_level = "class",
                                reporting_values,
                                syndicate = c(1192,435),
                                output_ccy = "USD",
                                exchange_rates = NULL,
                                exchange_rate_as_at = Sys.Date(),
                                data_as_at = Sys.Date()){
  
  as_at_proc_month <- proc_month_from_date(data_as_at)
  
  # reporting level
  ## set up query with parameters
  r_level <- tolower(reporting_level)
  level <- ifelse(r_level == "class" | r_level == "team", "rep.ClassType", 
                  ifelse(r_level == "minorclass",
                         "rep.MinorClass",
                         "r.ActuarialATG"))
  
  values <- paste0("'",reporting_values,"'", collapse=",")
  synd <- paste(as.character(syndicate), collapse=",")
  
  # path to query
  fpath <- system.file("sql", "claim_movements.sql", package="Faraday.Pricing")
  #fpath <- ("../inst/sql/claim_movements.sql")
  # run query
  query <- read_sql(fpath)
  
  query <- str_replace_all(query, "&YearFrom&", yoa_from)
  query <- str_replace_all(query, "&YearTo&", yoa_to)
  query <- str_replace_all(query, "&Synd&", synd)
  query <- str_replace_all(query, "&Where&", level)
  query <- str_replace_all(query, "&WhereValue&", values)
  query <- str_replace_all(query, "&AsAt&", as_at_proc_month)
  
  ##get data
  data <- get_data_from_sql_server(query = query,
                                   server = "FARADAYWAREHOUSE",
                                   database = "FaradayInfoTeam")
  
  # add inception and expiry dates as date objects
  retVal <- data  %>% 
    dplyr::mutate(inception_date = as.Date(inception_date_str, tz=""),
                  expiry_date = as.Date(expiry_date_str, tz="")) %>% 
    dplyr::select(-inception_date_str, -expiry_date_str)
  
  
  
  # get exchange rates
  ccy_codes <- tolower(unique(retVal$settlement_ccy))
  
  #retrieve exchange rates for given time if not entered
  if(is.null(exchange_rates)){
    
    exch_rates <- get_exchange_rates(currency_codes = ccy_codes,
                                     to_currency_code = output_ccy,
                                     as_at_date = as.Date(exchange_rate_as_at))
    
  } else {
    exch_rates <- exchange_rates %>% 
      dplyr::mutate(ccy = tolower(ccy))
    
    # check that all currencies required are available
    not_incl_ccy <- ccy_codes[!(ccy_codes %in% exch_rates$ccy)]
    
    if(length(not_incl_ccy) > 0){
      stop(paste0("Exchange rates for the following currencies are required: ", paste0(not_incl_ccy, collapse = ", ")))
    }
    
    output_rate <- exch_rates$rate[exch_rates$ccy == tolower(output_ccy)]
    
    exch_rates <- exch_rates %>% 
      dplyr::mutate(rate = rate / output_rate)
  }
  
  retVal <- retVal %>% 
    # ensure ccy codes are lower case
    dplyr::mutate(settlement_ccy = tolower(settlement_ccy)) %>% 
    dplyr::inner_join(exch_rates, by = c("settlement_ccy" = "ccy")) %>% 
    dplyr::group_by(as_at_proc_month,
                    synd_ref,
                    yoa,
                    class_type,
                    minor_class,
                    aatg,
                    premium_type_code,
                    program_reference,
                    sequel_id,
                    inception_date,
                    expiry_date,
                    block,
                    stat,
                    block_stat,
                    claim_reference,
                    commuted,
                    assured,
                    territorial_scope,
                    underwriter,
                    limit,
                    deductible,
                    risk_code,
                    proc_month,
                    date_of_loss_from,
                    date_of_loss_to,
                    claim_narrative, 
                    loss_narrative, 
                    cat_desc, claim_status) %>% 
    dplyr::summarise(paid = sum(paid * rate),
                     outstanding = sum(outstanding * rate),
                     say = sum(say * rate),
                     incurred = sum(incurred * rate)) %>% 
    ungroup() %>% 
    mutate(ccy_code = output_ccy)
  
  
  return(retVal)
  
}

compare <- function(yr, loc, curve, band_size){
  
  setkey(curve, type, attachment)
  
  curve_names <- unique(curve[, type])
  
  ret <- NULL
  
  for(i in 1:4){
    
    cn <- curve_names[i]
    
    x <- curve[type == cn, attachment]
    y <- curve[type == cn, value]
    
    l <- unique(c(seq(0, 0.1, length.out= 100), 
                  seq(0.1, 0.5, length.out= 100), 
                  seq(0.5, 1, length.out= 100)))
    
    out <- data.table(type = cn, attachment = l, value = approx(x, y, xout = l)$y)
    
    if(is.null(ret)){ret <- out} else {ret <- rbind(ret, out)}
  }
  
  d <- loc[yoa == yr, .(yoa, aatg, sequel_id, sprinkler_code = FRSPRINKLERSYS, LOCNUM,
                          state = STATECODE, construction = ISO_DESC, occupancy = ATC_DESC, pure_lc, line_size,
                          excess = excess * TIV, limit = limit * TIV, TIV, type = curve)]
  
  d <- d[, .(pure_lc = sum(pure_lc)), by = .(yoa, aatg, sequel_id, LOCNUM, sprinkler_code, state, construction, 
                                             occupancy, excess, limit, TIV, type, line_size)]
  
  sequel <- unique(d[, sequel_id])
  
  curve <- copy(ret)
  
  # Find the increamental value of both attachment and curve value
  curve[, ":="(attachment_inc = toinc(attachment), value_inc = toinc(value)), by = .(type)]
  
  # Find the number of loss in each band
  curve[, n_loss := ifelse(is.na(value_inc / attachment_inc), 0, value_inc / attachment_inc)]
  
  for (i in 1:length(unique(curve[, type]))){
    
    t <- curve[type == unique(curve[, type])[i]]
    t <- t[order(-attachment)]
    t[, frequency := 0]
    
    for (j in 1:nrow(t)){
      
      if(j == 1){t[j, frequency := n_loss]}
      else{
        p <- sum(t[1:j, frequency])
        t[j, frequency := n_loss - p]
      }
      
    }
    
    t[attachment == 0, frequency := 0]
    t[abs(frequency) < 1e-10, frequency := 0]
    
    # order the curve
    t <- t[order(attachment)]
    
    curve[type == unique(curve[, type])[i], frequency := t[, frequency]]
  }
  
  curve <- curve[frequency > 0, ]
  
  # Probability is number of loss divided by the total number of losses
  curve[, freq := frequency / sum(frequency), by = .(type)]
  
  curve <- curve[, .(attachment, type, p = freq)]
  
  d <- merge(d, curve, by = 'type', all.x = T, allow.cartesian = T)
  
  gc()
  
  d[, attachment := attachment * TIV]
  d <- d[attachment >= excess, ]
  d[attachment > limit, attachment := limit]
  d[, attachment := attachment - excess]
  d[, attachment := attachment * line_size]
  
  gc()
  
  d[, band := ceiling(attachment / band_size) * band_size - band_size / 2]
  d[band < band_size / 2, band := band_size / 2]
  
  d[, curve_lc := sum(band * p), by = .(yoa, aatg, sequel_id, LOCNUM, sprinkler_code, state, construction, 
                                        occupancy, excess, limit, TIV, type, line_size)]
  
  d[, freq := pure_lc / curve_lc * p]
  
  d <- d[, .(yoa, aatg, sprinkler_code, state, construction, occupancy, band, freq)]
  
  aatg_curve <- d[, .(freq = sum(freq)), by = .(yoa, aatg, band)]
  
  ## do the claims
  claims <- setDT(get_claim_movements(yr, yr, "aatg", c("CLA", "CLB"), output_ccy = 'USD'))
  
  claims <- claims[, .(yoa, aatg, loss_narrative, blockstats = block_stat, sequel_id, proc_month,
                       claim_ref = claim_reference, cat_desc, status = claim_status, paid, 
                       incurred = paid + outstanding, accident = date_of_loss_from)]
  
  claims <- claims[sequel_id %in% sequel]
  
  none_fire_terms <- "(?:^|(?<= ))(natural peril|dean|california fires|freeze|hurricane|typhoon|polar vortex|polar vertex|lightning|deluge|hail|hailstorm|snowstorm|rainstorm|winterstorms|flood|floods|flooded|flooding|tornado|tornados|lightening|wind|winds|storm|storms|windstorms|windstorm|cyclone|rain|rains|snow|frozen|wildfire|wildfires|microburst|cloudburst|driven water|storm surge|tsunami|earthquake|quake|derecho|supercell|ice jam)(?:(?= )|$)"
  
  claims[, loss_narrative := paste(loss_narrative, cat_desc, sep=" ")]
  
  # convert loss narrative to lower case
  claims[, loss_narrative := tolower(loss_narrative)]
  
  # remove all none standard characters
  claims[, loss_narrative := str_replace_all(loss_narrative, "[^[:alnum:]]", " ")]
  claims[, none_fire := str_detect(claims[, loss_narrative], none_fire_terms)]
  
  claims <- claims[none_fire == F, ]
  
  claims <- claims[, .(incurred = sum(incurred)), by = .(yoa, aatg, sequel_id, claim_ref)]
  claims[, band := ceiling(incurred / band_size) * band_size - band_size / 2]
  claims[band < band_size / 2, band := band_size / 2]
  
  claims[, count := incurred / band]
  
  claims_freq <- claims[, .(count = sum(count)), by = .(yoa, aatg, band)]
  
  p <- rbind(aatg_curve[, .(yoa, aatg, band, count = freq, type = "expected")], 
             claims_freq[, .(yoa, aatg, band, count, type = "actual")])
  
  return(p)
  
}

ch <- odbcConnect("Pricing_Dev")

curve <- setDT(sqlQuery(ch, "SELECT [attachment]
                        ,[value]
                        ,[type]
                        FROM [fire_engine].[dbo].[fisrt_loss_curve_new]",stringsAsFactors = F))

odbcClose(ch)

memory.limit(size=16194)

p11 <- compare(2011, loc, curve, 25000)
gc()
p12 <- compare(2012, loc, curve, 25000)
gc()
p13 <- compare(2013, loc, curve, 25000)
gc()
p14 <- compare(2014, loc, curve, 25000)
gc()
p15 <- compare(2015, loc, curve, 25000)
gc()

p_yoa <- rbind(p11, p12, p13, p14, p15)

p <- p_yoa[, .(count = sum(count)), by = .(aatg, band, type)]

require(ggplot2)

ggplot(p, aes(x = band, y = count, colour = type)) + geom_line() + facet_grid(.~aatg) +
  scale_y_log10() + scale_x_log10()

p_avg <- p_yoa[, .(avg = sum(band * count) / sum(count)), by = .(aatg, type, yoa)]

ggplot(p_avg, aes(x = aatg, y = avg, fill = type)) + geom_bar(stat="identity", position = "dodge") + 
  facet_grid(.~yoa)

p_tot <- p_yoa[, .(avg = sum(band * count)), by = .(aatg, type, yoa)]

ggplot(p_tot, aes(x = aatg, y = avg, fill = type)) + geom_bar(stat="identity", position = "dodge") + 
  facet_grid(.~yoa)




