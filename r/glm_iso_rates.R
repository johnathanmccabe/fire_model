##
#
# GLM model used on ISO base rates to produce relativity factors
#
#
#
##



library(data.table)
library(openxlsx)
library(Faraday.Pricing)

# data <- openxlsx::read.xlsx("U:/FRE/Actuarial/2016 Work/Fire/ISO Data.xlsm",
#                             sheet = "Results")


data <- fread("U:/FRE/Actuarial/2016 Work/Pricing Ultimates/Q2 2016/Comm Lines/iso_base_rate.csv")

## clean up column names
clean_column_names <- function(column_names){
  colnames <- gsub(x = column_names, pattern = "[^0-9a-zA-Z]", replacement = "")
  colnames <- gsub(x = colnames, pattern = "([[:lower:]])([[:upper:]])", replacement = "\\1_\\2" )
  colnames <- gsub(x = colnames,
                   pattern = "([[:upper:]])([[:upper:]])([[:lower:]])",
                   replacement = "\\1_\\2\\3" )
  colnames <- tolower(colnames)

  return(colnames)
}

colnames(data) <- clean_column_names(colnames(data))


data <- data %>%
  mutate_each(funs(as.factor), state, coverage, construction, occupancy)

glm.model <- glm(data = data, formula = rate ~ state + coverage + construction + occupancy,
                 family = gaussian(link = "log"))



# see http://stackoverflow.com/questions/32035119/how-to-solve-clipboard-buffer-is-full-and-output-lost-error-in-r-running-in-wi
write.table(names(exp(glm.model$coefficients)), "clipboard-16384", sep="\t", row.names=FALSE)
write.table(exp(glm.model$coefficients), "clipboard-16384", sep="\t", row.names=FALSE)



unique(data$state)
