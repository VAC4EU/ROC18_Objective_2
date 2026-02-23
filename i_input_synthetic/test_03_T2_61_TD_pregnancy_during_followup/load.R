rm(list=ls(all.names=TRUE))


baselinedate <- 20170101

immdis <- "E_GRAVES_AESI"

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

# list of datasets

listdatasets <- c("D3_pregnancy_while_in_followup_periods_in_cohort_E_GRAVES_AESI")

# dates variables 

listdates <- list()


listdates[["D3_pregnancy_while_in_followup_periods_in_cohort_E_GRAVES_AESI"]] <- c("pregnancy_start_date", "pregnancy_end_date", "start_period_E_GRAVES_AESI_d", "end_period_E_GRAVES_AESI_d", "start_period_in_this_pregnancy_E_GRAVES_AESI_d", "end_period_in_this_pregnancy_E_GRAVES_AESI_d")

# date baseline

baseline <- vector(mode="list")
for (namedataset in listdatasets) {
  for (datevar in listdates[[namedataset]]) {
    baseline[[namedataset]][[datevar]] <- as.Date(lubridate::ymd(baselinedate))
  }
}


# load datasets


for (namedataset in listdatasets){
  # data <- fread(paste0(thisdir, "/", namedataset, ".csv") )
  # data[, (listdates[[namedataset]]) := lapply(.SD, lubridate::ymd), .SDcols = listdates[[namedataset]]]
  data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
  for (datevar in listdates[[namedataset]]) {
    if (!is.na(baseline[[namedataset]][[datevar]])){
    #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
    }else{
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset,data)
  saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
}

