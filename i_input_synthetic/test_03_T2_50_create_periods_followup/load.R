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

listdatasetsRData <- "D3_all_vaccines_curated"
listdatasets <- c("D3_all_vaccines_curated", "D4_source_population",
                  "D3_cohort_E_GRAVES_AESI", "D3_flares_E_GRAVES_AESI")

# dates variables 

listdates <- list()

listdates[["D3_all_vaccines_curated"]] <- c("date_curated")
listdates[["D4_source_population"]] <- c("birth_date", "death_date", "spell_start_date", "study_entry_date", "study_exit_date")
listdates[["D3_cohort_E_GRAVES_AESI"]] <- c("study_entry_date", "study_exit_date", "cohort_entry_date_E_GRAVES_AESI", "start_follow_up_E_GRAVES_AESI")
listdates[["D3_flares_E_GRAVES_AESI"]] <- c("date_flare_E_GRAVES_AESI")

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
  data <- as.data.table(readxl::read_excel(paste0(thisdir, "/", namedataset, ".xlsx")))
  
  for (datevar in listdates[[namedataset]]) {
    if (!is.na(baseline[[namedataset]][[datevar]])){
    #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
    }else{
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset,data)
  if (namedataset %in% listdatasetsRData){
    save(data, file = file.path(thisdir, paste0(namedataset,".RData")), list = namedataset)
  }else{
  saveRDS(data, file = file.path(thisdir, paste0(namedataset,".rds")))
  }
}

