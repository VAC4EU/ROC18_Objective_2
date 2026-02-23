############################################################
#                                                          #
####   CREATE D3_flares_{ImmDis} ####
# 
#                                                          #
############################################################

# author: Rosa Gini
# 
# v 1.0.0  23 Sep 2024

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  immdis <- "E_GRAVES_AESI"
  testname <- "test_03_T2_30_create_dates_flares"
  thisdirinput <- file.path(dirtest,testname)
  thisdirconceptsets <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c(immdis)
}else{
  thisdirinput <- substr(dirtemp, 1, nchar(dirtemp)-1)
  thisdiroutput <- substr(dirtemp, 1, nchar(dirtemp)-1)
  thisdirconceptsets <- substr(dirconceptsets, 1, nchar(dirconceptsets)-1)
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

# load input datasets to be used by all diseases: none

for (immdis in immune_diseases_in_this_step){
  
  components_flare <- readRDS(file.path(thisdirinput,paste0("D3_components_flare_TD_", immdis,".rds")))
  
  
  outputfile <- unique(components_flare[!is.na(date_flare),.(person_id,date_flare)])
  
  setnames(outputfile,"date_flare",paste0("date_flare_",immdis))
  
  
  ########################################
  # save
  
  # outputfile <- TD
  nameoutput <- paste0("D3_flares_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  rm(components_flare)

    
}

