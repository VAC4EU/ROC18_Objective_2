############################################################################
#                                                          #
####   CREATE D5_Table_8_Impact_of_ablation_cohort_{ImmDis} ####
# 
#                                                          #
############################################################################

# author: Rosa Gini
# 
# v 1.1.0  11 Oct 2024
# 
# bugfix: fixed the formula of n_4_both_ (was equal to n_4_main_not_)   
# 
# v 1.0.0  30 Sep 2024

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  immdis <- "E_GRAVES_AESI"
  testname <- "test_05_T4_70_Table_8_impact_of_ablation_cohort"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  thisdirexp <- thisdiroutput
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c(immdis)
  thisgroupsofprompts <- c("all","PC","HOSP_DISP","HOSP_SPEC_DISP")
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirexp <- dirtablesubpop[[subpop]]
  thisgroupsofprompts <- groups_of_prompts_thisdatasource
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

true_groups_of_prompts <- thisgroupsofprompts[2:length(thisgroupsofprompts)]
other_groups_of_prompts <- setdiff(groups_of_prompts,thisgroupsofprompts)

# load input datasets to be used by all diseases

for (immdis in immune_diseases_in_this_step){
  
  print(paste0("Impact of ablation for ",immdis))
  
  cohort <- readRDS(file.path(thisdirinput,paste0("D3_cohort_",immdis,".rds")
))

  base_table8 <- data.table()

  base_table8[, ds := thisdatasource]
  
  ###########################
  # Persons with >= 1 code
  
  # in main
  base_table8[, n_1 := cohort[, .N]]
  
  # per group of prompt
  for (group in true_groups_of_prompts){
    base_table8[, (paste0("n_1_",group)) := cohort[get(paste0("has_not_a_code_in_the_study_period_",immdis,"_",group)) == 0, .N]]
    base_table8[, (paste0("p_1_",group)) := round(100*get((paste0("n_1_",group)))/n_1,1)]
  }
  for (group in other_groups_of_prompts) {
    base_table8[, (paste0("n_1_",group)) := NA_integer_ ]
    base_table8[, (paste0("p_1_",group)) := NA_integer_ ]
  }
  
  ###########################
  # Persons with 1st exclusion criterion
  
  namevar <- "exclude_because_exist_code_during_lookback_"
  
  # in main
  base_table8[, n_2 := cohort[get(paste0(namevar,immdis)) == 1, .N]]
  
  # per group of prompt
  for (group in true_groups_of_prompts){
    base_table8[, (paste0("n_2_",group)) := cohort[get(paste0(namevar,immdis,"_",group)) == 1, .N]]
  }
  for (group in other_groups_of_prompts) {
    base_table8[, (paste0("n_2_",group)) := NA_integer_ ]
  }
  
  ###########################
  # Persons with 2nd exclusion criterion
  
  namevar <- "exclude_because_exist_exclusion_criterion_during_lookback_"
  
  # in main
  base_table8[, n_3 := cohort[get(paste0(namevar,immdis)) == 1, .N]]
  
  # per group of prompt
  for (group in true_groups_of_prompts){
    base_table8[, (paste0("n_3_",group)) := cohort[get(paste0(namevar,immdis,"_",group)) == 1, .N]]
  }
  for (group in other_groups_of_prompts) {
    base_table8[, (paste0("n_3_",group)) := NA_integer_ ]
  }
  
  ###########################
  # Persons entering the cohort
  
  namevar <- "enter_cohort_"
  
  # in main
  base_table8[, n_4 := cohort[get(paste0(namevar,immdis)) == 1, .N]]
  
  # per group of prompt
  for (group in true_groups_of_prompts){
    base_table8[, (paste0("n_4_",group)) := cohort[get(paste0(namevar,immdis,"_",group)) == 1, .N]]
    base_table8[, (paste0("n_4_main_not_",group)) := cohort[get(paste0(namevar,immdis)) == 1 & (is.na(get(paste0(namevar,immdis,"_",group))) | get(paste0(namevar,immdis,"_",group)) == 0), .N]]
  base_table8[, (paste0("p_4_main_not_",group)) := round(100*get((paste0("n_4_main_not_",group)))/n_4,1)]
  base_table8[, (paste0("n_4_both_",group)) := cohort[get(paste0(namevar,immdis)) == 1 & (is.na(get(paste0(namevar,immdis,"_",group))) | get(paste0(namevar,immdis,"_",group)) == 1), .N]]
  base_table8[, (paste0("p_4_both_",group)) := round(100*get((paste0("n_4_both_",group)))/n_4,1)]
}

  for (group in other_groups_of_prompts) {
    base_table8[, (paste0("n_4_",group))  := NA_integer_]
    base_table8[, (paste0("n_4_main_not_",group)) := NA_integer_]
    base_table8[, (paste0("p_4_main_not_",group)) :=  NA_integer_]
    base_table8[, (paste0("n_4_both_",group))  := NA_integer_ ]
    base_table8[, (paste0("p_4_both_",group))  := NA_integer_ ]
  }
  
  ###########################
  # time between study start and cohort entrance
  
  namevar <- "cohort_entry_date_"
  
  cohort[, (paste0("time_to_entry_",immdis)) := as.integer(get(paste0(namevar,immdis)) - as.Date(study_start))]
  # per group of prompt
  for (group in true_groups_of_prompts){
    cohort[, (paste0("time_to_entry_",immdis,"_",group)) := as.integer(get(paste0(namevar,immdis,"_",group)) - as.Date(study_start))]
  }
  for (group in other_groups_of_prompts) {
    cohort[, (paste0("time_to_entry_",immdis,"_",group))  := NA_integer_ ]
  }
  
  namevar <- "time_to_entry_"
  
  # in main
  base_table8[, (paste0("median_",namevar)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                       cohort[,quantile(get(paste0(namevar,immdis)),.5, na.rm = T)], NA_integer_)]
  base_table8[, (paste0("p25_",namevar)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                    cohort[,quantile(get(paste0(namevar,immdis)),.25, na.rm = T)], NA_integer_)]
  base_table8[, (paste0("p75_",namevar)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                    cohort[,quantile(get(paste0(namevar,immdis)),.75, na.rm = T)], NA_integer_)]
  
  # per group of prompt
  for (group in true_groups_of_prompts){
    base_table8[, (paste0("median_",namevar,group)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                               cohort[,quantile(get(paste0(namevar,immdis,"_",group)),.5, na.rm = T)], NA_integer_)]
    base_table8[, (paste0("p25_",namevar,group)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                            cohort[,quantile(get(paste0(namevar,immdis,"_",group)),.25, na.rm = T)], NA_integer_)]
    base_table8[, (paste0("p75_",namevar,group)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                            cohort[,quantile(get(paste0(namevar,immdis,"_",group)),.75, na.rm = T)], NA_integer_)]
  }
  for (group in other_groups_of_prompts) {
    base_table8[, (paste0("median_",namevar,group)) := NA_integer_ ]
    base_table8[, (paste0("p25_",namevar,group)) := NA_integer_ ]
    base_table8[, (paste0("p75_",namevar,group)) := NA_integer_ ]  
  }
  
  ###########################
  # delay
  
  namevar <- "time_to_entry_"
  
  # per group of prompt
  for (group in true_groups_of_prompts){
    cohort[, (paste0("delay_",immdis,"_",group)) := as.integer(get(paste0(namevar,immdis,"_",group)) - get(paste0(namevar,immdis)))]
  }
  for (group in other_groups_of_prompts) {
    base_table8[, (paste0("delay_",immdis,"_",group)) := NA_integer_ ]
  }
  
  namevar <- "delay_"
  
  # per group of prompt
  for (group in true_groups_of_prompts){
    base_table8[, (paste0("median_",namevar,group)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                               cohort[,quantile(get(paste0(namevar,immdis,"_",group)),.5, na.rm = T)], NA_integer_)]
    base_table8[, (paste0("p25_",namevar,group)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                            cohort[,quantile(get(paste0(namevar,immdis,"_",group)),.25, na.rm = T)], NA_integer_)]
    base_table8[, (paste0("p75_",namevar,group)) := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                                            cohort[,quantile(get(paste0(namevar,immdis,"_",group)),.75, na.rm = T)], NA_integer_)]
  }
  for (group in other_groups_of_prompts) {
    base_table8[, (paste0("median_",namevar,group)) := NA_integer_]
    base_table8[, (paste0("p25_",namevar,group)) := NA_integer_]
    base_table8[, (paste0("p75_",namevar,group)) := NA_integer_ ]
  }

  listvar <- grep("^n_", names(base_table8), value = TRUE)
  listvar_p <- grep("^p_", names(base_table8), value = TRUE)
  
  if (thisdatasource %in% datasources_stricter_masking) {
    base_table8 <- mask_variables_per_threshold(base_table8, listvar, threshold[[thisdatasource]])
    base_table8 <- mask_variables_per_threshold(base_table8, listvar_p, threshold[[thisdatasource]])
  } else {
    base_table8 <- mask_variables_per_threshold(base_table8, listvar, threshold[[thisdatasource]])
  }
  
  base_table8[, threshold := threshold[[thisdatasource]]]
  
  ########################################
  # save

  outputfile <- base_table8
  nameoutput <- paste0("D5_Table_8_Impact_of_ablation_cohort_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  fwrite(outputfile, file = file.path(thisdirexp, paste0(nameoutput,".csv")))
    
}


