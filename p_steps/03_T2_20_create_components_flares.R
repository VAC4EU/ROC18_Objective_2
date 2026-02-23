############################################################
#                                                          #
####   CREATE D3_components_flares_{ImmDis} ####
# 
#                                                          #
############################################################

# author: Rosa Gini, Davide Messina
# 
# v 1.2.0  1 Oct 2024
#
# updated names of TP conceptsets (based on updated BRIDGE)
# 
# v 1.1.0  26 Sep 2024
#
# bugfix: a variable was named wrongly in the test data 
# 
# v 1.0.0  23 Sep 2024

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  # immdis <- "E_GRAVES_AESI"
  # immdis <- "Im_HASHIMOTO_AESI"
  # immdis <- "D_HEPATITISAUTOIMMUNE_AESI"
  # immdis <- "V_PAN_AESI"
  # immdis <- "M_ARTRHEU_AESI"
  # immdis <- "M_ARTPSORIATIC_AESI"
  immdis <- "N_DEMYELMS_AESI"
  # immdis <- "SK_ERYTHEMANODOSUM_AESI"
  # immdis <- "Im_SLE_AESI"
  # immdis <- "D_ULCERATIVECOLITIS_AESI"
  testname <- paste0("test_D3_components_flare_TD_",immdis)
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
  TP_vars <- unique(unlist(flare_components))[grepl("^TP_", unique(unlist(flare_components)))]

  for (TP_var in c(paste0(TP_vars, "_narrow"), "C_PERICARDITISRA_AESI_narrow")) {
    if (!file.exists(file.path(thisdirconceptsets, paste0(TP_var, ".RData")))) {
      tmp <- get(load(file.path(thisdir, "p_parameters", "archive_parameters",
                     paste0("Empty_TP", ".RData")))[[1]])
      assign(TP_var, tmp)
      save(TP_var, file = file.path(thisdirconceptsets, paste0(TP_var, ".RData")), list = TP_var)
      rm(list = TP_var)
    }
  }
}

# load input datasets to be used by all diseases: none

for (immdis in immune_diseases_in_this_step){
  
  print(paste("Prepare the building blocks to calculate the flares of",immdis))

  # classify the building blocks that are medicines, procedures, EMG or HOSP
  
  list_of_medicines <- flare_components[[immdis]][grep("^DP_",flare_components[[immdis]])]
  
  list_of_procedures <- flare_components[[immdis]][grep("^TP_",flare_components[[immdis]])]
  
  list_of_building_blocks <- setdiff(flare_components[[immdis]], c(flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])],flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]))
  
    
  #########################################
  # load disease-specific input datasets
  
  # load the cohort and restrict to those entering followup
  D3_cohort <- readRDS(file.path(thisdirinput, paste0("D3_cohort_",immdis,".rds") ))
  D3_cohort <- D3_cohort[ get(paste0("entering_follow_up_",immdis)) == 1,]
  tokeep <- c("person_id",paste0("cohort_entry_date_",immdis),paste0("start_follow_up_",immdis),"study_exit_date")
  D3_cohort <- D3_cohort[,..tokeep]
  D3_cohort[,in_study := 1]
  
  # load the file of immdis diagnosis and select those happened to people entering followup, after entrance in followup
  correct_concept_name <- data.table::fcase(immdis == "Im_HASHIMOTO_AESI_sensitivity", "Im_HASHIMOTO_AESI",
                                            immdis == "N_DEMYELMS_AESI_sensitivity", "N_DEMYELMS_AESI",
                                            default = immdis)
  immdisdiag <- get(load(file.path(thisdirconceptsets, paste0(correct_concept_name,"_narrow.RData")))[[1]])
  immdisdiag <- immdisdiag[,.(person_id,date,meaning_renamed)]
  immdisdiag <- merge(D3_cohort, immdisdiag, by = c("person_id"))[date >= get(paste0("cohort_entry_date_",immdis)) ,.(person_id,date,meaning_renamed)]

  # set up 1st common building block: hospitalization for immdis in primary diagnosis
  hosp <- immdisdiag[eval(parse(text = select_in_component[["Disease_H1"]])),]
  assign(flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])],hosp)
  
  # set up 2nd common building block: emergency room for immdis
  emg <- immdisdiag[eval(parse(text = paste(select_in_component[["Disease_ER"]]," | ", select_in_component[["Disease_ER_PC"]]) )),]
  assign(flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])],emg)
  
  # set up all the other elements listed in the parameter flare_components retrieved from the BRIDGE in the configuration file 07_algorithms (names of medicines and procedures are stored in separate lists): load all of the and restrict to persons entering followup. in some rare case, the building block is not  acodelist itself but there is a corresponding list of codelists in codelists_of_bblock
  
  for (buildingblock in list_of_building_blocks){
    if (length(codelists_of_bblock[[buildingblock]]) == 0){
      listbblocks <- buildingblock
    }else{
      listbblocks <- codelists_of_bblock[[buildingblock]]
    }
    
    for (bblock in listbblocks){
      suff <- fifelse(bblock %in% c(list_of_medicines),'','_narrow')
      if (bblock %in% c(list_of_medicines) & immdis %in% c("V_PAN_AESI", "M_ARTRHEU_AESI","M_ARTPSORIATIC_AESI","Im_SLE_AESI")){
        listvar <- c("person_id","date","meaning_renamed","codvar")
      }else{
        listvar <- c("person_id","date","meaning_renamed")
      }
      
      if (bblock %in% CONCEPTSETS_to_be_split) {
        list_concepts_to_load <- list.files(thisdirconceptsets)[grepl(paste0("^", bblock, "_", suff, ".*RData$"), list.files(thisdirconceptsets))]
        
        temp <- data.table()
        print(paste0(bblock, "/", list_concepts_to_load))
        for (single_file in list_concepts_to_load) {
          temp_1 <- get(load(file.path(thisdirconceptsets, single_file))[[1]])[,..listvar]
          rm(list = paste0(bblock,suff))
          
          print(paste(immdis,"- listvar:",listvar))
          
          if (nrow(temp_1) > 0){
            temp_1 <- merge(D3_cohort, temp_1, by = c("person_id"))[date >= get(paste0("cohort_entry_date_",immdis)) ,..listvar]
            temp <- rbind(temp, temp_1, fill = T)
            rm(temp_1)
          }
          
        }
        
        if (nrow(temp) > 0) {
          temp <- get(load(file.path(thisdirconceptsets, single_file))[[1]])[,..listvar]
        }
        
        assign(bblock,temp)
      } else {
        name_bblock_df <- load(file.path(thisdirconceptsets, paste0(bblock,suff,".RData")))[[1]]
        temp <- get(name_bblock_df)
        temp <- temp[,..listvar]
        print(paste(immdis,"- listvar:",listvar))
        
        if (nrow(temp) > 0){
          temp <- merge(D3_cohort, temp, by = c("person_id"))[date >= get(paste0("cohort_entry_date_",immdis)) ,..listvar]
        }
        
        # TODO remove in future version
        if (grepl("^TP_", bblock)){
          temp <- temp[, date := ymd(date)]
        }
        
        
        rm(list = name_bblock_df)
        assign(bblock, temp)
      }
      
      
      rm(temp)
    }
  }

  #########################################
  # process all the elements to obtain the building blocks of the flares: this procedure is tailored to each immdis
  # source(paste0(dirmacro, "GenerateTDDataset.R"))

  print(paste("Process the building blocks of the flares of",immdis))
  
  if (immdis == "E_GRAVES_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := start_follow_up_E_GRAVES_AESI]
    cohort[, end_record_d := study_exit_date]
    
    # records of DP_ANTITHYROID last 90 days: set this and merge subsequent episodes 
    DP <- DP_ANTITHYROID
    DP[, end := date + 90]
    merged_episodes <- CreateSpells(
      dataset = DP,
      id = "person_id",
      start_date = "date",
      end_date = "end"
    )
    rm(DP)
    setnames(merged_episodes,c("entry_spell_category","exit_spell_category" ),c("start_record_d","end_record_d"))
    merged_episodes <- merged_episodes[,.(person_id,start_record_d,end_record_d)]
    
    # create TD dataset where the first day of a DP_ANTITHYROID episode is recorded in last_DP_ANTITHYROID, and the subsequent days are marked as covered by a DP_ANTITHYROID treatment in the boolean bool_DP_ANTITHYROID_90_days. this requires 2 steps of GenerateTDDataset
    
    start_DP <- copy(merged_episodes)
    start_DP[, end_record_d := start_record_d]
    start_DP[, start_episode := 1]
    
    merged_episodes[, episode := 1]
    
    print(paste(immdis,"- DP_ANTITHYROID"))
    DP <- GenerateTDDataset(datasets = list(merged_episodes,start_DP),
                            UoO_vars = c("person_id","person_id"),
                            start_d_vars = c("start_record_d","start_record_d"),
                            end_d_vars = c("end_record_d","end_record_d"),
                            keep_auxiliary_variables = F,
                            TD_variables = list(list("episode"),list("start_episode")),
                            keep_periods_observed_by = "first",
                            default_value_for_unobserved = list(
                              "start_episode" = 0
                            )
                              )
  rm(merged_episodes,start_DP)
  
  # DP[, bool_DP_ANTITHYROID_90_days := fifelse(episode == 1 & start_episode == 0,1,0)]
  setnames(DP,"episode","bool_DP_ANTITHYROID_90_days")
  DP[start_episode == 1 , start_DP_ANTITHYROID := start_record_d]
  DP <- DP[, .(person_id,start_record_d,end_record_d,bool_DP_ANTITHYROID_90_days,start_DP_ANTITHYROID)]
  
  print(paste0(immdis," - last_DP_ANTITHYROID"))

  TD <- GenerateTDDataset(
    datasets = list(cohort,DP),
    UoO_vars = c("person_id","person_id"),
    start_d_vars = c("start_record_d","start_record_d"),
    end_d_vars = c("end_record_d","end_record_d"),
    keep_auxiliary_variables = F,
    TD_variables = list(list("in_study"),list("bool_DP_ANTITHYROID_90_days","start_DP_ANTITHYROID")),
    keep_periods_observed_by = "first",
    default_value_for_unobserved = list(
      "bool_DP_ANTITHYROID_90_days" = 0
    ),
    TD_variables_with_definite_value_until_unobserved = c("start_DP_ANTITHYROID")
    
  )  
  rm(cohort,DP)
  
  setnames(TD,"start_DP_ANTITHYROID","last_DP_ANTITHYROID")

  
  # create TD dataset where the first day of a DP_ANTITHYROID episode is recorded in start_DP_ANTITHYROID, and the subsequent days are marked as covered by a DP_ANTITHYROID treatment in the boolean bool_DP_ANTITHYROID_90_days
  
  # for TP_THYROIDECTOMY_AESI TP_RADIOIODINEABLA_AESI: the event is only on if bool_DP_ANTITHYROID_90_days == 0, so we need to merge the datasets as TD dataset, remove events that overlap bool_DP_ANTITHYROID_90_days == 0, and store in last_... the surviving dates

  TDvar_first_dataset = list("bool_DP_ANTITHYROID_90_days","last_DP_ANTITHYROID")
  for (bblock in c("TP_THYROIDECTOMY_AESI", "TP_RADIOIODINEABLA_AESI"
  ) ){
    print(paste0(immdis," - last_",bblock))
    # print(TDvar_first_dataset)
    temp <- get(bblock)
    if (nrow(temp > 0)){
      setnames(temp,"date","start_record_d")
      temp[, end_record_d := start_record_d]
      temp[, (paste0("last_",bblock)) := start_record_d]
      setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
      TD <- GenerateTDDataset(
        datasets = list(TD,temp),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
        keep_periods_observed_by = "first",
        default_value_for_unobserved = list(
          "bool_DP_ANTITHYROID_90_days" = 0
        ),
        TD_variables_with_definite_value = c("last_DP_ANTITHYROID")
      )  
      # remove dates that happen when ANTITHYROID treatment is ongoing
      TD[bool_DP_ANTITHYROID_90_days == 1, (paste0("last_",bblock)) := as.Date(NA)]
    }else{
      TD[, (paste0("last_",bblock)) := as.Date(NA)]
      TD[, (paste0("meaning_",bblock)) := ""]
    }
    TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
  }
  
 
  
  # now add dates that do not need exclusion during treatment
  for (bblock in c("O_FLARE_AESI", "E_THYROIDSTORM_AESI", "EMG_GD", "HOSP_GD") ){
    print(paste0(immdis," - ", bblock))
    temp <- get(bblock)
    if (nrow(temp > 0)){
      setnames(temp,"date","start_record_d")
      temp[, end_record_d := start_record_d]
      temp[, (paste0("last_",bblock)) := start_record_d]
      setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
      # saveRDS(TD,file = "C:/temp/TD.rds")
      # saveRDS(temp,file = "C:/temp/temp.rds")
      TD <- GenerateTDDataset(
        datasets = list(TD,temp),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
        keep_periods_observed_by = "first",
        default_value_for_unobserved = list(
          "bool_DP_ANTITHYROID_90_days" = 0
        ),
        TD_variables_with_definite_value = c("last_DP_ANTITHYROID", names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
      )  
    }else{
      TD[, (paste0("last_",bblock)) := as.Date(NA)]
      TD[, (paste0("meaning_",bblock)) := ""]
    }
    TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
  }
  
  # generate components of flares for E_GRAVES_AESI
  
  TD[, date_flare_Flare := last_O_FLARE_AESI]
  TD[, date_flare_Diagnoses := pmax(last_E_THYROIDSTORM_AESI, na.rm = T)]
  TD[, date_flare_Medications := pmax(last_DP_ANTITHYROID, na.rm = T)]
  TD[, date_flare_Procedures := pmax(last_TP_THYROIDECTOMY_AESI,last_TP_RADIOIODINEABLA_AESI, na.rm = T)]
  TD[, date_flare_Emergency := pmax(last_EMG_GD, na.rm = T)]
  TD[, date_flare_Hosp := pmax(last_HOSP_GD, na.rm = T)]
  }
  
  if (immdis %in% c("Im_HASHIMOTO_AESI", "Im_HASHIMOTO_AESI_sensitivity")){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]

    print(paste(immdis,"- DP_LEVOTHYROXINE"))
    
    # select the first episode of treatment
    if(nrow(DP_LEVOTHYROXINE) > 0){
      setkey(DP_LEVOTHYROXINE,person_id, date)
      DP <- unique(DP_LEVOTHYROXINE[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
    }else{
      DP <- DP_LEVOTHYROXINE
    }
    treatment <- "DP_LEVOTHYROXINE"
    DP[,start_record_d:= date]
    DP[,end_record_d:= date]
    setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))

    # create TD dataset with the first episode of treatment
    
    TD <- GenerateTDDataset(
      datasets = list(cohort,DP),
      UoO_vars = c("person_id","person_id"),
      start_d_vars = c("start_record_d","start_record_d"),
      end_d_vars = c("end_record_d","end_record_d"),
      keep_auxiliary_variables = F,
      TD_variables = list(list("in_study"),list(paste0("last_",treatment),paste0("meaning_",treatment))),
      keep_periods_observed_by = "first",
      TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
      
    )  
    rm(cohort,DP)
    
    # add to the TD dataset all events from flares, EMG and HOSP
    
    TDvar_first_dataset <- list(paste0("last_",treatment))
  
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    
    for (bblock in c("O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      # print(paste("list TD var",list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock)))))
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        # saveRDS(TD,file = "C:/temp/TD.rds")
        # saveRDS(temp,file = "C:/temp/temp.rds")
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(paste0("last_",treatment), names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )  
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    } 
    
    # generate components for flares of Im_HASHIMOTO_AESI
    
    TD[, date_flare_Flare := last_O_FLARE_AESI]
    TD[, date_flare_Diagnoses := as.Date(NA)]
    TD[, date_flare_Medications := pmax(get(paste0("last_",treatment))
, na.rm = T)]
    TD[, date_flare_Procedures := as.Date(NA)]
    TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
    TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis == "D_HEPATITISAUTOIMMUNE_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]


    # select the first episode of treatment for all treatments in
  list_of_medicines

    TDvar_first_dataset <- list("in_study")
    for (treatment in list_of_medicines){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))

      # create TD dataset with the first episode of treatment

      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))

      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))

      }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort

    for (bblock in c("O_FLARE_AESI", thisemg, thishosp, "I_HEPATITIS_COV", "D_ALCOHOLICLIVER_COV") ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }

    # generate components of flares for D_HEPATITISAUTOIMMUNE_AESI

    TD[, date_flare_Flare := last_O_FLARE_AESI]
    TD[, date_flare_Diagnoses := pmax(last_I_HEPATITIS_COV, last_D_ALCOHOLICLIVER_COV, na.rm = T)]

    TD[, date_flare_Medications := as.Date(NA)]
    for (treatment in list_of_medicines){
      TD[, date_flare_Medications := pmax(date_flare_Medications, get(paste0("last_",treatment)), na.rm = T)]
      }

    TD[, date_flare_Procedures := as.Date(NA)]
    TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
    TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis == "V_PAN_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]
    
    
    # select the first episode of treatment for all treatments in list_of_medicines, but medicines are each separate ATC
    
    list_of_medicines <- c("DP_IMMUNOSUPRESSANTSONLYPAN")
    TDvar_first_dataset <- list("in_study")
    for (treatment in list_of_medicines){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id","codvar")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      DP[,codvar := NULL]
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
      
    }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort
    
    for (bblock in c("O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }
    # generate components flares V_PAN_AESI

    TD[, date_flare_Flare := last_O_FLARE_AESI]
    TD[, date_flare_Diagnoses := as.Date(NA)]
    TD[, date_flare_Medications := pmax(get(paste0("last_",treatment))
                                        , na.rm = T)]
    TD[, date_flare_Procedures := as.Date(NA)]
    TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
    TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis == "M_ARTRHEU_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]

    
    # records of M_RACOMPLICATION_AESI last 90 days: set this and merge subsequent episodes 
    
    print(paste(immdis,"- M_RACOMPLICATION_AESI"))
    DIAG <- data.table()
    for (codelist in codelists_of_bblock[["M_RACOMPLICATION_AESI"]]){
      temp <- get(codelist) 
      DIAG <- rbind(DIAG,temp)
    } 
      
    DIAG[, end := date + 90]
    DIAG <- CreateSpells(
      dataset = DIAG,
      id = "person_id",
      start_date = "date",
      end_date = "end"
    )
    setnames(DIAG,c("entry_spell_category" ),c("start_record_d"))
    DIAG[, end_record_d := start_record_d]
    DIAG[, last_M_RACOMPLICATION_AESI := start_record_d]
    DIAG <- DIAG[,.(person_id,start_record_d,end_record_d, last_M_RACOMPLICATION_AESI)]
    
    # merge the record to the cohort
    
    cohort <- GenerateTDDataset(
      datasets = list(cohort,DIAG),
      UoO_vars = c("person_id","person_id"),
      start_d_vars = c("start_record_d","start_record_d"),
      end_d_vars = c("end_record_d","end_record_d"),
      keep_auxiliary_variables = F,
      TD_variables = list(list("in_study"),list("last_M_RACOMPLICATION_AESI")),
      keep_periods_observed_by = "first",
      TD_variables_with_definite_value = c("last_M_RACOMPLICATION_AESI")
    )  
    rm(DIAG)    

    # a flare is the first episode of treatment for DP_DMARD - for each separate ATC
    
    print(paste(immdis,"- DP_DMARD"))
    TDvar_first_dataset <- list("in_study","last_M_RACOMPLICATION_AESI")
    for (treatment in c("DP_DMARD")){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id","codvar")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      DP[,codvar := NULL]
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
      
    }
    
    # a flare is the first episode of treatment in DP_NSAIDRA or DP_CORTICOST
    
    # TDvar_first_dataset <- list("in_study")
    for (treatment in c("DP_NSAIDRA","DP_CORTICOST")){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      temp <- temp[,.(person_id,date,meaning_renamed)]
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))

      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
    }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort
    
    for (bblock in c("O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }

    # # generate components of flares for M_ARTRHEU_AESI
    
    TD[, date_flare_Flare := last_O_FLARE_AESI]
    TD[, date_flare_Diagnoses := last_M_RACOMPLICATION_AESI]
    TD[, date_flare_Medications := as.Date(NA)]
    for (treatment in list_of_medicines){
      TD[, date_flare_Medications := pmax(date_flare_Medications, get(paste0("last_",treatment)), na.rm = T)]
    }
    TD[, date_flare_Procedures := as.Date(NA)]
    TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
    TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis == "M_ARTPSORIATIC_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]

    # a flare is the first episode of treatment for DP_DMARDPSOARTH - for each separate ATC
    
    TDvar_first_dataset <- list("in_study")
    for (treatment in c("DP_DMARDPSOARTH")){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id","codvar")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      DP[,codvar := NULL]
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
      
    }
    
    
    # a flare is the first episode of treatment in DP_NSAIDPSOARTH or DP_CORTICOSTPSOARTH
    
    # TDvar_first_dataset <- list("in_study")
    for (treatment in c("DP_NSAIDPSOARTH","DP_CORTICOSTPSOARTH")){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      temp <- temp[,.(person_id,date,meaning_renamed)]
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
    }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort
    
    for (bblock in c("O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }

    # generate components for flares of M_ARTPSORIATIC_AESI

    TD[, date_flare_Flare := last_O_FLARE_AESI]
    TD[, date_flare_Diagnoses := as.Date(NA)]
    TD[, date_flare_Medications := as.Date(NA)]
    for (treatment in list_of_medicines){
      TD[, date_flare_Medications := pmax(date_flare_Medications, get(paste0("last_",treatment)), na.rm = T)]
    }
    TD[, date_flare_Procedures := as.Date(NA)]
    TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
    TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis %in% c("N_DEMYELMS_AESI", "N_DEMYELMS_AESI_sensitivity")){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]

    # a flare is the first episode of treatment in list_of_medicines
    
    TDvar_first_dataset <- list("in_study")
    for (treatment in list_of_medicines){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      temp <- temp[,.(person_id,date,meaning_renamed)]
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
    }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort
    
    for (bblock in c("O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }
    
    # generate components for flares of N_DEMYELMS_AESI
    TD[, date_flare_Flare := last_O_FLARE_AESI]
    TD[, date_flare_Diagnoses := as.Date(NA)]
    TD[, date_flare_Medications := as.Date(NA)]
    for (treatment in list_of_medicines){
      TD[, date_flare_Medications := pmax(date_flare_Medications, get(paste0("last_",treatment)), na.rm = T)]
    }
    TD[, date_flare_Procedures := as.Date(NA)]
    TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
    TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis == "SK_ERYTHEMANODOSUM_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]

    # a flare is the first episode of treatment in list_of_medicines
    
    TDvar_first_dataset <- list("in_study")
    for (treatment in list_of_medicines){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      temp <- temp[,.(person_id,date,meaning_renamed)]
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
    }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort
    
    for (bblock in c("Sk_PANNICULITIS_AESI", "TP_DEBRIDEMENTEN_AESI", "O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }
    
    # generate components for flares of SK_ERYTHEMANODOSUM_AESI

    TD[, date_flare_Flare := last_O_FLARE_AESI]
    TD[, date_flare_Diagnoses := last_Sk_PANNICULITIS_AESI]
    TD[, date_flare_Medications := as.Date(NA)]
    for (treatment in list_of_medicines){
      TD[, date_flare_Medications := pmax(date_flare_Medications, get(paste0("last_",treatment)), na.rm = T)]
    }
    
    TD[, date_flare_Procedures := last_TP_DEBRIDEMENTEN_AESI]
    TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
    TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis == "Im_SLE_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]


    # a flare is the first episode of treatment for DP_DMARDSLE - for each separate ATC
    
    TDvar_first_dataset <- list("in_study")
    for (treatment in c("DP_DMARDSLE")){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id","codvar")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      DP[,codvar := NULL]
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
      
    }
    
    
    # a flare is the first episode of treatment in "DP_CORTICOSTSLE", "DP_IMMUNOSUPRESSANTSLE", "DP_ANIFROLUMAB", "DP_BELIMUMAB", "DP_RITUXIMAB", "DP_CICLOSPORIN", "DP_VOCLOSPORIN", "DP_CYCLOPHOSPHAMIDE", "DP_NSAIDSLE"
    
    # TDvar_first_dataset <- list("in_study")
    for (treatment in c("DP_CORTICOSTSLE", "DP_IMMUNOSUPRESSANTSLE", "DP_ANIFROLUMAB", "DP_BELIMUMAB", "DP_RITUXIMAB", "DP_CICLOSPORIN", "DP_VOCLOSPORIN", "DP_CYCLOPHOSPHAMIDE", "DP_NSAIDSLE")){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      temp <- temp[,.(person_id,date,meaning_renamed)]
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
    }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort
    
    for (bblock in c("O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }
    
  # generate components for flares of Im_SLE_AESI

  TD[, date_flare_Flare := last_O_FLARE_AESI]
  TD[, date_flare_Diagnoses := as.Date(NA)]
  TD[, date_flare_Medications := as.Date(NA)]
  for (treatment in list_of_medicines){
  TD[, date_flare_Medications := pmax(date_flare_Medications, get(paste0("last_",treatment)), na.rm = T)]
  }
  TD[, date_flare_Procedures := as.Date(NA)]
  TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
  TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  if (immdis == "D_ULCERATIVECOLITIS_AESI"){
    # create a copy of the cohort that will be used across the computation
    cohort <- copy(D3_cohort)
    cohort[, start_record_d := get(paste0("start_follow_up_",immdis))]
    cohort[, end_record_d := study_exit_date]

    
    # a flare is the first episode of treatment in list_of_medicines
    
    TDvar_first_dataset <- list("in_study")
    for (treatment in list_of_medicines){
      print(paste(immdis,"-",treatment))
      temp <- get(treatment)
      temp <- temp[,.(person_id,date,meaning_renamed)]
      if(nrow(temp) > 0){
        setkey(temp,person_id, date)
        DP <- unique(temp[,`:=` (date = date[1], meaning_renamed = meaning_renamed[1]),by = c("person_id")])
      }else{
        DP <- temp
      }
      DP[,start_record_d:= date]
      DP[,end_record_d:= date]
      setnames(DP,c("date","meaning_renamed"),c(paste0("last_",treatment),paste0("meaning_",treatment)))
      
      # create TD dataset with the first episode of treatment
      
      cohort <- GenerateTDDataset(
        datasets = list(cohort,DP),
        UoO_vars = c("person_id","person_id"),
        start_d_vars = c("start_record_d","start_record_d"),
        end_d_vars = c("end_record_d","end_record_d"),
        keep_auxiliary_variables = F,
        TD_variables = list(TDvar_first_dataset,list(paste0("last_",treatment),paste0("meaning_",treatment))),
        keep_periods_observed_by = "first",
        TD_variables_with_definite_value = c(paste0("last_",treatment),paste0("meaning_",treatment))
        
      )
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",treatment), paste0("meaning_",treatment)))
    }
    
    # add to the TD dataset all events from flares, EMG and HOSP, and other diagnoses
    
    thisemg <- flare_components[[immdis]][grep("^EMG",flare_components[[immdis]])]
    thishosp <- flare_components[[immdis]][grep("^HOSP",flare_components[[immdis]])]
    
    TD <- cohort
    
    for (bblock in c("TP_BLOODTRANSFUSIONUC_AESI", "TP_COLECTOMY_AESI", "TP_PROCTOCOLECTOMY_AESI", "TP_RESTRPROCTOCOLECUC_AESI",  "D_RECTALBLEED_AESI", "B_ANAEMIACOLITIS_AESI" , "O_FLARE_AESI", thisemg, thishosp) ){
      print(paste0(immdis," - ", bblock))
      temp <- get(bblock)
      if (nrow(temp > 0)){
        setnames(temp,"date","start_record_d")
        temp[, end_record_d := start_record_d]
        temp[, (paste0("last_",bblock)) := start_record_d]
        setnames(temp,"meaning_renamed",paste0("meaning_",bblock))
        TD <- GenerateTDDataset(
          datasets = list(TD,temp),
          UoO_vars = c("person_id","person_id"),
          start_d_vars = c("start_record_d","start_record_d"),
          end_d_vars = c("end_record_d","end_record_d"),
          keep_auxiliary_variables = F,
          TD_variables = list(TDvar_first_dataset,list(paste0("last_",bblock), paste0("meaning_",bblock))),
          keep_periods_observed_by = "first",
          TD_variables_with_definite_value = c(names(temp)[grep("^last_",names(temp))],names(temp)[grep("^meaning", names(temp))])
        )
      }else{
        TD[, (paste0("last_",bblock)) := as.Date(NA)]
        TD[, (paste0("meaning_",bblock)) := ""]
      }
      TDvar_first_dataset = append(TDvar_first_dataset, list(paste0("last_",bblock), paste0("meaning_",bblock)))
    }
    

  # generate components for flares of D_ULCERATIVECOLITIS_AESI

  TD[, date_flare_Flare := last_O_FLARE_AESI]
  TD[, date_flare_Diagnoses := as.Date(NA)]
  for (diag in c("D_RECTALBLEED_AESI", "B_ANAEMIACOLITIS_AESI")){
    TD[, date_flare_Diagnoses := pmax(date_flare_Diagnoses, get(paste0("last_",diag)), na.rm = T)]
  }
  TD[, date_flare_Medications := as.Date(NA)]
  for (treatment in list_of_medicines){
    TD[, date_flare_Medications := pmax(date_flare_Medications, get(paste0("last_",treatment)), na.rm = T)]
  }
  TD[, date_flare_Procedures := as.Date(NA)]
  for (proc in list_of_procedures){
    TD[, date_flare_Procedures := pmax(date_flare_Procedures, get(paste0("last_",proc)), na.rm = T)]
  }
  TD[, date_flare_Emergency := pmax(get(paste0("last_",thisemg)), na.rm = T)]
  TD[, date_flare_Hosp := pmax(get(paste0("last_",thishosp)),na.rm = T)]
  }

  print(paste("Calculate the flares of",immdis))
  
   
  TD[,date_flare := pmax(date_flare_Flare, date_flare_Diagnoses,date_flare_Medications,date_flare_Procedures,date_flare_Emergency,date_flare_Hosp,na.rm = T)]

  ########################################
  # save
  
  outputfile <- TD
  nameoutput <- paste0("D3_components_flare_TD_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  rm(emg,hosp,immdisdiag,TD,D3_cohort)
  for (buildingblock in list_of_building_blocks){
    if (length(codelists_of_bblock[[buildingblock]]) == 0){
      listbblocks <- buildingblock
    }else{
      listbblocks <- codelists_of_bblock[[buildingblock]]
    }
    for (bblock in listbblocks){ 
      rm(list = bblock)
    }
  }
      
    
}

