##%######################################################%##
#                                                          #
####          CREATE OUTCOMES FROM CONCEPTSET           ####
####              FOR COMPLEX ALGORITHMS                ####
#                                                          #
##%######################################################%##



print('create secondary components SECCOMPONENTS.')

# load(paste0(dirpargen,"subpopulations_non_empty.RData"))

##for each SECCOMP create components

# create emptyconceptsetsataset


emptyconceptsetsataset <- data.table(person_id = character(0),
                                     date = as.Date(as.POSIXct(character(0))),
                                     end_date_record = as.Date(as.POSIXct(character(0))),
                                     codvar = character(0),
                                     event_record_vocabulary = character(0),
                                     text_linked_to_event_code = logical(0),
                                     event_free_text = character(0),
                                     present_on_admission = character(0),
                                     laterality_of_event = logical(0),
                                     meaning_renamed = character(0),
                                     origin_of_event = character(0),
                                     visit_occurrence_id = character(0),
                                     Col = character(0),
                                     Table_cdm = character(0))

for (SECCOMP in SECCOMPONENTS) {
  componentsSECCOMP <- vector(mode="list")
  print(SECCOMP)
  
  if (thisdatasource == "BIFAP") {
    n_batches <- 30
  } else {
    n_batches <- 1
  }
  
  COHORT_TMP <- as.data.table(readRDS(paste0(diroutput, "D4_source_population.rds")))  
  
  COHORT_TMP <- COHORT_TMP[,.(person_id,study_entry_date)]
  COHORT_TMP <- COHORT_TMP[, placeholder := rep_len(1:n_batches, nrow(COHORT_TMP))]
  COHORT_TMP <- split(COHORT_TMP, by = "placeholder", keep.by = F)
  
  componentsSECCOMP <- list()
  
  for (i in 1:n_batches) {
    
    #create datasets A and B to be merged for the secondary component
    datasets_to_be_merged <- vector(mode="list")
    
    for (ord in c('A','B')){
      temp <- emptyconceptsetsataset
      for (conceptset in  concept_set_seccomp[[SECCOMP]][[ord]]){
        print(paste(SECCOMP,'- conceptset',ord,conceptset, '- batch', i, 'out of', n_batches))
        
        toadd <- get(load(paste0(dirconceptsets, conceptset, ".RData"))[[1]])
        
        # delete records whose meanings are not observed in this whole subpopulation
        if (this_datasource_has_subpopulations == TRUE){ 
          toadd <- toadd[eval(parse(text = select_in_subpopulationsEVENTS[[subpop]])),]
        }
        # delete records whose meanings are not appropriate for a specific datasource are not observed in this whole subpopulation
        toadd <- toadd[ eval(parse(text = paste0('!(',select_meanings_AESI[[thisdatasource]],')'))),]
        
        if (ord == "A") {
          toadd <- toadd[date >= start_lookback]
        }
        
        toadd <- COHORT_TMP[[i]][toadd, on = "person_id", nomatch = NULL]
        
        # toadd <- toadd[,conceptsetname := conceptset]
        temp <- as.data.table(rbind(temp,toadd,fill = TRUE))
        rm(toadd)
      }
      temp <- temp[,.(person_id,date)]
      for (col in c('date')){
        setnames(temp, col,paste0(col,ord) )
      }
      datasets_to_be_merged[[ord]] <- temp
      rm(temp)
    }
    
    # select according to the rule of the component
    selection <- paste0("!is.na(dateA) & !is.na(dateB) & ", selectionrule_direction_seccomp[[SECCOMP]][[direction_seccomp[[SECCOMP]]]])
    
    # merge datasets A and B
    unique_A_AND_B_timeframe <- MergeFilterAndCollapse(
      listdatasetL = list(datasets_to_be_merged[['A']]),
      datasetS = datasets_to_be_merged[['B']],
      condition = selection,
      key = c("person_id"),
      typemerge = 2,
      sorting= c("person_id","dateA"),
      saveintermediatedataset = T,
      nameintermediatedataset = paste0(dirconceptsets,'tempfile'),
      strata = c("person_id"),
      summarystat = list(
        list(c('first'),'dateA','date_event')
      )
    )
    
    load(paste0(dirconceptsets,'tempfile.RData') )
    all_A_AND_B_timeframe <- tempfile
    
    if (rule_seccomp[[SECCOMP]] == "AND NOT"){
      if (nrow(datasets_to_be_merged[['A']]) > 0){
        all_component_A <- datasets_to_be_merged[['A']]
        all_component_A <- all_component_A[all_component_A[, .I[sample(.N,1)] , by = c("person_id","dateA")]$V1]
        if (nrow(all_A_AND_B_timeframe)>0){ 
          to_exclude <- all_A_AND_B_timeframe[,.(person_id,dateA,dateB)]
          listevents <-  merge(to_exclude,all_component_A, by = c("person_id","dateA"), all.y = T)[(eval(parse(text = 'is.na(dateB)'))),]
          rm(to_exclude)
        }
        else{
          listevents <-  all_component_A
        }
        rm(all_component_A)
      }
      else{
        listevents <- datasets_to_be_merged[['A']]
      }
    }
    if (rule_seccomp[[SECCOMP]] == "AND"){
      listevents <- all_A_AND_B_timeframe
    }
    
    listevents <- setnames(listevents, 'dateA','date') 
    
    components <- MergeFilterAndCollapse(
      listdatasetL = list(listevents),
      condition = "date >= study_entry_date - 365 ",
      key = c("person_id"),
      datasetS = COHORT_TMP[[i]],
      saveintermediatedataset = T,
      nameintermediatedataset = paste0(dirconceptsets,'tempfile'),
      strata = c("person_id"),
      summarystat =  list(
        list(c('min'),'date','date_event')
      )
    )
    
    load(paste0(dirconceptsets,'tempfile.RData') )
    
    if (SECCOMP == "E_DM1ALGOR_AESI") {
      load(paste0(dirtemp,"D3_PERSONS.RData")) 
      birthdate <- D3_PERSONS
      birthdate <- birthdate[,.(person_id,birth_date)]
      tempfile <- merge(tempfile, birthdate, all.x = TRUE, by = "person_id")
      tempfile <- tempfile[, age_at_date := age_fast(birth_date,date)]
      tempfile <- tempfile[age_at_date <25,]
      tempfile <- tempfile[,age_at_date := NULL]
      tempfile <- tempfile[,birth_date := NULL]
      rm(D3_PERSONS)
      rm(birthdate)
    }
    componentsSECCOMP <- append(componentsSECCOMP, list(tempfile))
    
  }
  
  componentsSECCOMP <- rbindlist(componentsSECCOMP)
  
  
  nameobjectSECCOMP <- paste0('D3_events_', SECCOMP, '_complex', suffix[[subpop]])
  saveRDS(componentsSECCOMP, file=paste0(direvents, paste0(nameobjectSECCOMP,".rds")))
  rm(datasets_to_be_merged,componentsSECCOMP,tempfile,COHORT_TMP,components,listevents,all_A_AND_B_timeframe,unique_A_AND_B_timeframe)
}


