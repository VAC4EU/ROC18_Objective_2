
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_70_Table_7_Cumulative_incidence"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
  thisdatasources_for_postprocessing <- c("TEST","TEST2")
  thisfolder_submission <- folders_submission
  thisfolder_submission[["TEST"]] <- paste0(thisdirinput,"folder_TEST/")
  thisfolder_submission[["TEST2"]] <- paste0(thisdirinput,"folder_TEST2/")
  
}else{
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  if (localprocessing){
    # this happens at DEAPs
    thisdirinput <- dirtablesubpop[[subpop]]
    thisdiroutput <- dirD6
    thisdatasources_for_postprocessing <- thisdatasource
    thisfolder_submission <- folders_submission
    thisfolder_submission[[thisdatasource]] <- thisdirinput
  }else{
      # this happens at DRE
      thisdatasources_for_postprocessing <- datasources_for_postprocessing
      thisfolder_submission <- folders_submission
      thisdiroutput <- paste0(direxp,"Formatted tables/")
  
    }
}


for (immdis in immune_diseases_in_this_step){ 
  # Table 7

  print(paste("Now creating: Table 7 in nice format for", immdis))

  tab_nice <- data.table()
  ds_with_data <- c() 
  for (ds in thisdatasources_for_postprocessing) {

    filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_7_cumulative_incidence_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread)  
      if (nrow(temp) > 0) {
        var <- "ds"
        temp[, (var) := ds]
        temp[, ord := seq_len(.N)]
        tab_nice <- rbind(tab_nice,temp, fill = T)
        ds_with_data <- c(ds_with_data,ds)
      }
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }
  }
  
  if (nrow(tab_nice) > 0) {
    tab_nice[, value := paste0(as.character(ci), " (",as.character(ll),"-",as.character(ul),")")]
    
    
    # Cast to one column per 'ds'
    tab_nice <- dcast(tab_nice, ord + period + age_band + gender ~ ds, value.var = "value")
  
    tab_nice[age_band == 99,Ageband := "All"]
    tab_nice[age_band == 0,Ageband := "0-17"]
    tab_nice[age_band == 18,Ageband := "18-59"]
    tab_nice[age_band == 60,Ageband := "60+"]
  
    tab_nice[gender == "all", gender := "All"]
  
    tab_nice[period == 1,Days := "180"]
    tab_nice[period == 2,Days := "365"]
    
    
    setnames(tab_nice, "gender", "Gender")
  
    # order
    setorder(tab_nice, Days,ord)
  
    
    tokeep <- c("Days","Ageband","Gender" , ds_with_data)
    tab_nice <- tab_nice[,..tokeep]
  
    # rename
    old_col_names = c(ds_with_data)
    new_col_names <- unlist( name_ds[ds_with_data])
    setnames(tab_nice, old_col_names,  new_col_names)
  
  
  
    ########################################
    # save
  
    outputfile <- tab_nice
    nameoutput <- paste0("D6_Table_7_Cumulative_incidence",immdis)
    assign(nameoutput, outputfile)
    # rds
    saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
    # csv
    fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
    # xls
    write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
    # html
    html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
    writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
    # rtf
    doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
  print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))
  }
 }