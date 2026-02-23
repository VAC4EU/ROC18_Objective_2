
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_80_Table_8_Impact_of_ablation"
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

true_groups_of_prompts <- groups_of_prompts[2:length(groups_of_prompts)]


for (immdis in immune_diseases_in_this_step){ 
  # Table 8

  print(paste("Now creating: Table 8 in nice format for", immdis))

  tab_nice <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_8_Impact_of_ablation_cohort_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread)  
      tab_nice <- rbind(tab_nice,temp, fill = T)
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }
  }

  # oldnames <- names(tab_nice)
  # newnames <- gsub(paste0("_Cov_",immdis), "", oldnames)
  # setnames(tab_nice,oldnames, newnames)
    
  # Helper function to format numbers with thousand separator
  format_with_comma <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
  }
  
  
  # Subjects with at least one disease diagnosis during study period

  j = 1

  row_header_1 = c(
    "Subjects with at least one disease diagnosis during study period, Main, n")

  s = 1
  tab_nice[,(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j))) ]
  tab_nice[get(paste0("n_",j,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s  + 1
  
  for (group in true_groups_of_prompts) {
    row_header_1 = c(row_header_1, paste0("Subjects with at least one disease diagnosis during study period, Ablation analysis keeping ", name_group[[group]],", n (%)"))
    tab_nice[!is.na(get(paste0("n_",j,"_",group))),(paste0("cell_",s)) := paste0(format_with_comma(get(paste0("n_",j,"_",group))), " (",get(paste0("p_",j,"_",group)),"%)") ]
    tab_nice[get(paste0("n_",j,"_",group,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    
    s = s  + 1
  }
  
  
  # "At least one disease diagnosis during look-back
  
  j = 2
  
  row_header_1 = c(row_header_1, "At least one disease diagnosis during look-back, Main, n")
  
  tab_nice[,(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j))) ]
  tab_nice[get(paste0("n_",j,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s  + 1
  
  for (group in true_groups_of_prompts) {
    row_header_1 = c(row_header_1, paste0("At least one disease diagnosis during look-back, Ablation analysis keeping ", name_group[[group]],", n"))
    tab_nice[!is.na(get(paste0("n_",j,"_",group))),(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j,"_",group))) ]
    tab_nice[get(paste0("n_",j,"_",group,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    s = s  + 1
  }
  
  # Additional exclusion criterion
  
  j = 3
  row_header_1 = c(row_header_1,paste0("Additional exclusion criterion, Main, n"))
  
  tab_nice[,(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j))) ]
  tab_nice[get(paste0("n_",j,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s  + 1
  
  for (group in true_groups_of_prompts) {
    row_header_1 = c(row_header_1,paste0("Additional exclusion criterion, Ablation analysis keeping ", name_group[[group]],", n"))
    tab_nice[!is.na(get(paste0("n_",j,"_",group))),(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j,"_",group))) ]
    tab_nice[get(paste0("n_",j,"_",group,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    s = s  + 1
  }
  
  
  # Subjects included in the study cohort
  
  j = 4
  row_header_1 = c(row_header_1,"Subjects included in the study cohort, Main, n")
  
  tab_nice[,(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j))) ]
  tab_nice[get(paste0("n_",j,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s  + 1
  
  for (group in true_groups_of_prompts) {
    row_header_1 = c(row_header_1,paste0("Subjects included in the study cohort, Ablation analysis keeping ", name_group[[group]],", n"))
    
    tab_nice[!is.na(get(paste0("n_",j,"_",group))),(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j,"_",group))) ]
    tab_nice[get(paste0("n_",j,"_",group,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    s = s  + 1
    
    row_header_1 = c(row_header_1,paste0("Subjects included in the study cohort, only in main and not in ", name_group[[group]],", n (%)"))
    tab_nice[!is.na(get(paste0("n_",j,"_main_not_",group))),(paste0("cell_",s)) := paste0(format_with_comma(get(paste0("n_",j,"_main_not_",group))), " (",get(paste0("p_",j,"_main_not_",group)),"%)") ]
    tab_nice[get(paste0("n_",j,"_main_not_",group,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    s = s + 1
    
    row_header_1 = c(row_header_1,paste0("Subjects in both main and ", name_group[[group]],", n (%)"))
    tab_nice[!is.na(get(paste0("n_",j,"_both_",group))),(paste0("cell_",s)) := paste0(format_with_comma(get(paste0("n_",j,"_both_",group))), " (",get(paste0("p_",j,"_both_",group)),"%)") ]
    tab_nice[get(paste0("n_",j,"_both_",group,"_check")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    s = s + 1
  }
  
  # Days from start of study period (1/1/2017) and first disease diagnosis (cohort entry)
  
  row_header_1 = c(row_header_1,"Days from start of study period (1/1/2017) and first disease diagnosis (cohort entry) in the main analysis, median (IQ range)")
  
  tab_nice[ , (paste0("cell_",s)) := paste0(median_time_to_entry_, " (", p25_time_to_entry_,"-",p75_time_to_entry_,")") ]
  s = s  + 1
  
  for (group in true_groups_of_prompts) {
    row_header_1 = c(row_header_1, paste0("Days from start of study period (1/1/2017) and first disease diagnosis (cohort entry), in the ablation analysis keeping ", name_group[[group]]," median (IQ range)"))
    tab_nice[ , (paste0("cell_",s)) := paste0(get(paste0("median_time_to_entry_",group)), " (", get(paste0("p25_time_to_entry_",group)),"-",get(paste0("p75_time_to_entry_",group)),")") ]
    s = s  + 1
  }
  
  # Difference between cohort entry date in the main cohort and in the ablation analysis
  
  for (group in true_groups_of_prompts) {
    row_header_1 = c(row_header_1, paste0("Difference between cohort entry date in the main cohort and in the ablation analysis keeping ", name_group[[group]],", median (IQ range)"))
    
    tab_nice[ , (paste0("cell_",s)) := paste0(get(paste0("median_delay_",group)), " (", get(paste0("p25_delay_",group)),"-",get(paste0("p75_delay_",group)),")") ]
    s = s  + 1
  }
  

  ###############################
  # clean up
  
  tokeep <- c("ds",names(tab_nice)[grep("^cell_",names(tab_nice))])
   
   tab_nice <- tab_nice[, ..tokeep]


  # Reshape
  tab_nice <- melt(tab_nice, id.vars = "ds", measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")


  # Cast to one column per 'ds'
  tab_nice <- dcast(tab_nice, cell ~ ds, value.var = "value")

  # transform cell into a number
  tab_nice[, cell := gsub("cell_", "", cell)]
  tab_nice[, cell := as.numeric(gsub("_", ".", cell))]

  
  # if (immdis %in% immdis_UOSL_no_ablation) {
  #   tab_nice[, UOSL := "NC"]
  # }

  # order
  setorder(tab_nice, cell)
  tab_nice[, ord := seq_len(.N)]


  # add row_header_1
  tab_nice[, row_header := row_header_1[ord]]



  # remove cell
  tab_nice[, cell := NULL]
  tab_nice[, ord := NULL]

  # rename
  old_col_names = c("row_header",setdiff( names(tab_nice),"row_header"))
  new_col_names <- unlist(c("Steps in ablation analysis", name_ds[setdiff( names(tab_nice),"row_header")]))
  setnames(tab_nice, old_col_names,  new_col_names)

  tab_nice <- tab_nice[, ..new_col_names]


  ########################################
  # save

  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_8_Impact_of_ablation_cohort_",immdis)
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
