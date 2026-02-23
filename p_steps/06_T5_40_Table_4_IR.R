
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_40_Table_4_IR"
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
  # Table 4

  print(paste("Now creating: Table 4 in nice format for", immdis))

  tab_nice <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_4_IR_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread)  
      tab_nice <- rbind(tab_nice,temp, fill = T)
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }
  }
  
    # Helper function to format numbers with thousand separator
    format_with_comma <- function(x) {
    format(x, big.mark = ",", scientific = FALSE)
    }

  row_header_1 <- c()  
  
  ###############################
  # list of cells

  s = 1

  row_header_1 = c(row_header_1,
    "Total cohort population",
    "Number of persons who started the follow-up",
    # "Total follow-up time regardless of interruptions from start of follow-up (PY)",
    "Number of flares before entering follow-up"
  )

  for (j in c(1,2,3)) {
    tab_nice[,(paste0("cell_",s)) := format_with_comma(get(paste0("n_",j))) ]
    checkvar <- paste0("n_",j,"_check")
    tab_nice <- tab_nice[ds %in% datasources_with_rounding, (checkvar) := 0 ]
    
    tab_nice[get(checkvar) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
      s = s + 1
  }

  row_header_1 = c(row_header_1,
                   # "Follow-up time (PY) until censor or first flare, excluding interruptions",
                   "Number of persons who develop a first flare-up episode",
                   "IR of first flare-up episode, per 1,000 PY (CI 95%)",
                   # Number of persons who have at least one pregnancy after start of follow-up
                                    "Number of persons who have at least one pregnancy after start of follow-up" ,
                                    # "Total follow-up time regardless of interruptions from start of follow-up while persons are pregnant  (PY)",
                                    "Number of persons who develop a first flare-up episode while they are pregnant",
                                    "IR of first flare-up episode while pregnant, per 1,000 PY PY (CI 95%)"
                   )
  

  for (j in c("1")){
    tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("m_",j,"_0")))) ]
    checkvar <- paste0("m_",j,"_0","_check")
    tab_nice <- tab_nice[ds %in% datasources_with_rounding, (checkvar) := 0 ]
    tab_nice[get(checkvar) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    s = s + 1
    tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(paste0("ir_",j)))," (",as.character(get(paste0("ll_",j))),"-",  as.character(get(paste0("ul_",j))),")")]
    s = s + 1
    tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("npr_",j)))) ]
    checkvar <- paste0("m_",j,"_0","_check")
    tab_nice <- tab_nice[ds %in% datasources_with_rounding, (checkvar) := 0 ]
    
    tab_nice[get(paste0("npr_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
    tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("mpr_",j)))) ]
    tab_nice[get(paste0("mpr_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(paste0("irpr_",j)))," (",as.character(get(paste0("llpr_",j))),"-",  as.character(get(paste0("ulpr_",j))),")")]
  s = s + 1
  }

  
row_header_1 = c(row_header_1, 
                 "Number of persons still in the study 90 days after first flare", 
                 "Number of persons who develop a second flare-up episode", 
                 "IR of second flare-up episode, per 1,000 PY PY (CI 95%)", 
                 "Number of persons who have at least one pregnancy after the first flare", 
                 # "Total follow-up time regardless of interruptions after first flare while persons are pregnant (PY)",
                 "Number of persons who develop a second flare-up episode while they are pregnant",
                 "IR of first flare-up episode while pregnant, per 1,000 PY PY (CI 95%)"
)


for (j in c("2")){
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("n_1_",j)))) ]
  tab_nice[get(paste0("n_1_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("m_",j,"_0")))) ]
  tab_nice[get(paste0("m_",j,"_0")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(paste0("ir_",j)))," (",as.character(get(paste0("ll_",j))),"-",  as.character(get(paste0("ul_",j))),")")]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("npr_",j)))) ]
  tab_nice[get(paste0("npr_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("mpr_",j)))) ]
  tab_nice[get(paste0("mpr_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(paste0("irpr_",j)))," (",as.character(get(paste0("llpr_",j))),"-",  as.character(get(paste0("ulpr_",j))),")")]
  s = s + 1
}

row_header_1 = c(row_header_1, 
                 "Number of persons still in the study 90 days after second flare",
                 "Number of persons who develop a third flare-up episode",
                 "IR of third flare-up episode, per 1,000 PY PY (CI 95%)",
                 "Number of persons who have at least one pregnancy 90 days after second flare",
                 # "Total follow-up time regardless of interruptions after second flare while persons are pregnant (PY)",
                 "Number of persons who develop a third flare-up episode while they are pregnant",
                 "IR of third flare-up episode while pregnant, per 1,000 PY PY (CI 95%)"
)  

for (j in c("3")){
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("n_1_",j)))) ]
  tab_nice[get(paste0("n_1_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("m_",j,"_0")))) ]
  tab_nice[get(paste0("m_",j,"_0")) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(paste0("ir_",j)))," (",as.character(get(paste0("ll_",j))),"-",  as.character(get(paste0("ul_",j))),")")]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("npr_",j)))) ]
  tab_nice[get(paste0("npr_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("mpr_",j)))) ]
  tab_nice[get(paste0("mpr_",j)) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := paste0(as.character(get(paste0("irpr_",j)))," (",as.character(get(paste0("llpr_",j))),"-",  as.character(get(paste0("ulpr_",j))),")")]
  s = s + 1
}

  tokeep <- c("ds",names(tab_nice)[grep("^cell_",names(tab_nice))])
   
  tab_nice <- tab_nice[, ..tokeep]
  

  # Reshape
  tab_nice <- melt(tab_nice, id.vars = "ds", measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")



  # Cast to one column per 'ds'
  tab_nice <- dcast(tab_nice, cell ~ ds, value.var = "value")
  
  # transform cell into a number
  tab_nice[, cell := gsub("cell_", "", cell)]
  tab_nice[, cell := as.numeric(gsub("_", ".", cell))]


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
  new_col_names <- unlist(c("Cohort characteristics", name_ds[setdiff( names(tab_nice),"row_header")]))
  setnames(tab_nice, old_col_names,  new_col_names)

  tab_nice <- tab_nice[, ..new_col_names]

    
  ########################################
  # save
  
  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_4_IR_",immdis)
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