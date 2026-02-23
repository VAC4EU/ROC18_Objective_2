
if (TEST){ 
  # this happens during development
  testname <- "test_06_T5_10_Table_1_Attrition"
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
      # only temporary: retrieve the temporary tables generated in the auxiliary step 06_T4_09
      thisdirinputbase <- paste0(dirtemp,"D5_Table_1/")
      thisdirinput <- list()
      dir.create(thisdirinputbase,showWarnings = F)
      # for (ds in thisdatasources_for_postprocessing) { 
      #   thisdirinput[[ds]] <- paste0(thisdirinputbase,"/",ds,"/")
      #   dir.create(thisdirinput[[ds]],showWarnings = F)
      # }
      # thisfolder_submission <- thisdirinput
      thisfolder_submission <- folders_submission
      thisdiroutput <- paste0(direxp,"Formatted tables/")
    }
}

# Helper function to format numbers with thousand separator
format_with_comma <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

format_with_comma_range <- function(original_range) {
  range_values <- regmatches(original_range, gregexpr("\\d+", original_range))[[1]]
  final_str <- paste("(", format_with_comma(range_values[1]), "-", format_with_comma(range_values[2]), "]")
  return(final_str)
}


for (immdis in immune_diseases_in_this_step){ 
  # Table 1

  print(paste("Now creating: Table 1 in nice format for", immdis))

  tab_nice <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_1_Attrition_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread)  
      tab_nice <- rbind(tab_nice,temp, fill = T)
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }
  }
  

  # replace n_4 with its complementary (this is an approximation in case one of the exclusion criteria is a small number)
  
  row_header_1 <- c()  
  row_header_partial <- list()
  row_header_partial[["1"]] <- "Total persons in the data instance"
  row_header_partial[["2a"]] <- "Sex or birthdate not defined"
  row_header_partial[["2b"]] <- "Birthdate absurd"
  row_header_partial[["2c"]] <- "Deathdate incomplete"
  row_header_partial[["2d"]] <- "No observation periods, or observation periods lacking some data banks"
  row_header_partial[["2e"]] <- "Observation periods invalid (start after ending)"
  row_header_partial[["2f"]] <- "Observation periods shorter than lookback (365 days)"
  row_header_partial[["3"]] <- "Exit from data source before 1 Jan 2017"
  row_header_partial[["4"]] <- paste("Inclusion: persons with a code of",name_immdis[[immdis]],  "identified after study entry date")
  row_header_partial[["5"]] <- "Disease codes found during lookback"
  row_header_partial[["6"]] <- "Other criteria suggesting the disease is present during lookback"
  row_header_partial[["7"]] <- paste("Inclusion: cohort of ",  name_immdis[[immdis]])
  row_header_partial[["8"]] <- "Persons dying before entering the follow-up"
  row_header_partial[["9"]] <- "Persons leaving alive the cohort before entering the follow-up (censoring)"
  row_header_partial[["10"]] <- "Persons entering follow-up"

  ###############################
  # list of cells
  
  s = 1
  
  j <- "1"
  row_header_1 = c(row_header_1,
                   row_header_partial[[j]])
  
  basevalue <- tab_nice[, get(paste0("n_", j))]
  checkname <- paste0("n_", j, "_check")
  checkvalue <- if (checkname %in% colnames(tab_nice)) tab_nice[, get(checkname)] else 0
  
  tab_nice[, (paste0("cell_",s)) := data.table::fcase(checkvalue == 0, format_with_comma(basevalue), # Original number
                                                      checkvalue == 1, basevalue, # <5
                                                      checkvalue == 2, basevalue, # NR
                                                      checkvalue == 3, format_with_comma_range(basevalue))] # Range
  s = s + 1
  
  
  for (j in c("2a","2b","2c","2d","2e","2f")) {
    row_header_1 = c(row_header_1,row_header_partial[[j]])
    
    basevalue <- tab_nice[, get(paste0("n_", j))]
    checkvalue <- tab_nice[, get(paste0("n_", j, "_check"))]
    percentagevalue <- tab_nice[, get(paste0("p_", j))]
    percentagevalue <- if (is.numeric(percentagevalue)) round(percentagevalue, 1)
    
    tab_nice[, (paste0("cell_",s)) := paste0(format_with_comma(basevalue))]
    # tab_nice[, (paste0("cell_",s)) := paste0(format_with_comma(basevalue), " (", percentagevalue,"%)")]
    
    s = s + 1
  }
  
  for (j in c("3","4","5","6","7","8","9","10")) {
    row_header_1 = c(row_header_1,row_header_partial[[j]])
    
    basevalue <- tab_nice[, get(paste0("n_", j))]
    checkvalue <- tab_nice[, get(paste0("n_", j, "_check"))]
    percentagevalue <- tab_nice[, get(paste0("p_", j))]
    percentagevalue <- if (is.numeric(percentagevalue)) round(percentagevalue, 1)
    
    tab_nice[, (paste0("cell_",s)) := paste0(format_with_comma(basevalue))]
    
    # tab_nice[get(checkvar) == 1,(paste0("cell_",s)) := paste0("< ",threshold)]
    s = s + 1
  }
  

  
  
  
  tokeep <- c("ds",names(tab_nice)[grep("^cell_",names(tab_nice))])
  
  tab_nice <- tab_nice[, ..tokeep]


  # Reshape
  tab_nice <- melt(tab_nice, id.vars = "ds", measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")

  # transform cell into an integer
  tab_nice[, cell := as.integer(gsub("cell_", "", cell))]
  # tab_nice[, cell := gsub("cell_", "", cell)]

  # add row_header_1
  tab_nice[, Exclusion_criteria := row_header_1[cell]]


  # Cast to one column per 'ds'
  tab_nice <- dcast(tab_nice, cell + Exclusion_criteria ~ ds, value.var = "value")

  # order
  setorder(tab_nice, cell)

  # remove cell
  tab_nice[, cell := NULL]
  # rename
  new_col_names <- unlist(c("Exclusion criteria", name_ds[colnames(tab_nice)[-1]]))
  setnames(tab_nice, old = colnames(tab_nice), new = new_col_names)

  ########################################
  # save

  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_1_Attrition_",immdis)
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

# # Save as RTF
# doc <- read_docx() %>% 
#   body_add_table(table, style = "table_template") %>% 
#   body_end_section_continuous()
# 
# # Save the RTF file
# print(doc, target = "output_table_with_thousand_separator.rtf")
