
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_30_Table_3_Covariates"
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
  # Table 3

  print(paste("Now creating: Table 3 in nice format for", immdis))

  tab_nice <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_3_Covariates_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread)  
      tab_nice <- rbind(tab_nice,temp, fill = T)
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }
  }

  oldnames <- names(tab_nice)
  newnames <- gsub(paste0("_Cov_",immdis), "", oldnames)
  setnames(tab_nice,oldnames, newnames)

  print(names(tab_nice))
  list_cov_in_tab <- gsub("n_","",intersect(c(paste0("n_",cov_of_immdis[[immdis]])), names(tab_nice)))
  list_cov_in_tab <- unique(gsub("n_", "", list_cov_in_tab))
  tokeep <- c(paste0("n_",cov_of_immdis[[immdis]]),paste0("p_",cov_of_immdis[[immdis]]),paste0("n_",cov_of_immdis[[immdis]],"_check"))
  
  tokeep <- c("ds","n_1",intersect(tokeep, names(tab_nice)))
  
  # tab_nice <- tab_nice[,..tokeep]
  #n_IM_IMC_COV_check
  
  row_header_1 = c(
  "Total cohort population", c(paste0(name_cov[list_cov_in_tab], " (n/%)")))
  
  for (j in c(1)) {
     tab_nice[,(paste0("cell_",j)) := as.character(format_with_comma(get(paste0("n_",j)))) ] 
  }
  
  for (j in seq_along(list_cov_in_tab)) {
    l = j + 1
    print(list_cov_in_tab[j])
    thiscov <- list_cov_in_tab[j]
    nthiscov <- paste0("n_",thiscov)
    pthiscov <- paste0("p_",thiscov)
    # tab_nice[!is.na(paste0("n_",list_cov_in_tab[j])),(paste0("cell_",l)) := paste0( format_with_comma(get(paste0("n_",list_cov_in_tab[j]))), " (", ifelse(is.character(get(paste0("p_",list_cov_in_tab[j]))),get(paste0("p_",list_cov_in_tab[j])),round(get(paste0("p_",list_cov_in_tab[j])), 1)),"%)")]
    # tab_nice[!is.na(paste0("n_",list_cov_in_tab[j])) & get(paste0("n_",list_cov_in_tab[j],"_check")) == 1,(paste0("cell_",l)) := paste0("< ",threshold)]
    #  tab_nice[!is.na(nthiscov),(paste0("cell_",l)) := paste0( format_with_comma(get(nthiscov)), " (", ifelse(is.character(get(pthiscov)),get(pthiscov),round(get(pthiscov), 1)),"%)")]
    tab_nice[!is.na(nthiscov),(paste0("cell_",l)) := paste0( format_with_comma(get(nthiscov)), " (", get(pthiscov),"%)")]
    # tab_nice[!is.na(nthiscov) & get(paste0(nthiscov,"_check")) == 1,(paste0("cell_",l)) := paste0("< ",threshold)]
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
  new_col_names <- unlist(c("Covariates at baseline", name_ds[setdiff( names(tab_nice),"row_header")]))
  setnames(tab_nice, old_col_names,  new_col_names)
  
  tab_nice <- tab_nice[, ..new_col_names]


  ########################################
  # save
  
  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_3_Covariates",immdis)
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