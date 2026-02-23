
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_50_Table_5_IR_age_gender"
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
  # Table 5

  print(paste("Now creating: Table 5 in nice format for", immdis))

  tab_nice <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    for (stratum in  c("M-allage","F-allage","allgender-0","allgender-18","allgender-60")) {
      filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_5_IR_", immdis,"_",stratum, ".csv")
    
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread) 
      temp[,tp := stratum]
      setnames(temp, "tp","stratum")
      tab_nice <- rbind(tab_nice,temp, fill = T)
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }
    }
  }
  
  tab_nice[,stratumage := fifelse(stratum == "allgender-0" | stratum == "allgender-18" | stratum == "allgender-60", 1, 0)]
  tab_nice[,stratumgender := fifelse(stratum == "M-allage" | stratum == "F-allage", 1, 0)]
  tab_nice[,stratumtype := fifelse(stratumgender == 1, "gender", "age")]

  
  
  row_header_1 <- c()  

  ###############################
  # list of cells

  s = 1
  row_header_1 = c(row_header_1,
                   # "Follow-up time (PY) until censor or first flare, excluding interruptions",
                   "Number of persons who develop a first flare-up episode",
                   "IR of first flare-up episode, per 1,000 PY (CI 95%)"
                   )


  for (j in c("1")){
    checkvar <- paste0("m_",j,"_0","_check")
    tab_nice[,checkvarsum := sum(get(checkvar), na.rm = T), by = .(ds,stratumtype)]
    tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("m_",j,"_0")))) ]
    tab_nice[get(paste0("m_",j,"_0","_check")) == 1,(paste0("cell_",s)) := as.character(paste0("< ",threshold))]
    tab_nice <- tab_nice[checkvarsum == 1 & ds %in% ds_rm_higher_level_if_one_smallcount,(paste0("cell_",s)) := "NC"]
    s = s + 1
    tab_nice[,(paste0("cell_",s)) := as.character(paste0(get(paste0("ir_",j))," (",as.character(get(paste0("ll_",j))),"-",  as.character(get(paste0("ul_",j))),")"))]
    s = s + 1
   }
# 
#   
row_header_1 = c(row_header_1, 
                 "Number of persons still in the study 90 days after first flare",
                 "Number of persons who develop a second flare-up episode",
                 "IR of second flare-up episode, per 1,000 PY PY (CI 95%)" 
                 )
# 
 
for (j in c("2")){
  checkvar <- paste0("n_1_",j,"_check")
  tab_nice[,checkvarsum := sum(get(checkvar), na.rm = T), by = .(ds,stratumtype)]
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("n_1_",j)))) ]
  tab_nice[get(paste0("n_1_",j)) == 1,(paste0("cell_",s)) := as.character(paste0("< ",threshold))]
  tab_nice <- tab_nice[checkvarsum == 1 & ds %in% ds_rm_higher_level_if_one_smallcount,(paste0("cell_",s)) := "NC"]
  s = s + 1
  checkvar <- paste0("m_",j,"_0","_check")
  tab_nice[,checkvarsum := sum(get(checkvar), na.rm = T), by = .(ds,stratumtype)]
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("m_",j,"_0")))) ]
  tab_nice[get(paste0("m_",j,"_0")) == 1,(paste0("cell_",s)) := as.character(paste0("< ",threshold))]
  tab_nice <- tab_nice[checkvarsum == 1 & ds %in% ds_rm_higher_level_if_one_smallcount,(paste0("cell_",s)) := "NC"]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(paste0(get(paste0("ir_",j))," (",as.character(get(paste0("ll_",j))),"-",  as.character(get(paste0("ul_",j))),")"))]
  s = s + 1
}
 
row_header_1 = c(row_header_1,
                 "Number of persons still in the study 90 days after second flare",
                 "Number of persons who develop a third flare-up episode",
                 "IR of third flare-up episode, per 1,000 PY PY (CI 95%)" #,
)  

for (j in c("3")){
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("n_1_",j)))) ]
  checkvar <- paste0("n_1_",j,"_check")
  tab_nice[,checkvarsum := sum(get(checkvar), na.rm = T), by = .(ds,stratumtype)]
  tab_nice[get(paste0("n_1_",j)) == 1,(paste0("cell_",s)) := as.character(paste0("< ",threshold))]
  tab_nice <- tab_nice[checkvarsum == 1 & ds %in% ds_rm_higher_level_if_one_smallcount,(paste0("cell_",s)) := "NC"]
  s = s + 1
  checkvar <- paste0("m_",j,"_0","_check")
  tab_nice[,checkvarsum := sum(get(checkvar), na.rm = T), by = .(ds,stratumtype)]
  tab_nice[,(paste0("cell_",s)) := as.character(format_with_comma(get(paste0("m_",j,"_0")))) ]
  tab_nice[get(paste0("m_",j,"_0")) == 1,(paste0("cell_",s)) := as.character(paste0("< ",threshold))]
  tab_nice <- tab_nice[checkvarsum == 1 & ds %in% ds_rm_higher_level_if_one_smallcount,(paste0("cell_",s)) := "NC"]
  s = s + 1
  tab_nice[,(paste0("cell_",s)) := as.character(paste0(get(paste0("ir_",j))," (",as.character(get(paste0("ll_",j))),"-",  as.character(get(paste0("ul_",j))),")"))]
  s = s + 1
  }
 
   tokeep <- c("ds","stratum",names(tab_nice)[grep("^cell_",names(tab_nice))])

  tab_nice <- tab_nice[, ..tokeep]


  # Reshape
  tab_nice <- melt(tab_nice, id.vars = c("ds", "stratum"), measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")



  # Cast to one column per 'ds'
  tab_nice <- dcast(tab_nice, cell + stratum ~ ds, value.var = "value")

  # transform cell into a number
  tab_nice[, cell := gsub("cell_", "", cell)]
  tab_nice[, cell := as.numeric(gsub("_", ".", cell))]


  # order
  setorder(tab_nice, cell)
  tab_nice[, ord := seq_len(.N)]


  # add row_header_1
  tab_nice[, row_header := row_header_1[cell]]

  # add gender and ageband
  
  # "F-allage"     "M-allage"     "allgender-0"  "allgender-18" "allgender-60"
  tab_nice <- tab_nice[stratum == "F-allage",Gender := "F"]
  tab_nice <- tab_nice[stratum == "M-allage",Gender := "M"]
  tab_nice <- tab_nice[is.na(Gender),Gender := ""]
  tab_nice <- tab_nice[stratum == "allgender-0",Ageband := "0-17"]
  tab_nice <- tab_nice[stratum == "allgender-18",Ageband := "18-59"]
  tab_nice <- tab_nice[stratum == "allgender-60",Ageband := "60+"]
  tab_nice <- tab_nice[is.na(Ageband),Ageband := ""]
  

  # remove cell
  tab_nice[, cell := NULL]
  tab_nice[, ord := NULL]
  tab_nice[, stratum := NULL]
  
  # rename
  old_col_names = c("row_header","Gender","Ageband",setdiff( names(tab_nice),c("row_header","Gender","Ageband")))
  new_col_names <- c("Characteristics","Gender","Ageband", unlist(name_ds[setdiff( names(tab_nice),c("row_header","Gender","Ageband"))]))
  setnames(tab_nice, old_col_names,  new_col_names)
 
  tab_nice <- tab_nice[, ..new_col_names]

  tab_nice_ageband <- tab_nice[Ageband != "",]
  tab_nice_ageband <- tab_nice_ageband[,Gender := NULL]
  
  tab_nice_gender <- tab_nice[Gender != "",]
  tab_nice_gender <- tab_nice_gender[Gender == "F" | Gender == "M",]
  tab_nice_gender <- tab_nice_gender[, Ageband := NULL]
  
  ########################################
  # save 
  
  for (stratum in c("ageband","gender")) {
    outputfile <- get(paste0("tab_nice_",stratum))
    nameoutput <- paste0("D6_Table_5_IR_",stratum,"_",immdis)
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
