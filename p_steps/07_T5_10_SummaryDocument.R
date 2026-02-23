
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_07_T5_10_SummaryDocument"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  # thisdirinput <- paste0(thisdir,"/g_export/Formatted tables/")
  thisdiroutput <- file.path(dirtest,testname,"/g_output_program/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  if (localprocessing){
    # this happens at DEAPs
    print("the step 07_T5_10_SummaryDocument should not be run in the DEAP's environment")
    break
  }else{
      # this happens at DRE
    thisdirinput <- dirD6
      thisdiroutput <- paste0(direxp, "Word documents/")
      dir.create(thisdiroutput, showWarnings = F)
      thisdirfigure <- "figures/"
    }
}

if (TEST | !localprocessing){
  for (j in 1:length(immune_diseases_in_this_step)) {
    immdis <- immune_diseases_in_the_study[[j]]
    
    immdis_name <- name_immdis[[immdis]]
    
    table_01_name <- paste0(" 10.",j,".1.1")
    table_01_caption <- paste0("Attrition table for the cohort with ", name_immdis[[immdis]])
    table_01_content <- fread(file = paste0(thisdirinput,"D6_Table_1_Attrition_",immdis,".csv"))
    
    table_02_name <- paste0(" 10.",j,".1.2")
    table_02_caption <- paste0("Characteristics of the study cohort with ", name_immdis[[immdis]])
    table_02_content <- fread(file = paste0(thisdirinput,"D6_Table_2_Cohort_characteristics",immdis,".csv"))
    
    table_03_name <- paste0(" 10.",j,".1.3")
    table_03_caption <- paste0("Comorbidities in the study cohort with ", name_immdis[[immdis]])
    table_03_content <- fread(file = paste0(thisdirinput,"D6_Table_3_Covariates",immdis,".csv"))
    
    table_04_name <- paste0(" 10.",j,".1.4")
    table_04_caption <- paste0("Background incidence rate of first, second, and third flare in the cohort of ", name_immdis[[immdis]],", per 1,000 person-years, with 95% confidence intervals.")
    table_04_content <- fread(file = paste0(thisdirinput,"D6_Table_4_IR_",immdis,".csv"))
    table_04_content[, N := seq(.N)]
    tokeeplist <- c(1,2,3,4,5,9,10,11,15,16,17)
    table_04_content <- table_04_content[N %in% tokeeplist,]
    table_04_content <- table_04_content[, N := NULL]
    
    table_05_name <- paste0(" 10.",j,".1.5")
    table_05_caption <- paste0("Six- and twelve-months cumulative incidence of flares of ", name_immdis[[immdis]],", with 95% confidence intervals")
    table_05_content <- fread(file = paste0(thisdirinput,"D6_Table_7_Cumulative_incidence",immdis,".csv"))
    table_05_content[, N := seq(.N)]
    tokeeplist <- c(1,13)
    table_05_content <- table_05_content[N %in% tokeeplist,]
    table_05_content <- table_05_content[, N := NULL]
    table_05_content <- table_05_content[, Ageband := NULL]
    table_05_content <- table_05_content[, Gender := NULL]
    
    fig_01_name <- paste0(" 10.",j,".2.1")
    fig_01_caption <- paste0("Curves of cumulative incidence of flares of ", name_immdis[[immdis]])
    pdf_file <- paste0(thisdirinput,"figures/Figure_KM_",  immdis,".pdf")
    fig_01_filename <- paste0(thisdirinput,"figures/Figure_KM_",immdis, ".png")
    pdf_convert(pdf_file, format = "png", dpi = 300, filenames = fig_01_filename)
    
    table_06_name <- paste0(" 10.",j,".2.1")
    table_06_caption <- paste0("Impact of the ablation of prompts on the attrition in the cohort with ", name_immdis[[immdis]], "and on the time and delay to entrance in the cohort (for those in both the main and in the ablation chort)")
    table_06_content <- fread(file = paste0(thisdirinput,"D6_Table_8_Impact_of_ablation_cohort_",immdis,".csv"))
    
    
    fig_02_name <- paste0(" 10.",j,".2.2")
    fig_02_caption <- paste0("Impact of the ablation of groups of prompts in the recruitment of the cohort of ", name_immdis[[immdis]],": bar plot of percentage of persons with a first code (before applying exclusion criteria), and box plot of delay in recruitment, restricted to persons both in the main cohort and in the ablation analysis keeping groups of prompts")
    pdf_file <- paste0(thisdirinput,"figures/Figure_ablation_",immdis,".pdf")
    
    if (file.exists(pdf_file)) { 
      fig_02_filename <- paste0(thisdirinput,"/figures/Figure_ablation_",immdis, ".png")
      pdf_convert(pdf_file, format = "png", dpi = 300, filenames = fig_02_filename)
    }else{
      fig_02_filename <- ""
    }
    
    fig_03_name <- paste0(" 10.",j,".2.3")
    fig_03_caption <- paste0("Impact of the components on cumulative incidence of first flare in the cohort of ", name_immdis[[immdis]],", per 1,000 persons")
    pdf_file <- paste0(thisdirinput,"figures/Figure_Impact_of_component_flare_",immdis,".pdf")
    
    if (file.exists(pdf_file)) { 
      fig_03_filename <- paste0(thisdirinput,"/figures/Figure_Impact_of_component_flare_",immdis, ".png")
      pdf_convert(pdf_file, format = "png", dpi = 300, filenames = fig_03_filename)
    }else{
      fig_03_filename <- ""
    }
    
    table_07_name <- paste0(" 10.",j,".2.2")
    table_07_caption <- paste0("Background incidence rate stratified per age categories, of first, second, and third flare in the cohort of ", name_immdis[[immdis]],", per 1,000 person-years, with 95% confidence intervals.")
    table_07_content <- fread(file = paste0(thisdirinput,"D6_Table_5_IR_ageband_",immdis,".csv"))
    # table_07_content[, N := seq(.N)]
    # tokeeplist <- c(3,4,5,8,9,10,13,14,15,18,19,20,23,24,25,28,29,30,33,34,35,38,39,40)
    # table_07_content <- table_07_content[N %in% tokeeplist,]
    # tocleanlist <- c(4,5,9,10,14,15,19,20,24,25,29,30,34,35,39,40)
    # table_07_content <- table_07_content[N %in% tocleanlist,Characteristics := ""]
    # table_07_content <- table_07_content[, N := NULL]
    # table_07_content <- table_07_content[, Gender := NULL]
    
    
    table_08_name <- paste0(" 10.",j,".2.3")
    table_08_caption <- paste0("Background incidence rate stratified per gender, of first, second, and third flare in the cohort of ", name_immdis[[immdis]],", per 1,000 person-years, with 95% confidence intervals.")
    table_08_content <- fread(file = paste0(thisdirinput,"D6_Table_5_IR_gender_",immdis,".csv"))
    # table_08_content[, N := seq(.N)]
    # tokeeplist <- c(6,7,8,12,13,14,18,19,20)
    # table_08_content <- table_08_content[N %in% tokeeplist,]
    # table_08_content <- table_08_content[, N := NULL]
    # table_08_content <- table_08_content[, Ageband := NULL]
    
     
    table_09_name <- paste0(" 10.",j,".1.4")
    table_09_caption <- paste0("Background incidence rate in the pregnant population of first, second, and third flare in the cohort of ", name_immdis[[immdis]],", per 1,000 person-years, with 95% confidence intervals.")
    table_09_content <- fread(file = paste0(thisdirinput,"D6_Table_4_IR_",immdis,".csv"))
    table_09_content[, N := seq(.N)]
    tokeeplist <- c(6,7,8,12,13,14,18,19,20)
    table_09_content <- table_09_content[N %in% tokeeplist,]
    table_09_content <- table_09_content[, N := NULL]
    
    table_10_name <- paste0(" 10.",j,".1.5")
    table_10_caption <- paste0("Six- and twelve-months cumulative incidence startified per age categories of flares of ", name_immdis[[immdis]],", per 1,000 persons, with 95% confidence intervals.")
    table_10_content <- fread(file = paste0(thisdirinput,"D6_Table_7_Cumulative_incidence",immdis,".csv"))
    table_10_content[, N := seq(.N)]
    tokeeplist <- c(2,3,4,14,15,16)
    table_10_content <- table_10_content[N %in% tokeeplist,]
    table_10_content <- table_10_content[, N := NULL]
    # table_10_content <- table_10_content[, Ageband := NULL]
    table_10_content <- table_10_content[, Gender := NULL]
    
    table_11_name <- paste0(" 11.",j,".1.5")
    table_11_caption <- paste0("Six- and twelve-months cumulative incidence startified per gender of flares of ", name_immdis[[immdis]],", per 1,000 persons, with 95% confidence intervals.")
    table_11_content <- fread(file = paste0(thisdirinput,"D6_Table_7_Cumulative_incidence",immdis,".csv"))
    table_11_content[, N := seq(.N)]
    tokeeplist <- c(5,6,17,18)
    table_11_content <- table_11_content[N %in% tokeeplist,]
    table_11_content <- table_11_content[, N := NULL]
    table_11_content <- table_11_content[, Ageband := NULL]
    # table_11_content <- table_11_content[, Gender := NULL]
    
    rmarkdown::render(paste0(dirmacro,"template_tables_figures_Obj2.Rmd"), output_format = "word_document", output_file = paste0(thisdiroutput,"/Section 10.",j," ",name_immdis[[immdis]],".docx"))
    
  }
}