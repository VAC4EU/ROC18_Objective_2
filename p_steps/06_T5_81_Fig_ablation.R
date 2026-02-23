
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_81_Fig_ablation"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI","V_PAN_AESI")
  thisdatasources_for_postprocessing <- c("TEST","TEST2","TEST3")
  thisfolder_submission <- folders_submission
  thisfolder_submission[["TEST"]] <- paste0(thisdirinput,"folder_TEST/")
  thisfolder_submission[["TEST2"]] <- paste0(thisdirinput,"folder_TEST2/")
  thisfolder_submission[["TEST3"]] <- paste0(thisdirinput,"folder_TEST3/")
  thisdirfigure <- "./"
}else{
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  if (localprocessing){
    # this happens at DEAPs
    thisdirinput <- dirtablesubpop[[subpop]]
    thisdiroutput <- dirD6
    thisdatasources_for_postprocessing <- thisdatasource
    thisfolder_submission <- folders_submission
    thisfolder_submission[[thisdatasource]] <- thisdirinput
    thisdirfigure <- "figures/"
  }else{
      # this happens at DRE
      thisdatasources_for_postprocessing <- datasources_for_postprocessing
      thisfolder_submission <- folders_submission
      thisdiroutput <- paste0(direxp,"Formatted tables/")
      thisdirfigure <- "figures/"
    }
}

true_groups_of_prompts <- groups_of_prompts[2:length(groups_of_prompts)]


tab_nice <- data.table()

tokeep <- c()
for (group in true_groups_of_prompts) {
  tokeep <- c(tokeep,paste0("p_1_",group),paste0("median_delay_",group), paste0("p25_delay_",group), paste0("p75_delay_",group) )
}

# Define colors for bars
group_colors <- c(
  "PC" = "blue", 
  "HOSP_DISP" = "darkorange", 
  "HOSP_SPEC_DISP" = "darkgreen"
)

for (immdis in immune_diseases_in_this_step){ 
  # Fig 8

  print(paste("Now creating: Figure ablation for", immdis))
  tab_nice <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_8_Impact_of_ablation_cohort_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread)  
      temp[, immdis := immdis]
      tab_nice <- rbind(tab_nice,temp, fill = T)
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }
  }
  # fwrite(tab_nice,"C:/temp/temp.csv")
  
  # Remove check variables
  check_vars <- colnames(tab_nice)[grepl("_check$", colnames(tab_nice))]
  tab_nice[, (check_vars) := NULL]
  
  # Melt the dataset into long format
  long_tab <- melt(
    tab_nice,
    id.vars = "ds",
    measure.vars = patterns("^p_1_", "^median_delay_", "^p25_delay_", "^p75_delay_"),
    variable.name = "group_of_prompts",
    value.name = c("p1", "median_delay", "p25_delay", "p75_delay")
  )
  # Clean up group_of_prompts to match "PC", "HOSP_DISP", "HOSP_SPEC_DISP"
  group_labels <- c(name_group[["PC"]], name_group[["HOSP_DISP"]], name_group[["HOSP_SPEC_DISP"]])
  long_tab[, group_of_prompts := factor(group_of_prompts, levels = 1:3, labels = group_labels)]
  
  # long_tab[, name_group_of_prompts := name_group[group_of_prompts]]
  long_tab[, name_ds := unlist(name_ds[ds])]
  long_tab[, name_ds := factor(name_ds)]
  long_tab[, wrapped_name_ds := str_wrap(name_ds, width = 15)]
  
  
  # # Set colors for groups
  # group_colors <- c(
  #   "PC" = "lightblue", 
  #   "HOSP_DISP" = "darkorange", 
  #   "HOSP_SPEC_DISP" = "darkgreen"
  # )
  
  group_colors <- c(
      "PC" = "lightblue" , 
      "Inpatient" = "darkorange", 
      "Inpatient or specialist" = "darkgreen"
   )

  
  # Add a new variable to distinguish between the bar plot and box plot
  long_tab[, plot_type := "Bar Plot"] # Default all rows to "Bar Plot"
  
  # Duplicate the dataset for the box plot
  box_tab <- copy(long_tab)
  box_tab[, plot_type := "Box Plot"] # Label this data as "Box Plot"
  
  # Combine the datasets for bar plot and box plot
  plot_data <- rbind(long_tab, box_tab)
  plot_data <- plot_data[,median_delay := as.numeric(median_delay)]
  plot_data <- plot_data[,p75_delay := as.numeric(p75_delay)]
  plot_data <- plot_data[,p25_delay := as.numeric(p25_delay)]
  plot_data <- plot_data[is.na(median_delay), median_delay := 0 ]
  plot_data <- plot_data[is.na(p75_delay), p75_delay := 0 ]
  plot_data <- plot_data[is.na(p25_delay), p25_delay := 0 ]
  
  facet_labels <- c(
    "Bar Plot" = "Percentage",
    "Box Plot" = "Days of delay"
  )
  
  # Create the combined plot with the updated changes
  p <- ggplot(plot_data, aes(x = wrapped_name_ds, fill = group_of_prompts)) +
    # Bar plot layer (only for Bar Plot data)
    geom_bar(
      data = plot_data[plot_type == "Bar Plot"],
      aes(y = p1),
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.6
    ) +
    # Box plot layer (only for Box Plot data)
    geom_boxplot(
      data = unique(plot_data[plot_type == "Box Plot"]),
      aes(
        ymin = p25_delay,
        lower = p25_delay,
        middle = median_delay,
        upper = p75_delay,
        ymax = p75_delay,
        group = interaction(name_ds, group_of_prompts) # Explicit grouping
      ),
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.6
    ) +
    # Facet by plot_type with custom labeller
    facet_grid(
      plot_type ~ ., 
      scales = "free_y", 
      labeller = as_labeller(facet_labels),
      switch = "y"
    ) +
    # Colors and theme
    scale_fill_manual(values = group_colors, name = "Group of Prompts") +
    labs(x = "Data source", y = "") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank() ,
      strip.text.y = element_blank(), # Remove vertical facet titles
      strip.text.y.left = element_text(size = 12, face = "bold") #, # Style for left facet labels
    )
  
  # Print the plot
  print(p)
  
  
  namefig <-paste0(thisdiroutput,thisdirfigure, "/Figure_ablation_",  immdis,".pdf")
  
  ggsave(namefig, p , device = "pdf", width = 10, height = 7)
}
