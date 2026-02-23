
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_71_KM_figure"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c(immdis)
  thisdatasources_for_postprocessing <- c("TEST","TEST2")
  thisfolder_submission <- folders_submission
  thisfolder_submission[["TEST"]] <- paste0(thisdirinput,"folder_TEST/")
  thisfolder_submission[["TEST2"]] <- paste0(thisdirinput,"folder_TEST2/")
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


for (immdis in immune_diseases_in_this_step){ 
  # Figure KM
  
  print(paste("Now creating: Figure KM for", immdis))
  tab_nice <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    filetoread <- paste0(thisfolder_submission[[ds]], "D5_data_KM_figure_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for KM figure"))
      temp <- fread(filetoread)
      flag_empty <- if (nrow(temp) == 0) T else F
      temp[, tp := ds]
      temp[, label := name_ds[[ds]]]
      setnames(temp, "tp", "ds")
      
      tab_nice <- rbind(tab_nice,temp, fill = T)
    }else{
      print(paste0(ds, " has no D5 for KM figure"))
    }
  }
  
  if (!flag_empty) {
    tab_nice[,ci := 1- survival]
    tab_nice[,ci_ll := 1- ll]
    tab_nice[,ci_ul := 1- ul]
    
    # Function to generate the Kaplan-Meier plot with shadows drawn first
    generate_km_plot_direct <- function(dt, output_file = "kaplan_meier_curves.pdf") {
      
      # Set a list of 10 distinguishable colors
      colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
      
      # Create the Kaplan-Meier plot with ggplot2
      km_plot <- ggplot(dt, aes(x = day, color = label, fill = label)) +
        
        # Plot all the confidence intervals first
        geom_ribbon(aes(ymin = ci_ll, ymax = ci_ul), alpha = 0.2, color = NA) +  # Transparent shadows
        
        # Plot the survival curves on top
        geom_step(aes(y = ci), size = 1) +  # Draw the survival curves
        
        # Custom colors for both the curves and the shadows
        scale_color_manual(values = colors[1:length(unique(dt$ds))]) +
        scale_fill_manual(values = colors[1:length(unique(dt$ds))]) +
        
        # force scale of y axis to 0-1
        ylim(0, 1) +
        
        # Labels and theme settings
        labs(title = "Cumulative incidence curves", 
             x = "Days", 
             y = "Cumulative incidence (1- KM)", 
             color = "Data Source", 
             fill = "Data Source") +
        theme_minimal() +
        theme(legend.position = "right")
      
      # Save the plot as a PDF
      ggsave(output_file, km_plot, device = "pdf", width = 10, height = 7)
    }
    
    namefig <- paste0(thisdiroutput,thisdirfigure, "/Figure_KM_",  immdis,".pdf")
    
    # Call the function to generate and save the Kaplan-Meier plot
    generate_km_plot_direct(tab_nice, namefig)
    
    
  }
  
  # 
  
}


