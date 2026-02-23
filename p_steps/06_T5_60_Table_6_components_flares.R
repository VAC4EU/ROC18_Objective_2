
if (TEST){ 
  # this happens during development
  immdis <- "E_GRAVES_AESI"
  testname <- "test_06_T5_60_Table_6_components_flares"
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
      thisdiroutput <- dirD6
      thisdiroutputfig <- paste0(thisdiroutput,"/figures/")
      thisdirfigure <- "../figures/"
      dir.create(thisdiroutputfig,showWarnings = F)
    }
}


for (immdis in immune_diseases_in_this_step){ 
  # Table 6

  print(paste("Now creating: Table 6 in nice format for", immdis))

  list_of_figures <- c()
  titles <- c()
  tab_bind <- data.table()
  for (ds in thisdatasources_for_postprocessing) {
    filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_6_Impact_of_component_flare_", ds,"_", immdis, ".csv")
    if (file.exists(filetoread)) {
      print(paste0(ds, " has D5 for this table"))
      temp <- fread(filetoread)  
      tab_nice <- temp # rbind(tab_nice,temp, fill = T)
      tempds <- copy(temp)
      tempds[, datasource := name_ds[[ds]]]
      setcolorder(tempds,"datasource")
      tab_bind <- rbind(tab_bind,tempds, fill = T)
      ########################################
      # save
      
      outputfile <- tab_nice
      nameoutput <- paste0("D6_Table_6_Impact_of_component_flare_", ds,"_", immdis)
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
      # # rtf
      # doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
      # print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))
      
      
    }else{
      print(paste0(ds, " has no D5 for this table"))
    }

    # figure of components
    
  if (ds %in% datasources_with_flat_structure) {
      figure <- paste0(thisfolder_submission[[ds]], "Figure_components_flares_", ds,"_", immdis, ".pdf")
  }else{
    figure <- paste0(thisfolder_submission[[ds]],thisdirfigure, "Figure_components_flares_", ds,"_", immdis, ".pdf")
  }
    
    print(figure)
    if (file.exists(figure) & ds != "PEDIANET" ) {
      list_of_figures <- c(list_of_figures,figure)
      titles <- c(titles,name_ds[[ds]])
      print(paste0(ds, " has the component figure"))
    }else {
      print(paste0(ds, " does not have the component figure"))
    }
  }
  # save
  
  outputfile <- tab_bind
  nameoutput <- paste0("D6_Table_6_Impact_of_component_flare_all_ds_", immdis)
  assign(nameoutput, outputfile)
  # xls
  write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
  convert_pdf_to_png <- function(pdf_file) {
    # png_filename <- sub("\\.pdf$",".png",pdf_file)
    # png_output = paste0(thisdiroutputfig,png_filename)
    # pdf_images <- pdftools::pdf_convert(pdf_file, format = "png", dpi = 300, filenames = png_output)
    pdf_images <- pdftools::pdf_convert(pdf_file, format = "png", dpi = 300)
    return(pdf_images)
  }

  if (length(list_of_figures) > 0) {
    print("enter")
    # Assuming all your figures are in list_of_figures
    list_of_images <- lapply(list_of_figures, convert_pdf_to_png)
    
        
    # Flatten the list of image file names (as pdf_convert may generate multiple files per PDF)
    flattened_images <- unlist(list_of_images)

    # Arrange the images in two columns, with titles
    arrange_images_in_pdf <- function(image_files, titles, output_pdf) {
      # Create a list to hold the grobs (graphical objects)
      grobs <- list()
      
      for (i in seq_along(image_files)) {
        img_png <- readPNG(image_files[i])
        
        # Create grobs for the title and image
        title_grob <- textGrob(titles[i], gp = gpar(fontsize = 12, fontface = "bold"))  # Title for the figure
        image_grob <- rasterGrob(img_png)
        
        # Combine title and image grobs vertically with tighter spacing
        combined_grob <- arrangeGrob(
          title_grob, image_grob, 
          ncol = 1, 
          heights = c(0.1, 0.9)  # Reduce the height for the title, making it closer to the image
        )
        grobs <- c(grobs, list(combined_grob))
      }
      
      # Arrange images in a 2-column layout using grid.arrange
      pdf(output_pdf, width = 8.5, height = 11)  # Adjust dimensions for portrait mode
      grid.arrange(grobs = grobs, ncol = 2)
      dev.off()
    }
    
    
    # Call the function to generate the PDF
    arrange_images_in_pdf(flattened_images, titles, paste0(thisdiroutputfig, "/Figure_Impact_of_component_flare_",  immdis,".pdf"))
  
  }
}
