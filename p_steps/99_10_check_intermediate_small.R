# TERMINOLOGY
# GT = ground truth
# OP = output program

# TODO to remove
# folders_to_be_tested <- ""

# Create empty excel and datasets
wb_general <- wb_workbook()
wb_general$add_worksheet("Summary")
wb_general$add_dxfs_style(name = "negStyle", bg_fill = wb_color(name = "firebrick2"))
wb_general$add_dxfs_style(name = "posStyle", bg_fill = wb_color(name = "forestgreen"))
wb_general$add_dxfs_style(name = "misStyle", bg_fill = wb_color(name = "yellow2"))
wb_general$add_dxfs_style(name = "nogtStyle", bg_fill = wb_color(name = "deepskyblue3"))

df_colors <- list()
df_colors[["OP"]][["GT_not_other"]] <- "yellow"
df_colors[["OP"]][["other_not_GT"]] <- "khaki"
df_colors[["QC"]][["GT_not_other"]] <- "deepskyblue"
df_colors[["QC"]][["other_not_GT"]] <- "skyblue"

i_folder <- "i_input_synthetic"
g_folder <- "master_test_intermediate_small"
general_df <- data.table()

dir.create(file.path(thisdir, "g_parameters"), showWarnings = F)
dir.create(file.path(dirtests, g_folder), showWarnings = F, recursive = T)

full_list_of_steps <- list.files(file.path(thisdir, i_folder))
if (length(folders_to_be_tested) != 0 & any(folders_to_be_tested != "")) {
  
  steps_not_found <- folders_to_be_tested[folders_to_be_tested %not in% full_list_of_steps]
  steps_found <- folders_to_be_tested[folders_to_be_tested %in% full_list_of_steps]
  if (length(steps_not_found) > 0) {
    warning(paste("Steps:", paste(steps_not_found, collapse = ", "), "have not been found and won't be tested"))
  }
  if (length(steps_found) == 0) {
    stop("No steps left to check")
  }
  rm(steps_not_found)
} else {
  steps_found <- full_list_of_steps
}

tests_path <- list()

# TODO add this
# rm(full_list_of_steps, folders_to_be_tested)

for (single_step in steps_found) {
  
  print(paste("Running test",single_step))
  
  # TODO remove this
  # single_step <- steps_found[[7]]
  
  metadata_df <- data.table()
  folder_df <- data.table()
  test_results <- data.table()
  error_datasets <- list()
  
  step_folder <- file.path(thisdir, i_folder, single_step)
  dir.create(file.path(dirtests, single_step), showWarnings = F, recursive = T)
  
  # list of datasets
  listdatasets <- list()
  listdatasets[["GT"]] <- list.files(file.path(step_folder, "i_ground_truth"), pattern = "\\.rds$|\\.csv", full.names = T)
  listdatasets[["OP"]] <- list.files(file.path(step_folder, "g_output_program"), pattern = "\\.rds$|\\.csv", full.names = T)
  listdatasets[["QC"]] <- list.files(file.path(step_folder, "g_output_QC"), pattern = "\\.rds$|\\.csv", full.names = T)
  listdatasets_ext <- rapply(listdatasets, tools::file_ext, how = "replace")
  
  custom_message <- list(OP = "output program", QC = "quality check")
  
  # Check if OP and QC folders are empty
  folders_to_test <- c("OP", "QC")
  folders_to_test <- folders_to_test[sapply(folders_to_test, function(x) !identical(listdatasets[[x]], character(0)))]
  current_folders <- c("GT", folders_to_test)
  
  current_folders <- names(listdatasets)[sapply(names(listdatasets), function(x) !identical(listdatasets[[x]], character(0)))]
  folders_to_test <- setdiff(current_folders, "GT")
  
  # TODO add case GT missing
  
  filename <- lapply(listdatasets,  function(x) fs::path_ext_remove(basename(x)))
  folder_df <- rbindlist(lapply(folders_to_test, function(x) check_filenames(filename, "GT", x, custom_message[[x]])))
  
  wb <- wb_workbook()
  wb$add_worksheet("General")
  invisible(lapply(filename[["GT"]], function(x) wb$add_worksheet(x)))
  wb$add_dxfs_style(name = "negStyle", bg_fill = wb_color(name = "firebrick2"))
  wb$add_dxfs_style(name = "posStyle", bg_fill = wb_color(hex = "forestgreen"))
  wb$add_dxfs_style(name = "misStyle", bg_fill = wb_color(hex = "yellow2"))
  
  for (name_GT in filename[["GT"]]) {
    
    hash_location <- sapply(current_folders, function(x) file.path(thisdir, "g_parameters",
                                                                   paste0(name_GT, "_hash_", x, ".csv")), simplify = F)
    file_location <- sapply(current_folders,
                            function(x) fs::path_rel(listdatasets[[x]][filename[[x]] == name_GT]), simplify = F)
    metadata_last_commit <- lapply(file_location, extract_last_hash)
    last_commit_hash <- sapply(metadata_last_commit, `[[`, "final_commit_id")
    
    table_to_add <- data.table(folder = c("GT", folders_to_test), filename = name_GT, hash = last_commit_hash,
                               date = sapply(metadata_last_commit, function(x) lubridate::format_ISO8601(as_datetime(
                                 git2r:::as.POSIXct.git_time(x[["final_signature"]][["when"]])))),
                               extension = sapply(c("GT", folders_to_test),
                                                  function(x) listdatasets_ext[[x]][filename[[x]] == name_GT]),
                               old_hash = c(sapply(hash_location, function(x) if (file.exists(x)) read.csv(x)[[1]] else "")))
    table_to_add[, changed := hash != old_hash]
    
    metadata_df <- rbindlist(list(metadata_df, table_to_add))
    rm(table_to_add)
    
    invisible(lapply(current_folders, function(x) write.csv(last_commit_hash[[x]], hash_location[[x]], row.names = FALSE)))
    
    # Load dataset
    GT <- readRDS(file_location[["GT"]])
    GT <- as.data.table(GT)
    
    for (folder in folders_to_test) {
      
      other <- smart_load(name_GT, dirname(file_location[[folder]]), subpop = F,
                          extension = listdatasets_ext[[folder]][filename[[folder]] == name_GT], return = T)
      other <- as.data.table(other)
      
      for (sngl_cl in colnames(other)) {
        if (sngl_cl %not in% colnames(GT)) {
          next
        }
        if (all(is.na(other[[sngl_cl]])) & all(is.na(GT[[sngl_cl]]))) {
          other[, (sngl_cl) := GT[, .SD, .SDcols = sngl_cl]]
          setattr(other[[sngl_cl]], "class", class(GT[[sngl_cl]]))
        }
        if (!identical(class(GT[[sngl_cl]]), class(other[[sngl_cl]]))) {
          if ("IDate" %in% class(GT[[sngl_cl]])) {
            other[, (sngl_cl) := as.IDate(get(sngl_cl))]
          }
          if ("Date" %in% class(GT[[sngl_cl]])) {
            other[, (sngl_cl) := as.Date(get(sngl_cl))]
          }
        }
      }
      
      test_results <- rbindlist(list(test_results, check_columns(GT, other, name_GT, custom_message[[folder]])))
      
      nrow_GT <- nrow(GT)
      nrow_other <- nrow(other)
      
      if (nrow_other > nrow_GT) {
        result <- F
        msg <- paste0("The ", custom_message[[folder]], " dataset has more records than the ground truth dataset")
      } else if (nrow_other < nrow_GT) {
        result <- F
        msg <- paste0("The ", custom_message[[folder]], " dataset has less records than the ground truth dataset")
      } else {
        result <- T
        msg <- ""
      }
      
      single_test <- data.table(folder = folder, filename = name_GT, type = "Check number records",
                                result = result, comment = msg)
      test_results <- rbindlist(list(test_results, single_test), use.names=TRUE)
      
      join_cols <- intersect(colnames(GT), colnames(other))
      
      GT_not_other <- tryCatch(
        expr = GT[!other, on = join_cols],
        error = function(e){
          return(e)
        }
      )
      
      other_not_GT <- tryCatch(
        expr = other[!GT, on = join_cols],
        error = function(e){
          return(e)
        }
      )
      
      result <- c()
      msg <- c()
      if ("error" %in% class(GT_not_other) | "error" %in% class(other_not_GT)) {
        if ("error" %in% class(GT_not_other)) {
          result <- c(result, F)
          msg <- c(msg, GT_not_other[[1]])
        }
        if ("error" %in% class(other_not_GT)) {
          result <- c(result, F)
          msg <- c(msg, other_not_GT[[1]])
        }
      } else if (nrow(GT_not_other) > 0 | nrow(other_not_GT) > 0 ) {
        if (nrow(GT_not_other) > 0) {
          result <- c(result, F)
          msg <- c(msg, "The ground truth has at least one row not present in the output program dataset")
          error_datasets[[name_GT]][[folder]][["GT_not_other"]] <- GT_not_other
        }
        if (nrow(other_not_GT) > 0) {
          result <- c(result, F)
          msg <- c(msg, "The output program has at least one row not present in the ground truth dataset")
          error_datasets[[name_GT]][[folder]][["other_not_GT"]] <- other_not_GT
        }
      } else {
        result <- T
        msg <- ""
      }
      
      single_test <- data.table(folder = folder, filename = name_GT, type = "Dataset is similar",
                                result = result, comment = msg)
      test_results <- rbindlist(list(test_results, single_test), use.names=TRUE)
      
    }
    
  }
  
  setcolorder(metadata_df, c("folder", "filename", "hash", "date", "extension", "changed", "old_hash"))
  wb$add_data_table(sheet = "General", metadata_df)
  wb$set_col_widths(cols = which(colnames(metadata_df) == "filename"), widths = max(nchar(metadata_df[, filename])) + 1)
  wb$set_col_widths(cols = which(colnames(metadata_df) == "date"), widths = max(nchar(metadata_df[, date])) - 2)
  
  overall_result <- data.table()
  
  if (!identical(folders_to_test, character(0))) {
    
    base_row_table_2 <- nrow(metadata_df) + 3
    wb$add_data_table(sheet = "General", folder_df, startRow = nrow(metadata_df) + 3)
    excel_col <- LETTERS[which(colnames(folder_df) == "result")]
    wb$add_conditional_formatting(dims = paste0(excel_col, base_row_table_2, ":", excel_col, base_row_table_2 + nrow(folder_df)),
                                  type = "containsText", rule = "FALSE", style = "negStyle")
    wb$add_conditional_formatting(dims = paste0(excel_col, base_row_table_2, ":", excel_col, base_row_table_2 + nrow(folder_df)),
                                  type = "containsText", rule = "TRUE", style = "posStyle")
    wb$add_conditional_formatting(dims = paste0(excel_col, base_row_table_2, ":", excel_col, base_row_table_2 + nrow(folder_df)),
                                  type = "containsText", rule = "MISSING", style = "misStyle")
    wb$add_conditional_formatting(dims = paste0(excel_col, base_row_table_2, ":", excel_col, base_row_table_2 + nrow(folder_df)),
                                  type = "containsText", rule = "NO GT", style = "nogtStyle")
    
    # TODO test to find best solution
    comment_excel_col <- LETTERS[which(colnames(folder_df) == "comment")]
    wb$add_cell_style(sheet = "General", dims = paste0(comment_excel_col, base_row_table_2, ":",
                                                       comment_excel_col, (base_row_table_2 + nrow(folder_df))),
                      wrap_text = TRUE)
    
    # setnames(test_results, c("folder", "result", "comment"), c("Folder", "Result", "Comment"))
    test_results[, index_1 := .GRP, by = c("filename", "type")]
    test_results[, index_2 := 1:.N, by = c("folder", "filename", "type")]
    test_results_wide <- dcast(test_results, filename + type + index_1 + index_2 ~ folder, value.var = c("result", "comment"))
    test_results[, c("index_1", "index_2") := NULL]
    test_results_wide[, c("index_1", "index_2") := NULL]
    setnames(test_results_wide, paste0("result_", folders_to_test), folders_to_test)
    
    cols_to_add <- setdiff(folders_to_test, colnames(test_results_wide))
    if (length(cols_to_add) > 0) test_results_wide[, (cols_to_add) := "MISSING"]
    
    setcolorder(test_results_wide, c("type", folders_to_test, paste0("comment_", folders_to_test)))
    
    test_results_wide <- split(test_results_wide, by = "filename", keep.by = F)
    
    cols_to_format <- which(colnames(test_results_wide[[1]]) %in% folders_to_test)
    start_excel_col <- LETTERS[min(cols_to_format)]
    end_excel_col <- LETTERS[max(cols_to_format)]
    
    for (df_name in names(test_results_wide)) {
      wb$add_data_table(sheet = df_name, test_results_wide[[df_name]])
      wb$set_col_widths(sheet = df_name, cols = which(colnames(test_results_wide[[df_name]]) == "type"),
                        widths = max(nchar(test_results_wide[[df_name]][, type])))
      
      end_excel_row <- nrow(test_results_wide[[df_name]]) + 1
      wb$add_conditional_formatting(dims = paste0(start_excel_col, "2:", end_excel_col, end_excel_row),
                                    type = "containsText", rule = "FALSE", style = "negStyle")
      wb$add_conditional_formatting(dims = paste0(start_excel_col, "2:", end_excel_col, end_excel_row),
                                    type = "containsText", rule = "TRUE", style = "posStyle")
      wb$add_conditional_formatting(dims = paste0(start_excel_col, "2:", end_excel_col, end_excel_row),
                                    type = "containsText", rule = "MISSING", style = "misStyle")
      
      find_last_cell <- function(wb, sheet, type = "both") {
        raw_data <- wb$worksheets[[which(wb$sheet_names == sheet)]]$sheet_data$cc
        last_row <- as.integer(max(as.integer(raw_data$row_r), na.rm = T))
        last_col <- max(raw_data[which(as.integer(raw_data$row_r) == last_row), "c_r"])
        if (type == "row") {
          return(last_row)
        } else if (type == "col") {
          return(last_col)
        } else {
          return(paste0(last_col, last_row))
        }
      }
      
      for (folder in names(error_datasets[[df_name]])) {
        
        for (df_type in names(error_datasets[[df_name]][[folder]])) {
          start_cell <- paste0("A", find_last_cell(wb, df_name, type = "row") + 2)
          wb$add_data(sheet = df_name, c(folder, df_type), array = T,
                      start_row = find_last_cell(wb, df_name, type = "row") + 2)
          wb$add_data(sheet = df_name, error_datasets[[df_name]][[folder]][[df_type]],
                      start_row = find_last_cell(wb, df_name, type = "row") + 1)
          end_cell <- find_last_cell(wb, df_name)
          wb$add_fill(sheet = df_name, dims = paste0(start_cell, ":", end_cell),
                      color = wb_color(name = df_colors[[folder]][[df_type]]))
        }
      }
    }
    
    overall_result <- rbind(folder_df[, .(folder, result)], test_results[, .(folder, result)])
    overall_result <- overall_result[, .(result = all(result)), by = folder]
    overall_result[, Step := single_step]
    setnames(overall_result, c("folder", "result"), c("Folder", "Result"))
    setcolorder(overall_result, c("Step", "Folder", "Result"))
    
    overall_result[, Result := as.character(Result)]
    overall_result <- dcast(overall_result, Step ~ Folder, value.var = "Result")
  }
  
  if ("GT" %not in% current_folders) {
    overall_result[, Step := single_step]
    overall_result[, (folders_to_test) := "NO GT"]
  }
  
  cols_to_add <- setdiff(c("OP", "QC"), colnames(overall_result))
  if (length(cols_to_add) > 0) {
    overall_result[, Step := single_step]
    overall_result[, (cols_to_add) := "MISSING"]
  }
  
  general_df <- rbindlist(list(general_df, overall_result), use.names=TRUE)
  
  # wb$open()
  tests_path[[single_step]] <- file.path(dirtests, single_step,
                                         paste0("QC_results_", single_step, "_",
                                                base::format(.POSIXct(Sys.time(), "GMT"), format = "%Y%m%dT%H%M%S"),
                                                ".xlsx"))
  wb$save(tests_path[[single_step]], overwrite = TRUE)
}

general_df[, Link := "link"]
setcolorder(general_df, c("Step", "Link"))
wb_general$add_data_table(sheet = 1, general_df)

wb_general$add_conditional_formatting(dims = paste0("B1:C", nrow(general_df) + 1), type = "containsText",
                                      rule = "FALSE", style = "negStyle")
wb_general$add_conditional_formatting(dims = paste0("B1:C", nrow(general_df) + 1), type = "containsText",
                                      rule = "TRUE", style = "posStyle")
wb_general$add_conditional_formatting(dims = paste0("B1:C", nrow(general_df) + 1), type = "containsText",
                                      rule = "MISSING", style = "misStyle")
wb_general$set_col_widths(cols = 1, widths = max(nchar(general_df[, Step])) + 1)

for (i in seq_along(tests_path)) {
  x <- paste0('=HYPERLINK(LEFT(CELL("filename"), FIND("master_test_intermediate_small", CELL("filename"), 1) - 1) & "',
              file.path(names(tests_path)[[i]], basename(tests_path[[i]])), '", "Link")')
  wb_general$add_formula(x = x, dims = paste0("B", i + 1))
}

wb_general$save(file.path(dirtests, g_folder,
                          paste0("QC_results_",
                                 base::format(.POSIXct(Sys.time(), "GMT"), format = "%Y%m%dT%H%M%S"), ".xlsx")),
                overwrite = TRUE)
