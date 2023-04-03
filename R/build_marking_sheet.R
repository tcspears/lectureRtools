#' Build marking sheet from essays downloaded from Blackboard
#' 
#' @param dir_list A list (or vector) of paths to essays
#' @return A data.frame of essay IDs
#' @import stringr
#' @import dplyr
#' @import tibble
#' @import fs
#' @import purrr
#' @export

build_marking_sheet <- function(dir_list){
  get_ids <- function(dir_path) {
    # List all files in the directory
    all_files <- dir_ls(dir_path)
    
    # Extract file information using regex
    file_info <- all_files %>%
      as_tibble() %>%
      rename(file_path = value) %>%
      mutate(file_name = path_file(file_path),
             essay_id = str_extract(file_name, "\\d{9}"),
             student_id = str_extract(file_name, "B(_?\\d{6})"))
    
    # Replace underscore if present in student_id
    file_info <- file_info %>%
      mutate(student_id = ifelse(!is.na(student_id), gsub("_", "", student_id), NA))
    
    # Filter out invalid files and create the final data.frame
    valid_files <- file_info %>%
      filter(!is.na(essay_id)) %>%
      select(file_path, student_id, essay_id)
    
    return(valid_files)
  }
  results_list <- map(dir_list, get_ids)
  combined_results <- bind_rows(results_list)
  return(combined_results)
}


