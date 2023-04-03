#' Extract email addresses from Blackboard user output
#' 
#' @param path_blackboard_email_file Path to Blackboard user information file (in CSV format)
#' @return A string containing the email addresses, separated by ';'
#' @import dplyr
#' @import readr
#' @import purrr
#' @export

extract_emails <- function(path_blackboard_email_file){
  students <- readr::read_csv(path_blackboard_email_file)
  emails <- students %>%
    dplyr::mutate(Email = paste0(`Username`, "@ed.ac.uk", sep = "")) %>%
    dplyr::summarise(ToField = paste(Email, collapse="; "))
  return(emails$ToField)
}