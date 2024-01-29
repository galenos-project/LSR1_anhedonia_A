split_columns <- function(x) {
  # Identify columns containing ';' in any of their values
  cols_to_split <- sapply(x, function(column) any(grepl(";", column)))


  # For each column that needs splitting
  for(col in names(x)[cols_to_split]) {
    # Create two new columns with appropriate names
    x <- x %>%
      separate(col, into = c(paste0(col, "[1]"), paste0(col, "[2]")),
               sep = ";", fill = "right", remove = TRUE) %>% 
      # Replace empty strings with NA in the new columns
      mutate(across(c(paste0(col, "[1]"), paste0(col, "[2]")), ~ replace(., . == "", NA)))
  }
  return(x)
}


positive_control_cohort_type <- function(x) {
  # Identify the groups that have 'Combination intervention'
  combination_groups <- x %>%
    filter(CohortType == "Combination intervention") %>%
    select(StudyId, ExperimentID, OutcomeId, TimeInMinute) %>%
    distinct() %>%
    mutate(GroupID = interaction(StudyId, ExperimentID, OutcomeId, TimeInMinute))
  
  # Update the CohortType for matching groups in the original dataframe
  x <- x %>%
    mutate(GroupID = interaction(StudyId, ExperimentID, OutcomeId, TimeInMinute)) %>%
    mutate(CohortType = ifelse(CohortType == "Positive control" & 
                                 (GroupID %in% combination_groups$GroupID),
                               "Control for combination intervention", 
                               CohortType)) %>%
    mutate(CohortType = ifelse(CohortType == "Positive control treated sham" & 
                                 (GroupID %in% combination_groups$GroupID),
                               "Sham for combination intervention", 
                               CohortType))
  return(x)
}

add_missing_sham <- function(x) {
  sham_presence <- x %>% 
    group_by(GroupID) %>% 
    summarise(HasSham = any(str_detect(RoleOfCohort, "S")))
  
  missing_sham <- sham_presence %>%
    filter(HasSham == FALSE) %>% 
    select(-HasSham)
  
  missing_sham_data <- missing_sham %>%
    left_join(x %>% 
                distinct(StudyId, ExperimentID, ExperimentLabel, OutcomeLabel, OutcomeId, GroupID, ExperimentType, .keep_all = TRUE), 
              by = "GroupID") %>% #just info from takes first study in that group of GroupID
    mutate(RoleOfCohort = "S") %>% 
    mutate(CohortType = "Imputed sham") %>% 
    mutate(across(.cols = !c("StudyId", "ExperimentID", "ExperimentLabel", "OutcomeLabel", "OutcomeId", "CohortType", "GroupID", "ExperimentType", "RoleOfCohort"),
                  .fns = ~ NA))
  
  y <- x %>% 
    bind_rows(missing_sham_data) %>% 
    arrange(StudyId, GroupID)
  
  return(y)
}
