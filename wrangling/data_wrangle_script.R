library(dplyr)
library(stringr)
library(tibble)
library(tidyverse)
library(tidyr)
library(purrr)
library(metafor)

source("wrangling/wrangling_functions.R", local = TRUE)

LSR <- 'LSR1'

# Import SyRF outcome data
LSR1_SyRFOutcomes <- read_csv("data/Quantitative_data_-_2024_01_31_-_e98bc7cf-dfa7-47b9-a9fa-344406c46e1c_-_Investigators_Unblinded.csv")

###### Tidying and cleaning the data ######
#clean ; from TiAb etc
LSR1_SyRFOutcomes$Title <- gsub(";", ":", LSR1_SyRFOutcomes$Title)
LSR1_SyRFOutcomes$Abstract <- gsub(";", ":", LSR1_SyRFOutcomes$Abstract)
LSR1_SyRFOutcomes$Authors <- gsub(";", ":", LSR1_SyRFOutcomes$Authors)
LSR1_SyRFOutcomes$PublicationName <- gsub(";", ":", LSR1_SyRFOutcomes$PublicationName)
LSR1_SyRFOutcomes$AlternateName <- gsub(";", ":", LSR1_SyRFOutcomes$AlternateName)
LSR1_SyRFOutcomes$Url <- gsub(";", ":", LSR1_SyRFOutcomes$Url)
LSR1_SyRFOutcomes$AuthorAddress <- gsub(";", ":", LSR1_SyRFOutcomes$AuthorAddress)
LSR1_SyRFOutcomes$Doi <- gsub(";", ":", LSR1_SyRFOutcomes$Doi)
LSR1_SyRFOutcomes$Keywords <- gsub(";", ":", LSR1_SyRFOutcomes$Keywords)
LSR1_SyRFOutcomes$CustomId <- gsub(";", ":", LSR1_SyRFOutcomes$CustomId)
LSR1_SyRFOutcomes$Year<- gsub(";", ":", LSR1_SyRFOutcomes$Year)
LSR1_SyRFOutcomes$ReferenceType <- gsub(";", ":", LSR1_SyRFOutcomes$ReferenceType)
LSR1_SyRFOutcomes$PdfRelativePath <- gsub(";", ":", LSR1_SyRFOutcomes$PdfRelativePath)




# Filter for reconciled studies (and rename columns for consistency with shiny outcomes/remove SYRF columnns)
LSR1_reconciled <- LSR1_SyRFOutcomes %>% 
  filter(`Is this a reconciliation?` == TRUE) %>%  #for PTSD `is this a reconciliation?`
  rename(ModelID = `DiseaseModelId(s)`, 
         ExperimentID = ExperimentId, 
         InterventionLabel = `TreatmentLabel(s)`,
         InterventionID = `TreatmentId(s)`,
         OutcomeResult = OutcomeAverage)

## first pass for removing duplicate reconciliations
recent_reconciledID <- LSR1_reconciled %>%
  arrange(desc(DateTimeDataEntered)) %>%
  group_by(StudyId) %>%
  slice(1) %>%
  ungroup()

# Filter all records that have a StudyIdStr and AnnotatorIdStr that is in the reconciled_study_annotator_pairs, to get all reconciled data
#reconciled_records_unique <- LSR1_reconciled %>%
#  semi_join(recent_reconciledID, by = "StudyId")

## Some studies were split due to their complexity, with RoB/ Arrive only entered once
## the ARRIVE/RoB data for the second split needs overwritten with the values from the first
#if this is the case, identify the studyIds concerned, recort in ll48 and 49, and change ll52-59 accordingly

## Match split studies [Get pairs from bibliographic download on SyRF]
## Find position of the start and end of ARRIVE ROB columns to store column names between the start and end index
col_names <- names(LSR1_reconciled)
start_index <- match("Title", col_names)
end_index <- match("Is any role of the funder in the design/analysis/reporting of study described?", col_names)
ARRIVEROB_columns <- col_names[start_index:end_index] 






## 3. Step to remove observations from an accidental dual-reconciliation: choose the most recent reconciliation (unlikely to be necessary)
# Choose the most recent reconciliation 
recent_reconciledID <- LSR1_reconciled %>%
  arrange(desc(DateTimeDataEntered)) %>%
  group_by(StudyId) %>%
  slice(1) %>%
  ungroup()

# Filter all records that have a StudyIdStr and AnnotatorIdStr that is in the reconciled_study_annotator_pairs to get final set of reconciled data
reconciled_records_unique <- LSR1_reconciled %>%
  semi_join(recent_reconciledID, by = c("StudyId", "InvestigatorId"))

# Rearrange rows to be grouped by studies and then experiment within studies. Reorder columns for readability
reconciled_records <- reconciled_records_unique %>%
  arrange(StudyId, ExperimentID, CohortId, OutcomeId, InterventionID) %>% 
  relocate(ExperimentLabel, .after = ExperimentID) %>% 
  relocate(CohortLabel, .after = CohortId) %>% 
  relocate(OutcomeLabel, .after = OutcomeId) %>% 
  relocate(c(OutcomeLabel, OutcomeId), .after = ExperimentLabel) %>% 
  relocate(c(InterventionLabel), .after = InterventionID)

savename <- paste0('reconciled_records_',Sys.Date(),'.csv')
write.csv(reconciled_records, savename)

# Split any column which contains multiple responses (separated by ';') into seperate columns
reconciled_records <- split_columns(reconciled_records)

# Label types of cohorts
# Aim = Make each experiment one comparison (with sham where present, control and intervention)
## Differentiate between positive and negative controls and TAAR1KO's

  
  reconciled_cohort_label <- reconciled_records %>%
    mutate(
      Treatment1Type = case_when(
        str_detect(`InterventionLabel[1]`, regex("veh|con", ignore_case = TRUE)) ~ "Negative control",
        is.na(`InterventionLabel[1]`) ~ NA_character_,
        TRUE ~ "Intervention"
      ),
      Treatment1Typesupp = case_when(
        str_detect(`InterventionLabel[2]`, regex("veh|con", ignore_case = TRUE)) ~ "Negative control",
        is.na(`InterventionLabel[2]`) ~ NA_character_,
        TRUE ~ "Intervention"
      )
    )
  
  

## sort the blank disease models (which are equivalent to sham)
reconciled_cohort_label <- reconciled_cohort_label %>%
  mutate(
    `IsDiseaseModelControl[1]` = case_when(
      `IsDiseaseModelControl[1]` == 'True' ~ TRUE,
      `IsDiseaseModelControl[1]` == 'False' ~ FALSE,
      is.na(`IsDiseaseModelControl[1]`) ~ TRUE,
      TRUE ~ as.logical(NA)  # default case if none of the conditions are met
    )
  )


reconciled_cohort_label <- reconciled_cohort_label %>%
  mutate(
    `IsDiseaseModelControl[2]` = case_when(
    `IsDiseaseModelControl[2]`  == 'True' ~ TRUE,
      `IsDiseaseModelControl[2]`  == 'False' ~ FALSE,
      is.na(`IsDiseaseModelControl[2]` ) ~ TRUE,
      TRUE ~ as.logical(NA)  # default case if none of the conditions are met
    )
  )


## Make CohortType column
# Combination interventions are interventions where currently licensed antipsychotic is an intervention

reconciled_cohort_type <- reconciled_cohort_label %>%
  mutate(CohortType = case_when(
    `IsDiseaseModelControl[1]` == FALSE & (Treatment1Type == "Negative control" | is.na(Treatment1Type)) ~ "Negative control",
    `IsDiseaseModelControl[1]` == FALSE & Treatment1Type == "Intervention"  ~ "Simple intervention",
    `IsDiseaseModelControl[1]` == TRUE & (Treatment1Type == "Negative control" | is.na(Treatment1Type)) ~ "Sham",
    `IsDiseaseModelControl[1]` == TRUE & Treatment1Type == "Intervention"  ~ "Sham with intervention",
    `IsDiseaseModelControl[1]` == TRUE ~ "Sham"
  )) %>% 
  relocate(CohortType, .after = `InterventionID[2]`) %>% 
  relocate(c(Treatment1Type), .after = `InterventionLabel[1]`) %>% 
  relocate(`IsDiseaseModelControl[1]`, .after = `ModelID[2]`)

reconciled_cohort_type <- reconciled_cohort_type %>%
  mutate(GroupID = interaction(StudyId, ExperimentID, OutcomeId, TimeInMinute))


## Label how positive controls are being used in positive control v TAAR1 Ag experiments
# If for an observation with the value "Positive control" in the CohortType column, there is an observation 
# with the CohortType "Combination intervention" with the same combination of values in the StudyId, ExperimentID, 
# OutcomeId, TimeInMinute variables, then the value in the CohortType column needs changed to "Control for 
# combination intervention". Similarly, if for an observation with the value "Positive control treated sham" 
# in the CohortType column, there is an observation with the CohortType "Combination intervention" with 
# the same combination of values in the StudyId, ExperimentID, OutcomeId, TimeInMinute variables, then the 
# value in the CohortType column needs changed to "Sham for combination intervention"

## Identify experiments that are just 'Simple' comparison, those which are 'Combination' comparisons and those which have 'TAAR1KO' involved
reconciled_comparison_type <- reconciled_cohort_type %>%
  mutate(ExperimentType = case_when(
     str_detect(CohortType, "Simple intervention") | str_detect(CohortType, "Simple") ~ "Intervention",
  ))

reconciled_cohort_role <- reconciled_comparison_type %>% 
  mutate(RoleOfCohort = case_when(
    str_detect(CohortType, "Sham with intervention") ~ "SI", 
    str_detect(CohortType, "Negative control")  ~ "C",
    str_detect(CohortType, "Simple intervention") ~ "I",
    str_detect(CohortType, "Sham") ~ "S"
  ))


## Wrangle wide so each observations is a single comparison
data <- reconciled_cohort_role
data <- data %>%
  mutate(GroupID = interaction(StudyId, ExperimentID, OutcomeId, TimeInMinute))

#### SyRF correction section ends


##### Extract treatment names and doses ####
data <- data%>%
  mutate(drugname1 = case_when(
    grepl("imipramine", data$`Name of Intervention[1]`) ~ "imipramine",
    grepl("olanzepine", data$`Name of Intervention[1]`) ~ "olanzapine",
    grepl("AMANTADINE", data$`Name of Intervention[1]`) ~ "amantadine",
    grepl("orientalis", data$`Name of Intervention[1]`) ~ "P. orientalis seed",
    grepl("Quinpirole", data$`Name of Intervention[1]`) ~ "quinpirole",
    grepl("QUINPIROLE", data$`Name of Intervention[1]`) ~ "quinpirole",
    grepl("L-DOPA", data$`Name of Intervention[1]`) ~ "L-DOPA",
    grepl("ARIPIPRAZOLE", data$`Name of Intervention[1]`) ~ "aripiprazole",
    grepl("ESCITALOPRAM", data$`Name of Intervention[1]`) ~ "escitalopram",
    grepl("IMIPRAMINE", data$`Name of Intervention[1]`) ~ "imipramine",
    grepl("D-AMPHETAMINE", data$`Name of Intervention[1]`) ~ "D-amphetamine",
    grepl("DOPAMINE", data$`Name of Intervention[1]`) ~ "dopamine",
    grepl("SCH23390", data$`Name of Intervention[1]`) ~ "SCH23390",
    grepl("SKF38393", data$`Name of Intervention[1]`) ~ "SKF38393",
    grepl("SKF-38393", data$`Name of Intervention[1]`) ~ "SKF38393",
    grepl("BUPROPION", data$`Name of Intervention[1]`) ~ "buproprion",
    grepl("risperidone", data$`Name of Intervention[1]`) ~ "risperidone",
    grepl("Risperidone", data$`Name of Intervention[1]`) ~ "risperidone",
    grepl("clozapine", data$`Name of Intervention[1]`) ~ "clozapine",
    grepl("CRYPTOTANSHINONE", data$`Name of Intervention[1]`) ~ "cryptotanshinone",
    grepl("Aripiprazole", data$`Name of Intervention[1]`) ~ "aripiprazole",
    grepl("aripiprazole", data$`Name of Intervention[1]`) ~ "aripiprazole",
    grepl("selegiline", data$`Name of Intervention[1]`) ~ "selegiline",
    grepl("OLZ", data$`Name of Intervention[1]`) ~ "olanzapine",
    grepl("quinpirole", data$`Name of Intervention[1]`) ~ "quinpirole",
    grepl("bromocriptine", data$`Name of Intervention[1]`) ~ "bromocriptine",
    grepl("Pramipexole", data$`Name of Intervention[1]`) ~ "pramipexole",    
    grepl("ROPINIROLE", data$`Name of Intervention[1]`) ~ "ropinirole",    
    grepl("TRANCYLCYPROMINE", data$`Name of Intervention[1]`) ~ "tranylcypromine",
    grepl("APH199", data$`Name of Intervention[1]`) ~ "APH199",
    grepl("METHYLPHENIDATE", data$`Name of Intervention[1]`) ~ "methyphenidate",
    grepl("ESCITALOPRAM", data$`Name of Intervention[1]`) ~ "escitalopram",
    grepl("LEVODOPA", data$`Name of Intervention[1]`) ~ "L-DOPA",
    grepl("BROMOCRIPTINE", data$`Name of Intervention[1]`) ~ "bromocriptine",
    grepl("PRAMIPREXOLE", data$`Name of Intervention[1]`) ~ "pramipexole",
    grepl("Δ3 ,2-hydroxybakuchiol", data$`Name of Intervention[1]`) ~ "2_HBC",
    grepl("SCH23390", data$`Name of Intervention[1]`) ~ "SCH23390",
    grepl("PIRIBEDIL", data$`Name of Intervention[1]`) ~ "piribedil",
    grepl("bromocriptine", data$`Name of Intervention[1]`) ~ "bromocriptine",
    grepl("SKF83959", data$`Name of Intervention[1]`) ~ "SKF83959",
    grepl("SIMVASTATIN", data$`Name of Intervention[1]`) ~ "simvastatin",
    grepl("IMPIRAMINE", data$`Name of Intervention[1]`) ~ "imipramine",
    is.na(data$`Name of Intervention[1]`) ~ NA,
    TRUE ~ "Other"
  ))

data <- data%>%
  mutate(drugname2 = case_when(
    grepl("imipramine", data$`Name of Intervention[2]`) ~ "imipramine",
    grepl("olanzepine", data$`Name of Intervention[2]`) ~ "olanzapine",
    grepl("AMANTADINE", data$`Name of Intervention[2]`) ~ "amantadine",
    grepl("orientalis", data$`Name of Intervention[2]`) ~ "P. orientalis seed",
    grepl("Quinpirole", data$`Name of Intervention[2]`) ~ "quinpirole",
    grepl("QUINPIROLE", data$`Name of Intervention[2]`) ~ "quinpirole",
    grepl("L-DOPA", data$`Name of Intervention[2]`) ~ "L-DOPA",
    grepl("ARIPIPRAZOLE", data$`Name of Intervention[2]`) ~ "aripiprazole",
    grepl("ESCITALOPRAM", data$`Name of Intervention[2]`) ~ "escitalopram",
    grepl("IMIPRAMINE", data$`Name of Intervention[2]`) ~ "imipramine",
    grepl("D-AMPHETAMINE", data$`Name of Intervention[2]`) ~ "D-amphetamine",
    grepl("DOPAMINE", data$`Name of Intervention[2]`) ~ "dopamine",
    grepl("SCH23390", data$`Name of Intervention[2]`) ~ "SCH23390",
    grepl("SKF38393", data$`Name of Intervention[2]`) ~ "SKF38393",
    grepl("SKF-38393", data$`Name of Intervention[2]`) ~ "SKF38393",
    grepl("BUPROPION", data$`Name of Intervention[2]`) ~ "buproprion",
    grepl("risperidone", data$`Name of Intervention[2]`) ~ "risperidone",
    grepl("Risperidone", data$`Name of Intervention[2]`) ~ "risperidone",
    grepl("clozapine", data$`Name of Intervention[2]`) ~ "clozapine",
    grepl("CRYPTOTANSHINONE", data$`Name of Intervention[2]`) ~ "cryptotanshinone",
    grepl("Aripiprazole", data$`Name of Intervention[2]`) ~ "aripiprazole",
    grepl("aripiprazole", data$`Name of Intervention[2]`) ~ "aripiprazole",
    grepl("selegiline", data$`Name of Intervention[2]`) ~ "selegiline",
    grepl("OLZ", data$`Name of Intervention[2]`) ~ "olanzapine",
    grepl("quinpirole", data$`Name of Intervention[2]`) ~ "quinpirole",
    grepl("bromocriptine", data$`Name of Intervention[2]`) ~ "bromocriptine",
    grepl("Pramipexole", data$`Name of Intervention[2]`) ~ "pramipexole",    
    grepl("ROPINIROLE", data$`Name of Intervention[2]`) ~ "ropinirole",    
    grepl("TRANCYLCYPROMINE", data$`Name of Intervention[2]`) ~ "tranylcypromine",
    grepl("APH199", data$`Name of Intervention[2]`) ~ "APH199",
    grepl("METHYLPHENIDATE", data$`Name of Intervention[2]`) ~ "methyphenidate",
    grepl("ESCITALOPRAM", data$`Name of Intervention[2]`) ~ "escitalopram",
    grepl("LEVODOPA", data$`Name of Intervention[2]`) ~ "L-DOPA",
    grepl("BROMOCRIPTINE", data$`Name of Intervention[2]`) ~ "bromocriptine",
    grepl("PRAMIPREXOLE", data$`Name of Intervention[2]`) ~ "pramipexole",
    grepl("Δ3 ,2-hydroxybakuchiol", data$`Name of Intervention[2]`) ~ "2_HBC",
    grepl("SCH23390", data$`Name of Intervention[2]`) ~ "SCH23390",
    grepl("PIRIBEDIL", data$`Name of Intervention[2]`) ~ "piribedil",
    grepl("bromocriptine", data$`Name of Intervention[2]`) ~ "bromocriptine",
    grepl("SKF83959", data$`Name of Intervention[2]`) ~ "SKF83959",
    grepl("SIMVASTATIN", data$`Name of Intervention[2]`) ~ "simvastatin",
    grepl("IMPIRAMINE", data$`Name of Intervention[2]`) ~ "imipramine",
    is.na(data$`Name of Intervention[2]`) ~ NA,
    TRUE ~ "Other"
  ))



columnnmae13 <- "Measurement unit for treatment dose:[1]"
condition <- data$StudyId == 'd51b4559-f349-4c76-a5db-e63698b10f46'
data[condition, columnnmae13] <- "micrograms (ug) per kg"

condition <- data$CohortId == '20c38490-ce3a-40a2-b262-91652c67ea03'
data[condition, columnnmae13] <- "mg/kg"


colname14 <- "Dose of treatment used:[1]"
condition <- data$CohortId == '20c38490-ce3a-40a2-b262-91652c67ea03'
data[condition, colname14] <- '1'

colname15 <- "Animal strain?"
condition <- data$CohortId == 'f4f81624-4f5f-401f-a2b5-62a690fbd6d1'
data[condition, colname14] <- 'NMRI'

colname15 <- "Animal strain?"
condition <- data$CohortId == '944c6614-089b-4f41-bf57-ab08e5afb4c0'
data[condition, colname14] <- 'Swiss albino'

colname15 <- "Animal strain?"
condition <- data$CohortId == 'a47258ef-5b15-443e-b043-07c1f11659ed'
data[condition, colname14] <- 'PVG Hooded'

colname15 <- "Animal strain?"
condition <- data$CohortId == 'cc6534a5-d880-45a2-ab36-544a2e30d3c1'
data[condition, colname14] <- 'WAG/Rij'





data <- data %>% 
  mutate(`Measurement unit for treatment dose:[1]` = case_when(
    str_detect(`Measurement unit for treatment dose:[1]`, "(?i)miligrams\\s?\\(mg\\)\\s?per\\s?kg") ~ "mg/kg",
    str_detect(`Measurement unit for treatment dose:[1]`, "(?i)micrograms\\s?\\(ug\\)\\s?per\\s?kg") ~ "ug/kg",
    str_detect(`Measurement unit for treatment dose:[1]`, "(?i)micrograms\\s?\\(ug\\)") ~ "ug",
    TRUE ~ `Measurement unit for treatment dose:[1]`
  ))



data <- data %>% 
  mutate(`Measurement unit for treatment dose:[2]` = if_else(
    str_detect(`Measurement unit for treatment dose:[2]`, "miligrams \\(mg\\) per kg"),
    "mg/kg",
    if_else(
      str_detect(`Measurement unit for treatment dose:[2]`, "micrograms \\(ug\\) per kg"),
      "ug/kg",
      if_else(
        str_detect(`Measurement unit for treatment dose:[2]`, "micrograms \\(ug\\)"),
        "ug",
        `Measurement unit for treatment dose:[2]`  # If none of the conditions are met, keep the original value
      )
    )
  ))


data$Treatment1Label1 <- paste0(data$drugname1, ", ", data$`Dose of treatment used:[1]`, ' ', data$`Measurement unit for treatment dose:[1]`)
data$Treatment1Label2 <- paste0(data$drugname2, ", ", data$`Dose of treatment used:[2]`, ' ', data$`Measurement unit for treatment dose:[2]`)

data <- data %>% 
  mutate(
    TreatmentLabel = case_when(
      !is.na(drugname1) & !is.na(drugname2) ~ paste(Treatment1Label1, "&", Treatment1Label2),
      !is.na(drugname1) ~ Treatment1Label1,
      !is.na(drugname2) ~ Treatment1Label2,
      TRUE ~ NA_character_
    )
  )

data$DrugLabel <- data$drugname1

###for each cohort, label sham and control groups, along with TAAR1KO -ve control Contol for combination interventions (== positive control)
### first, we need to work out the attributes of each group - wgat types of comparisons will they allow?
# Assuming "df" is your data frame with columns "groupId" and "cohortType"

##### Count occurrences for each cohort type within each group #####
group_characteristics <- data %>%
  group_by(GroupID, CohortType) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = CohortType, values_from = count, values_fill = 0) %>%
  ungroup()
group_characteristics$total <- rowSums(select(group_characteristics, -GroupID))
group_characteristics$GroupID <- as.character(group_characteristics$GroupID)
data$GroupID <- as.character(data$GroupID)



##### Wrangling experiment type and outcome data #####
##### 3.1.1 T v C  with sham ####
## using same approach as for 3.3 and 3.4 to get consistency in final df ##
data_TvCs <- data.frame()

for (i in 1:nrow(group_characteristics)) {
  group <- group_characteristics[i, 1]
  cohset <- data[data$GroupID == as.character(group), ]  
  
  
  # Create combinations of interventions and positive controls
  combinations <- expand.grid(
    Intervention = unique(cohset$CohortLabel [cohset$CohortType == "Simple intervention"]),
    Control = unique(cohset$CohortLabel [cohset$CohortType == "Negative control"]),
    Sham = unique(cohset$CohortLabel [cohset$CohortType == "Sham"])
  )
  
  if (nrow(combinations) > 0) {
    cohset <- combinations %>%
      left_join(cohset %>% filter(CohortType == "Simple intervention"), by = c("Intervention" = "CohortLabel")) %>%
      left_join(cohset %>% filter(CohortType == "Negative control"), by = c("Control" = "CohortLabel")) %>%
      left_join(cohset %>% filter(CohortType == "Sham"), by = c("Sham" = "CohortLabel"))
    
    if (nrow(cohset) > 0) {
      data_TvCs <- bind_rows(data_TvCs, cohset)
    }}}

data_TvCs$Label <- data_TvCs$TreatmentLabel.x
data_TvCs$SortLabel <- "TvC"
data_TvCs <- subset(data_TvCs, !is.na(data_TvCs$Intervention) & !is.na(data_TvCs$Control) & !is.na(data_TvCs$Sham))

##### 3.1.2 T v C  without sham ####

data_TvC <- data.frame()

for (i in 1:nrow(group_characteristics)) {
  group <- group_characteristics[i, 1]
  cohset <- data[data$GroupID == as.character(group), ]  
  
  # Create combinations of interventions and positive controls
  combinations <- expand.grid(
    Intervention = unique(cohset$CohortLabel [cohset$CohortType == "Simple intervention"]),
    Control = unique(cohset$CohortLabel [cohset$CohortType == "Negative control"])
  )
  
  # Merge combinations with the original data
  cohset <- combinations %>%
    left_join(cohset %>% filter(CohortType == "Simple intervention"), by = c("Intervention" = "CohortLabel")) %>%
    left_join(cohset %>% filter(CohortType == "Negative control"), by = c("Control" = "CohortLabel"))
  data_TvC <- bind_rows(data_TvC, cohset)
}
data_TvC$Label <- data_TvC$TreatmentLabel.x
data_TvC$SortLabel <- "TvC"
data_TvC <- subset(data_TvC, !is.na(data_TvC$Intervention) & !is.na(data_TvC$Control))

data_sub_TvC <- subset(data_TvC, !data_TvC$GroupID.x %in% data_TvCs$GroupID.x)
data_TvCc <- bind_rows(data_TvCs, data_sub_TvC)


##### 3.1.2 C v S  : effect of model ####

data_CvS <- data.frame()

for (i in 1:nrow(group_characteristics)) {
  group <- group_characteristics[i, 1]
  cohset <- data[data$GroupID == as.character(group), ]  
  
  # Create combinations of interventions and positive controls
  combinations <- expand.grid(
    Control = unique(cohset$CohortLabel [cohset$CohortType == "Sham"]),
    Intervention = unique(cohset$CohortLabel [cohset$CohortType == "Negative control"])
  )
  
  # Merge combinations with the original data
  cohset <- combinations %>%
    left_join(cohset %>% filter(CohortType == "Negative control"), by = c("Intervention" = "CohortLabel")) %>%
    left_join(cohset %>% filter(CohortType == "Sham"), by = c("Control" = "CohortLabel"))
  data_CvS <- bind_rows(data_CvS, cohset)
}
data_CvS$Label <- data_CvS$TreatmentLabel.x
data_CvS$SortLabel <- "CvS"
data_CvS <- subset(data_CvS, !is.na(data_CvS$Intervention) & !is.na(data_CvS$Control))


##### combine to a single df and merge back to main #####

data2 <- bind_rows(data_TvCc, data_CvS)


data2 <- data2 %>%
  rename_at(vars(ends_with(".x")), ~str_remove(., "\\.x") %>% paste0("_I")) %>%
  rename_at(vars(ends_with(".y")), ~str_remove(., "\\.y") %>% paste0("_C"))


#####colname corrections #####
data2$`Type of outcome` <- data2$`Type of outcome:_I`
data2$StudyId <- data2$StudyId_I
data2$ErrorType <- data2$ErrorType_I
data2$GreaterIsWorse <- data2$GreaterIsWorse_I
data2$`Measurement unit of treatment dose:` <- data2$`Measurement unit for treatment dose:_I`
data2$`Duration of treatment` <- data2$`Duration of treatment:[1]_I`
data2$`Unit of measurement for treatment duration` <- data2$`Unit of measurement for treatment duration:[1]_I`
data2$`Species of animals?` <- data2$`Species of animals?_I`
data2$`Animal strain?` <- data2$`Animal strain?_I`
data2$`Sex of animals?` <- data2$`Sex of animals?_I`
data2$`Were caregivers/investigator blinded to which intervention each animal received?` <- data2$`Were caregivers/investigator blinded to which intervention each animal received?_I`
data2$`Is any role of the funder in the design/analysis/reporting of study described?` <- data2$`Is any role of the funder in the design/analysis/reporting of study described?_I`
data2$Title <- data2$Title_I
data2$Year <- data2$Year_I
data2$ModelID <- data2$`ModelID[1]_I`
data2$Authors <- data2$Authors_I

#GroupIdlist <- unique(data2$GroupID.x) ##?

##### get standard placement - Single I #####
data2$F_C_L <- data2$Control
data2$F_C_n <- as.numeric(data2$NumberOfAnimals_C)
data2$F_C_m <- as.numeric(data2$OutcomeResult_C)
data2$F_C_v <- as.numeric(data2$OutcomeError_C)

data2$F_T_L <- data2$Intervention
data2$F_T_n <- as.numeric(data2$NumberOfAnimals_I)
data2$F_T_m <- as.numeric(data2$OutcomeResult_I)
data2$F_T_v <- as.numeric(data2$OutcomeError_I)

data2$F_S_L <- data2$Sham
data2$F_S_n <- as.numeric(data2$NumberOfAnimals)
data2$F_S_m <- as.numeric(data2$OutcomeResult)
data2$F_S_v <- as.numeric(data2$OutcomeError)

data_all_F <- data2


savename_all <- paste0(LSR,'data_all_',Sys.Date(),'.csv')

write_csv(data_all_F, savename_all)

###### Calculate effect size for simple interventions #####

# Data are ready to calculate effect sizes with numerical data for one comparison (test/control) per line

# Note: current code doesn't have cohort level questions split into treatment and control annotations per line
# Not an issue currently since control and treatment cohorts have had the same characteristics (sex, strain, etc) so far, but probably the subject of future edits (e.g. using pivot_wider function)



## Read in data and edit outcome label 
#pass though better
# dataall <- read_csv("dataall.csv")
dataall <- data_all_F



dataall <- dataall %>% 
  mutate(OutcomeType = case_when(str_detect(OutcomeLabel_I, regex("DA/DOPC", ignore_case = TRUE)) ~ "DA/DOPC ratio",
                                 str_detect(OutcomeLabel_I, regex("DOPAC", ignore_case = TRUE)) ~ "DOPAC concentration",
                                 str_detect(OutcomeLabel_I, regex("sucrose", ignore_case = TRUE)) ~ "Sucrose preference test",
                                 str_detect(OutcomeLabel_I, regex("DA")) ~ "Dopamine concentration", 
                                 str_detect(OutcomeLabel_I, regex("Dopamine", ignore_case = TRUE)) ~ "Dopamine concentration",
                                 str_detect(OutcomeLabel_I, regex("DRD2", ignore_case = TRUE)) ~ "DRD2 mRNA expression",
                                 TRUE ~ `Type of outcome`)) %>% 
  relocate(OutcomeType, .after = OutcomeLabel)

outcomeFrequencies <- dataall %>% group_by(OutcomeType, OutcomeLabel_I, `Type of outcome`) %>% count()

savename_of <- paste0(LSR,'_OutcomeFrequencies',Sys.Date(),'.csv')

write.csv(outcomeFrequencies, savename_of)
## 1. Calculate SD for all comparisons 
#F = final
#C/T/S = control/treatment/sham
#L/n/m/v = cohort label/n in cohort/mean/variance

dataall <- dataall %>% 
  mutate(F_C_v.SD = case_when(ErrorType == "IQR" ~ (F_C_v/1.35), 
                              ErrorType == "SD" ~ F_C_v, 
                              ErrorType == "SEM" ~ sqrt(F_C_n)*F_C_v)) %>%  
  relocate(F_C_v.SD, .after = F_C_v) %>% 
  mutate(F_T_v.SD = case_when(ErrorType == "IQR" ~ (F_T_v/1.35), 
                              ErrorType == "SD" ~ F_T_v, 
                              ErrorType == "SEM" ~ sqrt(F_T_n)*F_T_v)) %>%  
  relocate(F_T_v.SD, .after = F_T_v) %>% 
  mutate(F_S_v.SD = case_when(ErrorType == "IQR" ~ (F_S_v/1.35), 
                              ErrorType == "SD" ~ F_S_v, 
                              ErrorType == "SEM" ~ sqrt(F_S_n)*F_S_v)) %>%  
  relocate(F_S_v.SD, .after = F_S_v)

# Check number of comparisons where NMD can be calculated

dataall <- dataall %>%
  rowwise() %>%
  mutate(`NMD_possible` = all(!is.na(F_S_L)))

diag <- subset(dataall, (dataall$F_C_v > 0 & dataall$F_T_v > 0))
dataall <- diag

## 2. Calculate effect sizes (SMD for all, NMD where possible)

### Calculate true n for control groups (n'c)

# Step 1: Number of groups served by control group
#F_C_L_frequencies <- dataall %>%
#  group_by(StudyId, OutcomeId, F_C_L) %>%
#  summarise(Frequency_FCL = n()) %>% 
#  ungroup()

# Step 2: Join the frequencies back to the original dataframe
#dataall <- dataall %>%
#  left_join(F_C_L_frequencies)

# Step 3: Divide F_C_n by the frequency count
#dataall <- dataall %>%
#  mutate(F_C_n_true = F_C_n / Frequency_FCL)

### SMD

#Hedges g to account for small sample sizes (default for SMD when using the escalc() function) - Hedge’s g (statistically corrects for variance that may be introduced when sample sizes are small (Larry V. Hedges 1981))
# m1i = control, m2i = rx 

# Hedge's g effect size
SMD_data_all.nottrue <- escalc(
  measure = "SMD", 
  m1i = dataall$F_C_m, 
  m2i = dataall$F_T_m, 
  sd1i = dataall$F_C_v.SD, 
  sd2i = dataall$F_T_v.SD, 
  n1i = dataall$F_C_n, 
  n2i = dataall$F_T_n, 
  data = dataall) %>% 
  select(yi, vi)

dataall$SMD <- SMD_data_all.nottrue$yi
dataall$SMDv <- SMD_data_all.nottrue$vi

#escalc (m1 - m2) = (control - treatment)
# so if greater is better, then *-1

#dataall$SMD_true <- SMD_data_all.true$yi #Row 112, SMD calculated, but SMD_true not calculated
#dataall$SMDv_true <- SMD_data_all.true$vi

### NMD

# Assume that treatments are closer to shams than controls are 
# So C-S > T-S 


dataall <- dataall %>% 
  mutate(`NMD` = 100*(((F_C_m - F_S_m) - (F_T_m - F_S_m))/(F_C_m - F_S_m))) %>% 
  mutate(`NMD_SDc*` = 100*((F_C_v.SD/(F_C_m - F_S_m)))) %>% 
  mutate(`NMD_SDrx*` = 100*((F_T_v.SD/(F_C_m - F_S_m)))) %>% 
  mutate(NMDv = sqrt(((`NMD_SDc*`)^2/F_C_n) + ((`NMD_SDrx*`)^2/F_T_n))) 

dataall.direction <- dataall %>% 
  mutate(SMD = if_else((GreaterIsWorse == "FALSE"), -1*SMD, SMD)) 

diagnostic <- dataall.direction %>% 
  select(F_C_m, F_T_m, F_S_m, SMD, NMD, GreaterIsWorse) %>% 
  mutate(CbiggerT = if_else(F_C_m > F_T_m, "Yes", "No"))

# nicely named columns for subgroup analysis

df <- dataall.direction


# Replace unit of measurements for drugs with abbreviations 
df <- df %>% 
  mutate(`Measurement unit of treatment dose1:` = if_else(
    str_detect(`Measurement unit for treatment dose:[1]_I`, "miligrams \\(mg\\) per kg"),
    "mg/kg",
    if_else(
      str_detect(`Measurement unit for treatment dose:[1]_I`, "micrograms \\(ug\\) per kg"),
      "ug/kg",
      if_else(
        str_detect(`Measurement unit for treatment dose:[1]_I`, "micrograms \\(ug\\)"),
        "ug",
        `Measurement unit for treatment dose:[1]_I`  # If none of the conditions are met, keep the original value
      )
    )
  ))

df <- df %>% 
  mutate(`Measurement unit of treatment dose2:` = if_else(
    str_detect(`Measurement unit for treatment dose:[2]_I`, "miligrams \\(mg\\) per kg"),
    "mg/kg",
    if_else(
      str_detect(`Measurement unit for treatment dose:[2]_I`, "micrograms \\(ug\\) per kg"),
      "ug/kg",
      if_else(
        str_detect(`Measurement unit for treatment dose:[2]_I`, "micrograms \\(ug\\)"),
        "ug",
        `Measurement unit for treatment dose:[2]_I`  # If none of the conditions are met, keep the original value
      )
    )
  ))


### Duration of treatment (categorical)

# Create variable standardised to Weeks
df <- df %>% 
  mutate(`Duration of treatment` = as.numeric(`Duration of treatment`)) %>% 
  mutate(DurationOfTreatmentWeeks = case_when(`Unit of measurement for treatment duration` == "Days" ~ `Duration of treatment`/7, 
                                              `Unit of measurement for treatment duration` == "Months" ~ `Duration of treatment`/4.345, 
                                              `Unit of measurement for treatment duration` == "Weeks" ~ `Duration of treatment`, 
                                              `Unit of measurement for treatment duration` == "Single dose" ~ 1/7))


# Create categorical variable for Duration of treatment. Grouped into up to a week, between a week and 4 weeks, more than 4 weeks
df <- df %>% 
  mutate(TreatmentDurationCategory = case_when(DurationOfTreatmentWeeks <= 1 ~ "Less than 1 week", 
                                               DurationOfTreatmentWeeks > 1 & DurationOfTreatmentWeeks < 4 ~ "Between 1-4 weeks", 
                                               DurationOfTreatmentWeeks >= 4 ~ "More than 4 weeks"))




### Prophylactic or therapeutic



df <- df %>% 
  mutate(ProphylacticOrTherapeutic = case_when(`Timing of treatment administration:[1]_I` == "After disease model induction" ~ "Therapeutic", 
                                               TRUE ~ "Prophylactic"))

## Give original variables better names for analysis in new columns - this is all for simple 

df <- df %>% 
  mutate(Species = `Species of animals?`, 
         Strain = `Animal strain?`,
         Sex = `Sex of animals?`, 
         DrugName = drugname1_I,  #change for combination
         InterventionAdministrationRoute = `Treatment administration route:[1]_I`, #change for combination
         DoseOfIntervention_mgkg = `Dose of treatment used:[1]_I`)  

## Categorise by outcome type - requires checking with each iteration
df <-  df %>%
  mutate(outcome_type = case_when(
    OutcomeType == "Sucrose preference test" ~ "Sucrose preference test",
    OutcomeType == "DOPAC concentration" ~ "DOPAC concentration",
    OutcomeType == "Dopamine concentration" ~ "Dopamine concentration",
    OutcomeType == "DA/DOPC ratio" ~ "DA/DOPC ratio",
    TRUE ~ "Other"
  )) 

## Add drug characteristics - Info taken from Taar1_drugs_spiros_18.12.23.xlsx
#df <- df %>% 
#  mutate(pE50 = case_when(DrugName == "LK000764" ~ 8.40, 
#                          DrugName == "RO5073012" ~ 7.64, 
#                          DrugName == "RO5166017" ~ 7.23,
#                          DrugName == "RO5203648" ~ 7.52,
#                          DrugName == "RO5263397" ~ 7.77,
#                          DrugName == "RO5256390" ~ 7.74,
#                          DrugName == "Ulotaront" | DrugName == "SEP-363856" ~ 6.85,
#                          DrugName == "Ralmitaront" | DrugName == "RO6889450" ~ 7.23,
#                          DrugName == "AP163" ~ 6.95,
#                          DrugName == "Compound 50A" ~ 5.20,
#                          DrugName == "Compound 50B" ~ 6.39,
#                          TRUE ~ NA_real_)) %>% 
#  mutate(Efficacy = case_when(DrugName == "LK000764" ~ "TAAR1 full agonist", 
#                              DrugName == "RO5073012" ~ "TAAR1 partial agonist", 
 #                             DrugName == "RO5166017" ~ "TAAR1 full agonist",
#                              DrugName == "RO5203648" ~ "TAAR1 partial agonist",
#                              DrugName == "RO5263397" ~ "TAAR1 partial agonist",
#                              DrugName == "RO5256390" ~ "TAAR1 full agonist",
#                              DrugName == "Ulotaront" | DrugName == "SEP-363856" ~ "TAAR1 full agonist",
#                              DrugName == "Ralmitaront" | DrugName == "RO6889450"~ "TAAR1 partial agonist",
#                              DrugName == "AP163" ~ "TAAR1 full agonist",
#                              DrugName == "Compound 50A" ~ "TAAR1 partial agonist",
#                              DrugName == "Compound 50B" ~ "TAAR1 partial agonist", 
#                              TRUE ~ "NA")) %>% 
#  mutate(Selectivity = case_when(DrugName == "LK000764" ~ "Unclear", 
#                                 DrugName == "RO5073012" ~ "High", 
#                                 DrugName == "RO5166017" ~ "High",
#                                 DrugName == "RO5203648" ~ "High",
#                                 DrugName == "RO5263397" ~ "High",
#                                 DrugName == "RO5256390" ~ "High",
#                                 DrugName == "Ulotaront" | DrugName == "SEP-363856" ~ "Low (5HT1A partial agonism)",
#                                 DrugName == "Ralmitaront" | DrugName == "RO6889450" ~ "Unclear",
#                                 DrugName == "AP163" ~ "Unclear",
#                                 DrugName == "Compound 50A" ~ "Unclear",
#                                 DrugName == "Compound 50B" ~ "Low (5HT1A partial agonism)", 
#                                 TRUE ~ "NA")) %>% 
#  mutate(MolarMass = case_when(DrugName == "LK000764" ~ 299.1058, 
#                               DrugName == "RO5073012" ~ 249.742, 
#                               DrugName == "RO5166017" ~ 219.288,
#                               DrugName == "RO5203648" ~ 231.08,
#                               DrugName == "RO5256390" ~ 218.29,
#                               DrugName == "RO5263397" ~ 194.21,
#                               DrugName == "Ulotaront" | DrugName == "SEP-363856" ~ 183.27,
#                               DrugName == "Ralmitaront" | DrugName == "RO6889450" ~ 314.38,
#                               DrugName == "AP163" ~ 282.1368,
#                               DrugName == "Compound 50A" ~ 170.3,
#                               DrugName == "Compound 50B" ~ 170.3, 
#                               TRUE ~ NA_real_)) %>% 
#  mutate(EC50mM = case_when(DrugName == "LK000764" ~ 0.004, 
#                            DrugName == "RO5073012" ~ 0.023, 
#                            DrugName == "RO5166017" ~ 0.059,
#                            DrugName == "RO5203648" ~ 0.03,
#                            DrugName == "RO5256390" ~ 0.018,
#                            DrugName == "RO5263397" ~ 0.017,
#                            DrugName == "Ulotaront" | DrugName == "SEP-363856" ~ 0.14,
#                            DrugName == "Ralmitaront" | DrugName == "RO6889450" ~ 0.059,
#                            DrugName == "AP163" ~ 0.112,
#                            DrugName == "Compound 50A" ~ 6.25,
#                            DrugName == "Compound 50B" ~ 0.405, 
#                            TRUE ~ NA_real_)) 

# Calculate standardised dose for overall dose-response meta-regression
#df <- df %>% 
#  mutate(DoseOfIntervention_mgkg = as.numeric(DoseOfIntervention_mgkg)) %>% 
#  mutate(StandardisedDose = log((DoseOfIntervention_mgkg/1000)/((MolarMass)*(EC50mM/1000000)))) 


###### For RoB subgroup analysis ######
df <- df %>%
  rename(`(RoB) Were caregivers/investigator blinded to which intervention each animal received?` = `Were caregivers/investigator blinded to which intervention each animal received?`)

# Calculate overall RoB score

df <- df %>%
  rowwise() %>%
  mutate(RoSBScoreAny = sum(c_across(contains("RoB")) == "Yes", na.rm = TRUE)) %>%
  ungroup()

df <- df %>%
  rowwise() %>%
  mutate(RoBScore = sum(c_across(contains("RoB")) == "Yes", na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(RoBScore = paste0(RoBScore, " criteria met"))


df$RoSBScoreAny <- as.numeric(df$RoSBScoreAny)

df$RoBTF <- 'No low RoB items'
for(i in 1:nrow(df)) {
  if(df[i, "RoSBScoreAny"] > 0) {
    df[i,'RoBTF'] <- 'Some low RoB items'
  }
}
##### For reporting quality subgroup analysis #####

df <- df %>% 
  rename(`(ARRIVE) Is any role of the funder in the design/analysis/reporting of the study described?` = `Is any role of the funder in the design/analysis/reporting of study described?`)

# Calculate overall ARRIVE score
df <- df %>%
  mutate(ARRIVEScore = rowSums(across(contains("ARRIVE"), ~ (.x == "Yes") | (.x == "NA (ethical approval declared)")), na.rm = TRUE)) %>% 
  mutate(ARRIVEScoreCat = case_when(ARRIVEScore <= 3 ~ "A: < 3 criteria met",
                                    ARRIVEScore > 3 & ARRIVEScore <= 7 ~ "B: 4-7 criteria met",
                                    ARRIVEScore > 7 & ARRIVEScore <= 11 ~ "C: 8-11 criteria met",
                                    ARRIVEScore > 11 & ARRIVEScore <= 15 ~ "D: 12-15 criteria met",
                                    ARRIVEScore > 15 & ARRIVEScore <= 19 ~ "E: 16-19 criteria met",
                                    ARRIVEScore > 19 ~ "F: > 20 criteria met")) 



df$CatDisInd <- df$`Type of depression/anhedonia model[1]_I`
   
# remove studies with 999 as N or SD
df4 <- subset(df, !df$NumberOfAnimals_I == 999)
df4 <- subset(df4, !df4$NumberOfAnimals_C == 999)
df4 <- subset(df4, !df4$OutcomeError_C == 999)
df <- subset(df4, !df4$OutcomeError_I == 999)

df <- df %>% 
  mutate_all(~replace(., . == 999, NA))




# SAVE FILE
savefile_output <- paste0(LSR,'_','clean_data_',Sys.Date(),'.csv')
write.csv(df, savefile_output, row.names = FALSE)
