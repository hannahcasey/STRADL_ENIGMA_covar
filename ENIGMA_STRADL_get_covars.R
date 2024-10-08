library(dplyr)
library(readxl)
library(data.table)
library(readr)

## Main covariates ----
### SubjID ----
demographics <- read.csv("/Volumes/STRADL/Collated/STRADL-Measures-Phenotypic/STRADL_Demographics.csv")
psych_meds <- read.csv("/Volumes/STRADL/Collated/STRADL-Measures-Phenotypic/STRADL_Meds_Psych.csv")
SCID <- read.csv("/Volumes/GenScotDepression/data/genscot/phenotypes/SCID_QC_201113_GWASids.csv")
bridge <- read.csv("/Volumes/STRADL/Collated/STRADL-GenScot-Linkage/STRADL_GenScot_ID_Linkage_July_2019.csv")

SCID_joined <- SCID %>%
  right_join(bridge, by = c("gwas" = "GenScot_ID")) %>%
  rename(ID = STRADL_ID)

covars <- demographics %>% ## SubjID, Age, Sex, site (from ID)
  left_join(psych_meds, by = "ID") %>% ## AD
  left_join(SCID_joined, by = "ID") %>% ## Dx, Recur, Epi, AO
  rename(SubjID = ID) 

### Dx ----
## SCID at baseline GS (not imaging assessment)
## SCID_Diagnosis: (0 - No major disorder, 1 - Single MDD, 2 - Recurrent MDD, 3 - Bipolar Disorder, NA - missing)
covars$Dx <- NA
covars$Dx[covars$SCID_Diagnosis == 0 | covars$SCID_Diagnosis == 3] <- 0
covars$Dx[covars$SCID_Diagnosis == 1 | covars$SCID_Diagnosis == 2] <- 1

### Age, Sex, Site ----
covars$Age <- floor(covars$Age)
covars$Sex <- ifelse(covars$Sex == "M", 1,2)
covars$Site <- ifelse(grepl("DU", covars$SubjID), "DU", "AB")

### Recur ----
covars$Recur <- NA
covars$Recur[covars$Dx == 0] <- 0
covars$Recur[covars$SCID_Diagnosis == 1] <- 1
covars$Recur[covars$SCID_Diagnosis == 2] <- 2

### AD ----
covars$AD <- NA
covars$AD[covars$Meds_Antidepressant == 1 & covars$Dx == 1] <- 2 ## Antidepressant users
covars$AD[covars$Meds_Antidepressant == 0 & covars$Dx == 1] <- 1 ## Antidepressant free MDD patients
covars$AD[covars$Meds_Antidepressant == 0 & covars$Dx == 0] <- 0 ## HC

### AO ----
covars$AO <- covars$SCID_AgeOnset

### Sev ----
## DSM-IV MDD criteria: A1-3, A6, A9, A12, A13, A16, A19
## ?=inadequate information, 1=absent or false, 2=subthreshold, 3=threshold or true

covars <- covars %>%
  mutate_at(vars(A1, A2, A3, A6, A9, A12, A13, A16, A19), 
            ~recode(.,`1` = 0,
                        `2` = 0,
                        `3` = 1))

covars$Sev <- rowSums(covars[c("A1", "A2", "A3", "A6", "A9", "A12", "A13", "A16", "A19")])

### Epi ----
## SCID_episodes = 99: too numerous to count
covars$Epi <- covars$SCID_episodes

### Rem, BDI, HDRS, ADcur ----
covars$Rem <- NA
covars$BDI <- NA
covars$HDRS <- NA
covars$ADcur <- NA

covars <- covars %>% 
  select(SubjID, Dx, Age, Sex, Site, Recur, AD, Rem, AO, Sev, BDI, HDRS, Epi, ADcur)

## Additional Covariates ----

### IQ, IQ_Method ----
covars$IQ <- NA
covars$IQ_method <- NA

### Education_Years, Education_Category, Education_Method ----
education <- read.csv("/Volumes/GenScotDepression/data/genscot/phenotypes/EducationV2V5merged.csv")

education_joined <- education %>%
  right_join(bridge, by = c("id" = "GenScot_ID")) %>%
  rename(SubjID = STRADL_ID)

covars <- left_join(covars, education_joined, by = "SubjID")

covars$Education_Years <- NA
covars$Education_Category <- covars$years_category
covars$Education_Method <- "(Category: Years, 0: 0, 1: 1-4,2: 5-9, 3: 10-11, 4: 12-13, 5: 14-15, 6: 16-17, 7: 18-19, 8: 20-21, 9: 22-23, 10: 24+)"

### Race_Ethnicity ----
#fhx_area <- read.csv("/Volumes/GenScotDepression/data/genscot/phenotypes/fhx_area.csv")
# No coding reference
covars$Race_Ethnicity <- NA

### SubjectSES, SubjectSES_Scale, ParentSES, ParentSES_Scale ----
covars$SubjectSES <- NA
covars$SubjectSES_Scale <- NA
covars$ParentSES <- NA
covars$ParentSES_Scale <- NA

### Antidepressant_Use_Lifetime, Lithium_Use_Lifetime, Typical_Antypsychotic_Use_Lifetime, Atypical_Antypsychotic_Use_Lifetime, Antiepileptic_Use_Lifetime, OtherMed_Use_Current ----
covars$Antidepressant_Use_Lifetime <- NA
covars$Lithium_Use_Lifetime <- NA
covars$Typical_Antypsychotic_Use_Lifetime <- NA
covars$Atypical_Antypsychotic_Use_Lifetime <- NA
covars$Antiepileptic_Use_Lifetime <- NA
covars$OtherMed_Use_Lifetime <- NA

### Lithium_Use_Current, Typical_Antipsychotic_Use_Current, Atypical_Antipsychotic_Use_Current, Antiepileptic_Use_Current, OtherMed_Use_Current ----
#unique(unlist(strsplit(psych_meds$Meds_Psych_List[psych_meds$Meds_Antipsychotic == 1],",")))

lithium <- c("Priadel")
typical_antipsychotics <- c("Flupentixol", "Prochlorperazine")
atypical_antipsychotics <- c("Olanzapine", "Quetiapine", "Clozapine")

covars <- left_join(covars, psych_meds, by = c("SubjID" = "ID"))

covars$Lithium_Use_Current <- ifelse(grepl(lithium, covars$Meds_Psych_List), 1,0)
covars$Lithium_Use_Current[is.na(covars$Meds_Psych_List)] <- NA
covars$Typical_Antipsychotic_Use_Current <- ifelse(grepl(paste(typical_antipsychotics,collapse="|"), covars$Meds_Psych_List), 1,0)
covars$Typical_Antipsychotic_Use_Current[is.na(covars$Meds_Psych_List)] <- NA
covars$Atypical_Antipsychotic_Use_Current <- ifelse(grepl(paste(atypical_antipsychotics,collapse="|"), covars$Meds_Psych_List), 1,0)
covars$Atypical_Antipsychotic_Use_Current[is.na(covars$Meds_Psych_List)] <- NA
covars$Antiepileptic_Use_Current <- covars$Meds_Anticonvulsant
covars$OtherMed_Use_Current <- ifelse((covars$Meds_Psych_List != "" & covars$Meds_Antidepressant == 0 & covars$Meds_Antipsychotic == 0 & covars$Meds_Anticonvulsant == 0), 1,0)

### Generalized_Anxiety_Disorder_Current ----
HADS <- read.csv("/Volumes/STRADL/Collated/STRADL-Measures-Phenotypic/STRADL_Scores_HADS.csv")
covars <- left_join(covars, HADS, by = c("SubjID" = "ID"))
covars$Generalized_Anxiety_Disorder_Current <- ifelse(covars$HADS.A > 7, 1,0)

### Generalized_Anxiety_Disorder_Lifetime ----
covars$Generalized_Anxiety_Disorder_Lifetime <- NA

### Panic_Disorder_Current, Panic_Disorder_Lifetime, Social_Anxiety_Disorder_Current, Social_Anxiety_Disorder_Lifetime, OCD_Current, OCD_Lifetime, PTSD_Current, PTSD_Lifetime, Depression_With_Psychotic_Features_Current, Depression_With_Psychotic_Features_Lifetime, Other_Dx_Current, Other_Dx_Lifetime, Alcohol_Use_Disorder_Current, Alcohol_Use_Disorder_Lifetime, # Smoking_Status_Current, Smoking_Status_Lifetime, Other_Substance_Use_Disorder_Current, Other_Substance_Use_Disorder_Lifetime ----

covars$Panic_Disorder_Current  <- NA
covars$Panic_Disorder_Lifetime <- NA
covars$Social_Anxiety_Disorder_Current <- NA
covars$Social_Anxiety_Disorder_Lifetime <- NA
covars$OCD_Current <- NA
covars$OCD_Lifetime <- NA
covars$PTSD_Current <- NA
covars$PTSD_Lifetime <- NA
covars$Depression_With_Psychotic_Features_Current <- NA
covars$Depression_With_Psychotic_Features_Lifetime <- NA
covars$Other_Dx_Current <- NA
covars$Other_Dx_Lifetime <- NA
covars$Alcohol_Use_Disorder_Current <- NA
covars$Alcohol_Use_Disorder_Lifetime <- NA
covars$Smoking_Status_Current <- NA
covars$Smoking_Status_Lifetime <- NA
covars$Other_Substance_Use_Disorder_Current <- NA
covars$Other_Substance_Use_Disorder_Lifetime <- NA

### BMI ----
BMI <- read.csv("/Volumes/STRADL/Collated/STRADL-Measures-Biological/STRADL_Biological_BMI.csv")
covars <- left_join(covars, BMI, by = c("SubjID" = "ID"))

# Save covariates ----
covars <- covars %>% 
  select(SubjID, Dx, Age, Sex, Site, Recur, AD, Rem, AO, Sev, BDI, HDRS, Epi, ADcur,
         IQ, IQ_method, Education_Years, Education_Category, Education_Method, Race_Ethnicity, SubjectSES,
         SubjectSES_Scale, ParentSES, ParentSES_Scale, Antidepressant_Use_Lifetime, Lithium_Use_Current, Lithium_Use_Lifetime,
         Typical_Antipsychotic_Use_Current, Typical_Antypsychotic_Use_Lifetime, Atypical_Antipsychotic_Use_Current,
         Atypical_Antypsychotic_Use_Lifetime, Antiepileptic_Use_Current, Antiepileptic_Use_Lifetime, OtherMed_Use_Current,
         OtherMed_Use_Lifetime, Generalized_Anxiety_Disorder_Current, Generalized_Anxiety_Disorder_Lifetime, Panic_Disorder_Current,
         Panic_Disorder_Lifetime, Social_Anxiety_Disorder_Current, Social_Anxiety_Disorder_Lifetime, OCD_Current, OCD_Lifetime, PTSD_Current,
         PTSD_Lifetime, Depression_With_Psychotic_Features_Current, Depression_With_Psychotic_Features_Lifetime, Other_Dx_Current,
         Other_Dx_Lifetime, Alcohol_Use_Disorder_Current, Alcohol_Use_Disorder_Lifetime, Smoking_Status_Current, Smoking_Status_Lifetime,
         Other_Substance_Use_Disorder_Current, Other_Substance_Use_Disorder_Lifetime, BMI)



write.csv(covars, "/Volumes/STRADL/Processing/ENIGMA_covars/STRADL_covars.csv")

# Individual Symptom ----
### SubjID ----
individual_symptoms <- demographics %>% ## SubjID, Age, Sex, site (from ID)
  left_join(psych_meds, by = "ID") %>% ## AD
  left_join(SCID_joined, by = "ID") %>% ## Dx, Recur, Epi, AO
  rename(SubjID = ID) 

### BDI_Total, BDI1-21 ----
individual_symptoms$BDI_Total <- NA
individual_symptoms$BDI1 <- NA
individual_symptoms$BDI2 <- NA
individual_symptoms$BDI3 <- NA
individual_symptoms$BDI4 <- NA
individual_symptoms$BDI5 <- NA
individual_symptoms$BDI6 <- NA
individual_symptoms$BDI7 <- NA
individual_symptoms$BDI8 <- NA
individual_symptoms$BDI9 <- NA
individual_symptoms$BDI10 <- NA
individual_symptoms$BDI11 <- NA
individual_symptoms$BDI12 <- NA
individual_symptoms$BDI13 <- NA
individual_symptoms$BDI14 <- NA
individual_symptoms$BDI15 <- NA
individual_symptoms$BDI16 <- NA
individual_symptoms$BDI17 <- NA
individual_symptoms$BDI18 <- NA
individual_symptoms$BDI19 <- NA
individual_symptoms$BDI20 <- NA
individual_symptoms$BDI21 <- NA


### HDRS_Total, HDRS17 ----
individual_symptoms$HDRS_Total <- NA
individual_symptoms$HDRS1 <- NA
individual_symptoms$HDRS2 <- NA
individual_symptoms$HDRS3 <- NA
individual_symptoms$HDRS4 <- NA
individual_symptoms$HDRS5 <- NA
individual_symptoms$HDRS6 <- NA
individual_symptoms$HDRS7 <- NA
individual_symptoms$HDRS8 <- NA
individual_symptoms$HDRS9 <- NA
individual_symptoms$HDRS10 <- NA
individual_symptoms$HDRS11 <- NA
individual_symptoms$HDRS12 <- NA
individual_symptoms$HDRS13 <- NA
individual_symptoms$HDRS14 <- NA
individual_symptoms$HDRS15 <- NA
individual_symptoms$HDRS16 <- NA
individual_symptoms$HDRS17 <- NA

### IDS_Total, IDS1-30 ----
individual_symptoms$IDS_Total <- NA
individual_symptoms$IDS1 <- NA
individual_symptoms$IDS2 <- NA
individual_symptoms$IDS3 <- NA
individual_symptoms$IDS4 <- NA
individual_symptoms$IDS5 <- NA
individual_symptoms$IDS6 <- NA
individual_symptoms$IDS7 <- NA
individual_symptoms$IDS8 <- NA
individual_symptoms$IDS9 <- NA
individual_symptoms$IDS10 <- NA
individual_symptoms$IDS11 <- NA
individual_symptoms$IDS12 <- NA
individual_symptoms$IDS13 <- NA
individual_symptoms$IDS14 <- NA
individual_symptoms$IDS15 <- NA
individual_symptoms$IDS16 <- NA
individual_symptoms$IDS17 <- NA
individual_symptoms$IDS18 <- NA
individual_symptoms$IDS19 <- NA
individual_symptoms$IDS20 <- NA
individual_symptoms$IDS21 <- NA
individual_symptoms$IDS22 <- NA
individual_symptoms$IDS23 <- NA
individual_symptoms$IDS24 <- NA
individual_symptoms$IDS25 <- NA
individual_symptoms$IDS26 <- NA
individual_symptoms$IDS27 <- NA
individual_symptoms$IDS28 <- NA
individual_symptoms$IDS29 <- NA
individual_symptoms$IDS30 <- NA


### QIDS_Total, QIDS16 ----
QIDS <- read.csv("/Volumes/STRADL/Collated/STRADL-Measures-Phenotypic/STRADL_Scores_QIDS.csv")
individual_symptoms <- left_join(individual_symptoms, QIDS, by = c("SubjID" = "ID"))

individual_symptoms$QIDS_Total <- individual_symptoms$QIDS
individual_symptoms$QIDS1 <- NA
individual_symptoms$QIDS2 <- NA
individual_symptoms$QIDS3 <- NA
individual_symptoms$QIDS4 <- NA
individual_symptoms$QIDS5 <- NA
individual_symptoms$QIDS6 <- NA
individual_symptoms$QIDS7 <- NA
individual_symptoms$QIDS8 <- NA
individual_symptoms$QIDS9 <- NA
individual_symptoms$QIDS10 <- NA
individual_symptoms$QIDS11 <- NA
individual_symptoms$QIDS12 <- NA
individual_symptoms$QIDS13 <- NA
individual_symptoms$QIDS14 <- NA
individual_symptoms$QIDS15 <- NA
individual_symptoms$QIDS16 <- NA

### MADRS_Total, MADRS1-MADRS10 ----
individual_symptoms$MADRS_Total <- NA
individual_symptoms$MADRS1 <- NA
individual_symptoms$MADRS2 <- NA
individual_symptoms$MADRS3 <- NA
individual_symptoms$MADRS4 <- NA
individual_symptoms$MADRS5 <- NA
individual_symptoms$MADRS6 <- NA
individual_symptoms$MADRS7 <- NA
individual_symptoms$MADRS8 <- NA
individual_symptoms$MADRS9 <- NA
individual_symptoms$MADRS10 <- NA

### CES-D_Total, CES-D1-CES-D20 ----
individual_symptoms$`CES-D_Total` <- NA
individual_symptoms$`CES-D1` <- NA
individual_symptoms$`CES-D2` <- NA
individual_symptoms$`CES-D3` <- NA
individual_symptoms$`CES-D4` <- NA
individual_symptoms$`CES-D5` <- NA
individual_symptoms$`CES-D6` <- NA
individual_symptoms$`CES-D7` <- NA
individual_symptoms$`CES-D8` <- NA
individual_symptoms$`CES-D9` <- NA
individual_symptoms$`CES-D10` <- NA
individual_symptoms$`CES-D11` <- NA
individual_symptoms$`CES-D12` <- NA
individual_symptoms$`CES-D13` <- NA
individual_symptoms$`CES-D14` <- NA
individual_symptoms$`CES-D15` <- NA
individual_symptoms$`CES-D16` <- NA
individual_symptoms$`CES-D17` <- NA
individual_symptoms$`CES-D18` <- NA
individual_symptoms$`CES-D19` <- NA
individual_symptoms$`CES-D20` <- NA

### CTQ_Total,  CTQ_emotional_abuse, CTQ_physical_abuse, CTQ_sexual_abuse,  CTQ_emotional_neglect, CTQ_physical_neglect, CTQ_minimization_denial ----
CTQ <- read.csv("/Volumes/STRADL/Collated/STRADL-Measures-Phenotypic/STRADL_Scores_CTQ.csv")
individual_symptoms <- left_join(individual_symptoms, CTQ, by = c("SubjID" = "ID"))

individual_symptoms$CTQ_Total <- rowSums(individual_symptoms[,names(individual_symptoms) %in% paste0("CTQ_", 1:28)])

individual_symptoms$CTQ_emotional_abuse <- individual_symptoms$TotalEA
individual_symptoms$CTQ_physical_abuse <- individual_symptoms$TotalPA
individual_symptoms$CTQ_sexual_abuse <- individual_symptoms$TotalSA
individual_symptoms$CTQ_emotional_neglect <- individual_symptoms$TotalEN
individual_symptoms$CTQ_physical_neglect <- individual_symptoms$TotalPN
individual_symptoms$CTQ_minimization_denial <- individual_symptoms$TotalDenial


# Save individual symptoms ----
individual_symptoms <- individual_symptoms %>% 
  select(SubjID, BDI_Total, paste0("BDI", 1:21),
         HDRS_Total, paste0("HDRS", 1:17),
         IDS_Total, paste0("IDS", 1:30),
         QIDS_Total, paste0("QIDS", 1:16),
         MADRS_Total, paste0("MADRS", 1:10),
         "CES-D_Total", paste0("CES-D", 1:20),
         CTQ_Total, CTQ_emotional_abuse, CTQ_physical_abuse, CTQ_sexual_abuse, CTQ_emotional_neglect, CTQ_physical_neglect, CTQ_minimization_denial)


write.csv(individual_symptoms, "/Volumes/STRADL/Processing/ENIGMA_covars/STRADL_individual_symptoms.csv")

# Medication ----

### SubjID -----
freetext_meds <- read.csv("/Volumes/STRADL/Collated/STRADL-Measures-Phenotypic/STRADL_Meds_Freetext.csv")

medication <- freetext_meds %>% ## SubjID, Age, Sex, site (from ID)
  left_join(psych_meds, by = "ID") %>%
  rename(SubjID = ID) 

## Categorize all psychotropic meds
antidepressants <- c("Citalopram", "Amitriptyline", "Amitriptyline", "Fluoxetine", "Sertraline",
                     "Escitalopram", "Venlafaxine", "Mirtazapine", "Paroxetine",
                     "Clomipramine", "Duloxetine", "Nortriptyline", "Trazodone",
                     "Tranylcypromine", "Dusolepin")

antiepileptic <- c("Gabapentin", "Carbamazapine", "Pregabalin", "Levetiracetam",
                   "Lamotrigine")

other_psychotropic <- c("Diazepam", "Zopiclone", "Diazepam", "Lorazepam", "Prazosin")


### Antidepressant_Med1, Antidepressant_Med2 ----

# Function to extract specific medications and return as separate columns
extract_medication <- function(current_medication, specific_medications) {
  # Split the string of prescribed drugs into a vector
  prescribed_drug_list <- unlist(strsplit(current_medication, ",\\s*"))
  
  # Find intersection with specific_medications
  prescribed_specific_medication <- intersect(prescribed_drug_list, specific_medications)
  
  # Return specific_medications, filling with NA if none
  return(c(prescribed_specific_medication, rep(NA, length(specific_medications) - length(prescribed_specific_medication))))
}


## Apply the function to each row in the dataframe
antidepressants_split <- t(sapply(medication$Meds_Psych_List, extract_medication, antidepressants))

## Convert result to dataframe
antidepressant_df <- data.frame(antidepressants_split)

## Assign column names for the new dataframe
colnames(antidepressant_df) <- paste0("Antidepressant_Med", 1:ncol(antidepressant_df))

# Combine the original dataframe with the new antidepressant columns
medication <- cbind(medication, antidepressant_df)

### Antidepressant_Daily_Dose_Med1, Antidepressant_Duration_Years_Med1, Antidepressant_Daily_Dose_Med2, Antidepressant_Duration_Years_Med2 ----
medication$Antidepressant_Daily_Dose_Med1 <- NA
medication$Antidepressant_Duration_Years_Med1 <- NA

medication$Antidepressant_Daily_Dose_Med2 <- NA
medication$Antidepressant_Duration_Years_Med2 <- NA

### Lithium_Med1 ----
## No participants taking multiple lithium medications
medication$Lithium_Med1  <- sapply(medication$Meds_Psych_List, extract_medication, lithium)

### Lithium_Daily_Dose_Med1, Lithium_Duration_Years_Med1 ----
medication$Lithium_Daily_Dose_Med1 <- NA
medication$Lithium_Duration_Years_Med1 <- NA
medication$Lithium_Med2 <- NA
medication$Lithium_Daily_Dose_Med2 <- NA
medication$Lithium_Duration_Years_Med2 <- NA

### Typical_AntiPsych_Med1, Typical_AntiPsych_Med2 ----
typical_antipsych_split <- t(sapply(medication$Meds_Psych_List, extract_medication, typical_antipsychotics))

## Convert result to dataframe
typical_antipsych_df <- data.frame(typical_antipsych_split)

## Assign column names for the new dataframe
colnames(typical_antipsych_df) <- paste0("Typical_AntiPsych_Med", 1:ncol(typical_antipsych_df))

# Combine the original dataframe with the new antidepressant columns
medication <- cbind(medication, typical_antipsych_df)

### Typical_AntiPsych_Daily_Dose_Med1, Typical_AntiPsych_Duration_Years_Med1, Typical_AntiPsych_Daily_Dose_Med2, Typical_AntiPsych_Duration_Years_Med2 ----
medication$Typical_AntiPsych_Daily_Dose_Med1 <- NA
medication$Typical_AntiPsych_Duration_Years_Med1 <- NA
medication$Typical_AntiPsych_Daily_Dose_Med2 <- NA
medication$Typical_AntiPsych_Duration_Years_Med2 <- NA

### Atypical_AntiPsych_Med1, Atypical_AntiPsych_Med2 ----
atypical_antipsych_split <- t(sapply(medication$Meds_Psych_List, extract_medication, atypical_antipsychotics))

## Convert result to dataframe
atypical_antipsych_df <- data.frame(atypical_antipsych_split)

## Assign column names for the new dataframe
colnames(atypical_antipsych_df) <- paste0("Atypical_AntiPsych_Med", 1:ncol(atypical_antipsych_df))

# Combine the original dataframe with the new antidepressant columns
medication <- cbind(medication, atypical_antipsych_df[1:2])

### Atypical_AntiPsych_Daily_Dose_Med1, Atypical_AntiPsych_Duration_Years_Med1, Atypical_AntiPsych_Daily_Dose_Med2, Atypical_AntiPsych_Duration_Years_Med2 ----
medication$Atypical_AntiPsych_Daily_Dose_Med1 <- NA
medication$Atypical_AntiPsych_Duration_Years_Med1 <- NA
medication$Atypical_AntiPsych_Daily_Dose_Med2 <- NA
medication$Atypical_AntiPsych_Duration_Years_Med2 <- NA

### Antiepileptic_Med1, Antiepileptic_Med2 ----
antiepileptic_split <- t(sapply(medication$Meds_Psych_List, extract_medication, antiepileptic))

## Convert result to dataframe
antiepileptic_df <- data.frame(antiepileptic_split)

## Assign column names for the new dataframe
colnames(antiepileptic_df) <- paste0("Antiepileptic_Med", 1:ncol(antiepileptic_df))

# Combine the original dataframe with the new antidepressant columns
medication <- cbind(medication, antiepileptic_df)


### Antiepileptic_Daily_Dose_Med1, Antiepileptic_Duration_Years_Med1, Antiepileptic_Daily_Dose_Med2, Antiepileptic_Duration_Years_Med2 ----
medication$Antiepileptic_Daily_Dose_Med1 <- NA
medication$Antiepileptic_Duration_Years_Med1 <- NA

medication$Antiepileptic_Daily_Dose_Med2 <- NA
medication$Antiepileptic_Duration_Years_Med2 <- NA

### Other_Med1, Other_Med2 ----
other_medication_split <- t(sapply(medication$Meds_Psych_List, extract_medication, other_psychotropic))

## Convert result to dataframe
other_medication_df <- data.frame(other_medication_split)

## Assign column names for the new dataframe
colnames(other_medication_df) <- paste0("Other_Med", 1:ncol(other_medication_df))

# Combine the original dataframe with the new antidepressant columns
medication <- cbind(medication, other_medication_df[1:2])


### Other_Daily_Dose_Med1, Other_Duration_Years_Med1, Other_Daily_Dose_Med2, Other_Duration_Years_Med2, Serum_Level1, Medication_Serum_Level1 ----
medication$Other_Daily_Dose_Med1 <- NA
medication$Other_Duration_Years_Med1 <- NA

medication$Other_Daily_Dose_Med2 <- NA
medication$Other_Duration_Years_Med2 <- NA
medication$Serum_Level1 <- NA
medication$Medication_Serum_Level1 <- NA

# Save medication----
medication <- medication %>% 
  select(SubjID, 
         Antidepressant_Med1, Antidepressant_Daily_Dose_Med1, Antidepressant_Duration_Years_Med1, Antidepressant_Med2,
         Antidepressant_Daily_Dose_Med2, Antidepressant_Duration_Years_Med2, Lithium_Med1, Lithium_Daily_Dose_Med1,
         Lithium_Duration_Years_Med1, Lithium_Med2, Lithium_Daily_Dose_Med2, Lithium_Duration_Years_Med2, Typical_AntiPsych_Med1,
         Typical_AntiPsych_Daily_Dose_Med1, Typical_AntiPsych_Duration_Years_Med1, Typical_AntiPsych_Med2,
         Typical_AntiPsych_Daily_Dose_Med2, Typical_AntiPsych_Duration_Years_Med2, Atypical_AntiPsych_Med1, Atypical_AntiPsych_Daily_Dose_Med1, 
         Atypical_AntiPsych_Duration_Years_Med1, Atypical_AntiPsych_Med2, Atypical_AntiPsych_Daily_Dose_Med2,
         Atypical_AntiPsych_Duration_Years_Med2, Antiepileptic_Med1, Antiepileptic_Daily_Dose_Med1,
         Antiepileptic_Duration_Years_Med1, Antiepileptic_Med2, Antiepileptic_Daily_Dose_Med2, 
         Antiepileptic_Duration_Years_Med2, Other_Med1, Other_Daily_Dose_Med1, Other_Duration_Years_Med1,
         Other_Med2, Other_Daily_Dose_Med2, Other_Duration_Years_Med2, Serum_Level1, Medication_Serum_Level1
         )


write.csv(medication, "/Volumes/STRADL/Processing/ENIGMA_covars/STRADL_medication.csv")

