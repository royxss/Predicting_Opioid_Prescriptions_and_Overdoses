
############################# File Name : Data_Formatting.R ############################
## Description : This file performs merging and basic cleansing of files.
##               It removes unimportant columns, normalizes main datafile,
##               and transforms rows/columns to the need of the problem.
########################################################################################

rm(list=ls())
setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\U.S.OpiatePrescriptionsOverdoses")
load("Init_Data_Load.RData")

# Load required libraries
library(dplyr)
library(tidyr)

# Discard unimportant columns.
# Filter drug induced cases only from demographic data
# Notice the data has many invalid rows given as extra statistics.
# We can filter if the state code values are NA

DemogDrugState <- DemogDrugState[DemogDrugState$Drug.Alcohol.Induced.Causes == 'Drug-Induced Causes' & !is.na(DemogDrugState$State.Code) ,
                            c('State','Deaths','Population')] 

DemogDrugStateRaceAge <- DemogDrugStateRaceAge[DemogDrugStateRaceAge$Drug.Alcohol.Induced.Causes == 'Drug-Induced Causes' & !is.na(DemogDrugStateRaceAge$State.Code) ,
                                 c('State','Race','Ten.Year.Age.Groups','Deaths','Population')]

# Notice 51 States. We will fix it later

# Choose most common occuring drugs to make the data more meaningful.
num_drugs <- 250

drug_names <- PrescNPIDrugEvents %>%
  group_by(DRUG_NAME) %>%
  summarise(occurences = n()) %>%
  arrange(desc(occurences))


# Subsetting drugs out of total drugs (2703).
drugs <- as.character(drug_names$DRUG_NAME[1:num_drugs])


# Replace any hypenated or compound drug names with periods
drugs <- sort(gsub("\ |-",".",drugs))
PrescNPIDrugEvents$DRUG_NAME <- gsub("\ |-",".",PrescNPIDrugEvents$DRUG_NAME)


# Only consider entries that prescribed at least one of the drugs 
PrescNPIDrugEvents <- PrescNPIDrugEvents[PrescNPIDrugEvents$DRUG_NAME %in% drugs,]


# Combine the prescriptions for drugs 
# that are repeated (multiple entries for the same drug for the same prescriber)
ptm <- proc.time()
PrescNPIDrugEvents <- PrescNPIDrugEvents %>%
  group_by(NPI,NPPES_PROVIDER_LAST_ORG_NAME,NPPES_PROVIDER_FIRST_NAME,DRUG_NAME) %>%
  mutate(TOTAL_CLAIM_COUNT=sum(TOTAL_CLAIM_COUNT,na.rm=TRUE)) %>%
  filter(!duplicated(DRUG_NAME)) %>%
  ungroup()
proc.time() - ptm


# Convert from long to wide format and collapse the rows to one row per prescriber 
# with the number of prescriptions written for each drug

ptm <- proc.time()
PrescNPIDrugEvents <- PrescNPIDrugEvents %>% 
  select(NPI,DRUG_NAME, TOTAL_CLAIM_COUNT) %>%
  spread(key=DRUG_NAME, value=TOTAL_CLAIM_COUNT,fill=0) %>%
  select(NPI, one_of(drugs))
proc.time() - ptm


# Merge with PrescNPISummary about the PrescNPIDrugEvents
ptm <- proc.time()
PrescNPIDrugEvents <- PrescNPIDrugEvents %>% 
  merge(PrescNPISummary, by="NPI") %>%
  mutate(Opioid.Prescriber=ifelse( (OPIOID_BENE_COUNT<10 | is.na(OPIOID_BENE_COUNT)) & (OPIOID_CLAIM_COUNT<10 | is.na(OPIOID_CLAIM_COUNT)) ,0,1)) %>%
  select(NPI, Gender=NPPES_PROVIDER_GENDER, State=NPPES_PROVIDER_STATE, Credentials=NPPES_CREDENTIALS, Specialty=SPECIALTY_DESCRIPTION, 
         Opioid_Claims=OPIOID_CLAIM_COUNT, Opioid.Prescriber, one_of(drugs))
proc.time() - ptm


################################### Save and Cleanup ###############################

# Remove variables which need not be saved
rm(list = c('drug_names','PrescNPISummary','drugs','num_drugs','ptm'))

# Save Image File
save.image(file="Data_Formatting.RData")

# Detach libraries
detach("package:dplyr", unload = TRUE)
detach("package:tidyr", unload = TRUE)

# Remove all
rm(list=ls())
