
################################# File Name : Data_Cleaning.R ################################
## Description : This file performs analysis of data issues and fixes to generate clean data
##               before modelling.
##############################################################################################

rm(list=ls())
setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\U.S.OpiatePrescriptionsOverdoses")
load("Data_Formatting.RData")

# Load required libraries
library('rvest')
library('tidyr')
library('dplyr')
library('Amelia')

# Extract state codes from web pages
# Using selectorGadget to find CSS
stateCodePage <- read_html("https://www.infoplease.com/state-abbreviations-and-state-postal-codes")
stateCodeAbbvr <- as.data.frame(stateCodePage %>% 
                                  html_node(".sgmltable") %>% 
                                  html_table())


# Check if we have codes for all states in source data
setdiff(DemogDrugState$State, stateCodeAbbvr$State)
setdiff(DemogDrugStateRaceAge$State, stateCodeAbbvr$State)
# District of Columbia is mis-spelt
# Let's change it in stateCodeAbbvr data
stateCodeAbbvr$State[stateCodeAbbvr$State == "Dist. of Columbia"] <- "District of Columbia"


# Carefully map each state with approrpiate postal codes
DemogDrugState <- merge(DemogDrugState, stateCodeAbbvr, by.x = "State", by.y = "State")
DemogDrugState <- within(DemogDrugState, rm("Abbreviation"))

DemogDrugStateRaceAge <- merge(DemogDrugStateRaceAge, stateCodeAbbvr, by.x = "State", by.y = "State")
DemogDrugStateRaceAge <- within(DemogDrugStateRaceAge, rm("Abbreviation"))


# It's better to remove opiods data from dataset as it makes the prediction obvious
opioids <- as.list(DrugCatOpiod[,"Drug Name"])
opioids <- gsub("\ |-",".",opioids)
Prescriber <- PrescNPIDrugEvents  # Backup main data
Prescriber <- Prescriber[, !names(Prescriber) %in% opioids]


# Examine datatypes
str(Prescriber)
# Issues : NPI, Opioid Prescriber should not be numeric


# Check data before converting to prevent junk convertion
# Selecting non-drug columns to check statistics. Including drug columns 
# at the same time will make it more complex.
colsNonDrug <- c("NPI","Gender","State", "Credentials", "Specialty", "Opioid_Claims","Opioid.Prescriber")
summary(Prescriber[,colsNonDrug])
# Too many NA's in Opioid_Claims. Let's find the percentage
percentNA <- sum(is.na(Prescriber$Opioid_Claims)) * 100 / length(Prescriber$Opioid_Claims)
percentNA
# 24% NA values. Intuitively is the Claim is NA, we have to assume it to be 0.
Prescriber$Opioid_Claims[which(is.na(Prescriber$Opioid_Claims))] <- 0


# Let's run the missing value check on whole dataset
Null_Counter <- apply(Prescriber, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == "-"))/length(x))
Null_Counter


# NPI, Opioid Prescriber should be factor
Prescriber$NPI <- as.factor(Prescriber$NPI)
Prescriber$Opioid.Prescriber <- as.factor(Prescriber$Opioid.Prescriber)
numVars <- names(which(sapply(Prescriber, is.numeric)))
catVars <- names(which(sapply(Prescriber, is.factor)))
logicalVars <- names(which(sapply(Prescriber, is.logical)))
# Cross verify
stopifnot(length(numVars) + length(catVars) + length(logicalVars) == ncol(Prescriber))

# Check missing data using Amelia
missmap(Prescriber[,colsNonDrug], main = "Missing values ")

# Check structure again
str(Prescriber[,colsNonDrug])

# Check is NPI is unique as it is the primary key.
stopifnot(length(levels(Prescriber$NPI)) == nrow(Prescriber))


# Gender has "" values
Prescriber[Prescriber$Gender == "",colsNonDrug]
# Only 1 record has missing gender but it is a Opioid Prescriber
percentGenderMiss <- nrow(Prescriber[Prescriber$Gender == "",colsNonDrug]) * 100 /nrow(Prescriber)
percentGenderMiss
# Remove the record as the count is very small
ifelse(percentGenderMiss < 0.001, 
       Prescriber <- Prescriber[-c(Prescriber[Prescriber$Gender == "","NPI"]),])
# Drop unused levels
Prescriber$Gender <- droplevels(Prescriber$Gender)
stopifnot(levels(Prescriber$Gender) > 2)


# Credentials have "" value
percentCredMiss <- nrow(Prescriber[Prescriber$Credentials == "",colsNonDrug]) * 100 /nrow(Prescriber)
percentCredMiss
# 3.12% credentials are missing as "".


Prescriber %>%
  group_by(Credentials) %>%
  dplyr::summarise(credential.counts = n()) %>%
  arrange(credential.counts) %>% 
  data.frame() %>% 
  head(n=100)
# Credentials are very inconsistent. It cannot be used as factors
Prescriber$Credentials <- as.character(Prescriber$Credentials)

# We can set missing "" as Not Available. 
Prescriber$Credentials[Prescriber$Credentials == ""] <- 'NA'  # Check again.

# Speciality looks fine.

# Checking States
unique(Prescriber$State)
# Something is wrong. There are 61 levels but States are 50.

MissingStateMainData <- setdiff(stateCodeAbbvr$`Postal Code`,unique(Prescriber$State))
stateCodeAbbvr[stateCodeAbbvr$`Postal Code` %in% c(MissingStateMainData),"State"]
# This is not our concern


ErrorStatesMainData <- setdiff(unique(Prescriber$State), stateCodeAbbvr$`Postal Code`)
# Above are the error values in the prescriber data. These are typos or union territories.
# We can create "other" category
# Insert a new level
levels(Prescriber$State) <- c(levels(Prescriber$State),"other")
Prescriber$State[Prescriber$State %in% ErrorStatesMainData] <- "other"
# Drop unused levels
Prescriber$State <- droplevels(Prescriber$State)


# Let's find out what demographic state info we don't have
setdiff(DemogDrugState$`Postal Code`,unique(Prescriber$State))
# There isn't any state in Demography which is not present in main data


MissingDemogInfo <- setdiff(unique(Prescriber$State), DemogDrugState$`Postal Code`)
MissingDemogInfo
# There are states in main data whose demographic information is not present.


################################### Save and Cleanup ###############################

# Remove variables
rm(list = c('PrescNPIDrugEvents', 'catVars','logicalVars','MissingStateMainData',
            'numVars','opioids','percentCredMiss','percentGenderMiss','percentNA',
            'stateCodePage','ErrorStatesMainData'))

# Save Image File
save.image(file="Data_Cleaning.RData")

# Detach libraries
detach("package:dplyr", unload = TRUE)
detach("package:tidyr", unload = TRUE)
detach("package:rvest", unload = TRUE)
detach("package:Amelia", unload = TRUE)

# Remove all
rm(list=ls())