
############################### File Name : Init_Data_Load.R ##########################
## Description : This script lands all the relevant data as-is. 
##               No transformation is done in this layer.
#######################################################################################

# Set local directory
setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\U.S.OpiatePrescriptionsOverdoses")

# Load required libraries
library(readxl)


ptm <- proc.time()
DrugCatOpiod <- read_excel("PartD_Prescriber_PUF_NPI_14_Drug_Category_Lists.xlsx", 
                                sheet = "Opioid Drug Names",
                                col_names = TRUE, col_types = NULL, na = "", skip = 2)
DrugCatOpiod <- DrugCatOpiod[-((nrow(DrugCatOpiod)-2):(nrow(DrugCatOpiod))),-1]
proc.time() - ptm

ptm <- proc.time()
PrescNPISummary <- read.table("PartD_Prescriber_PUF_NPI_14.txt", header = TRUE, sep = "\t",
                              fill=TRUE, strip.white=TRUE, comment.char="", 
                              quote = "",  
                              encoding="UTF-8")
proc.time() - ptm


ptm <- proc.time()
PrescNPIDrugEvents <- read.table("PartD_Prescriber_PUF_NPI_Drug_14.txt", header = TRUE, sep = "\t",
                              fill=TRUE, strip.white=TRUE, comment.char="", 
                              quote = "",  
                              encoding="UTF-8")
proc.time() - ptm

ptm <- proc.time()
DemogDrugStateRaceAge <- read.table("Yr_Drug_State_Race_AgeGp 2014.txt", header = TRUE, sep = "\t",
                                 fill=TRUE, strip.white=TRUE, comment.char="", 
                                 encoding="UTF-8")
proc.time() - ptm


ptm <- proc.time()
DemogDrugState <- read.table("Yr_Drug_State 2014.txt", header = TRUE, sep = "\t",
                                    fill=TRUE, strip.white=TRUE, comment.char="", 
                                    encoding="UTF-8")
proc.time() - ptm



################################### Save and Cleanup ###############################
# Save Image File
save.image(file="Init_Data_Load.RData")

# Detach libraries
detach("package:readxl", unload = TRUE)

# Remove all
rm(list=ls())
