# Install packages
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jtools)
library(zoo)
library(patchwork)
library(stringr)
library(stringi)

# Argument listed in make when running in termi0l
arg = commandArgs(trailingOnly = TRUE)
print(paste("Script is currently running for:", arg, "..."))


#=========================================================
# Reading data
#=========================================================

panel = fread(paste("../../gen/preparation/temp/panelSatisfaction_", paste(arg),".csv", sep = ""))


#=========================================================
# Remove unneccessary 
#=========================================================

panel = panel %>% select(-c(grep("Cable|Store|Duration Out|Length Ownership|Prime|Ent Audiences|New user|Sport|Subscribe to|Agree|Need|Brand|New Consider|Bundle Main|Go to|Cancelled|Boomerang|Number of Times|APV|Activity|Prime Services|Rental|Mobile", colnames(panel))))



#=========================================================
# Save Data
#=========================================================

dir.create("../../gen/preparation/output/")
fwrite(panel, paste("../../gen/preparation/output/panelFinal_", paste(arg),".csv", sep = ""))

# done
finished = list("done!")
fwrite(finished, "../../gen/preparation/temp/final.txt")
