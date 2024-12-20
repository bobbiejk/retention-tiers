# Install packages
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jtools)
library(zoo)
library(patchwork)

# Argument listed in make when running in terminal
arg = commandArgs(trailingOnly = TRUE)
print(paste("Script is currently running for:", arg, "..."))


#=========================================================
# Reading data
#=========================================================

panel = fread(paste("../../gen/preparation/temp/panelUsage_", paste(arg),".csv", sep = ""))


#=========================================================
# Create dataframe
#=========================================================

df = panel %>% select(Panelist, month, weight2, platform, sub.CURRENT, sub.BAND, SubscriptionTime)

#=========================================================
# Satisfaction From Panel
#=========================================================

fill.in.missings = function(data, var){
  
  final.data = data %>% group_by(Panelist, platform, SubscriptionTime) %>%
    mutate(!!var := ifelse(is.na(!!sym(var)) & sub.BAND == 1, nafill(!!sym(var), "locf"), !!sym(var))) %>%
    mutate(!!var := ifelse(is.na(!!sym(var)) & sub.BAND == 1, nafill(!!sym(var), "nocb"), !!sym(var)))
  
  return(final.data)
}

df$Importance = as.numeric(panel$`Video Importance`)
df = fill.in.missings(df, "Importance")

df$NPS = as.numeric(panel$`Subscription NPS Score`)
df = fill.in.missings(df, "NPS")

df$HappyTechnology = ifelse("Buffering|download|forward|interface" %in% panel$`Video Happy`,1, NA)
df$HappySharing = ifelse("different house" %in% panel$`Video Happy`,1, NA)
df$HappySearch = ifelse("Recommendations|ease|search" %in% panel$`Video Happy`,1, NA)
df$HappySport = ifelse("sport" %in% panel$`Video Happy`,1, NA)
df$HappyVariety = ifelse("variety|content|number" %in% panel$`Video Happy`,1, NA) 
df$HappyQuality = ifelse("quality" %in% panel$`Video Happy`,1, NA)
df$HappyValue = ifelse("Value for money" %in% panel$`Video Happy`,1, NA)
df$HappyNotManyAds = ifelse("The amount of adverts" %in% panel$`Video Happy`,1, NA)
df$HappyNoAds = ifelse("without adverts" %in% panel$`Video Happy`,1, NA)

df$UnhappyTechnology = ifelse("Buffering|download|forward|interface" %in% panel$`Video Unhappy`,1, NA)
df$UnhappySharing = ifelse("different house" %in% panel$`Video Unhappy`,1, NA)
df$UnhappySearch = ifelse("Recommendations|ease|search" %in% panel$`Video Unhappy`,1, NA)
df$UnhappySport = ifelse("sport" %in% panel$`Video Unhappy`,1, NA)
df$UnhappyVariety = ifelse("variety|content|number" %in% panel$`Video Unhappy`,1, NA)
df$UnhappyQuality = ifelse("quality" %in% panel$`Video Unhappy`,1, NA)
df$UnhappyValue = ifelse("Value for money" %in% panel$`Video Unhappy`,1, NA)
df$UnhappyNotManyAds = ifelse("The amount of adverts" %in% panel$`Video Unhappy`,1, NA)
df$UnhappyNoAds = ifelse("without adverts" %in% panel$`Video Unhappy`,1, NA)



#=========================================================
# Merge Data
#=========================================================

not.included = panel %>% select(-c(`Video Importance`, `Subscription NPS Score`, `Video Happy`, `Video Unhappy`))

df = left_join(df, not.included, by = c("Panelist", "month", "weight2", "platform", "sub.CURRENT", "sub.BAND", "SubscriptionTime"))

#=========================================================
# Save Data
#=========================================================

fwrite(df, paste("../../gen/preparation/temp/panelSatisfaction_", paste(arg),".csv", sep = ""))

# done
finished = list("done!")
fwrite(finished, "../../gen/preparation/temp/satisfaction.txt")
