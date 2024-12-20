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

panel = fread(paste("../../gen/preparation/temp/panelSubscriptionPlan_", paste(arg),".csv", sep = ""))


#=========================================================
# Create dataframe
#=========================================================

df = panel %>% select(Panelist, month, weight2, platform, sub.CURRENT, sub.BAND, SubscriptionTime)

#=========================================================
# Usage From Panel
#=========================================================

fill.in.missings = function(data, var){
  
  final.data = data %>% group_by(Panelist, platform, SubscriptionTime) %>%
    mutate(!!var := ifelse(is.na(!!sym(var)) & sub.BAND == 1, nafill(!!sym(var), "locf"), !!sym(var))) %>%
    mutate(!!var := ifelse(is.na(!!sym(var)) & sub.BAND == 1, nafill(!!sym(var), "nocb"), !!sym(var)))
  
  return(final.data)
}

df$UsageTV = ifelse(panel$`Usage Video TV` == "Do not use", 0, NA)
df$UsageTV = ifelse(panel$`Usage Video TV` == "1-2 days per week", 1, df$UsageTV)
df$UsageTV = ifelse(panel$`Usage Video TV` == "1 hour most days", 2, df$UsageTV)
df$UsageTV = ifelse(panel$`Usage Video TV` == "2 hours most days", 3, df$UsageTV)
df$UsageTV = ifelse(panel$`Usage Video TV` == "3 hours most days", 4, df$UsageTV)
df$UsageTV = ifelse(panel$`Usage Video TV` == "4 hours most days", 5, df$UsageTV)
df$UsageTV = ifelse(panel$`Usage Video TV` == "5+ hours most days", 6, df$UsageTV)

df = fill.in.missings(df, "UsageTV")

df$UsageTablet = ifelse(panel$`Usage Video Tablet` == "Do not use", 0, NA)
df$UsageTablet = ifelse(panel$`Usage Video Tablet` == "1-2 days per week", 1, df$UsageTablet)
df$UsageTablet = ifelse(panel$`Usage Video Tablet` == "1 hour most days", 2, df$UsageTablet)
df$UsageTablet = ifelse(panel$`Usage Video Tablet` == "2 hours most days", 3, df$UsageTablet)
df$UsageTablet = ifelse(panel$`Usage Video Tablet` == "3 hours most days", 4, df$UsageTablet)
df$UsageTablet = ifelse(panel$`Usage Video Tablet` == "4 hours most days", 5, df$UsageTablet)
df$UsageTablet = ifelse(panel$`Usage Video Tablet` == "5+ hours most days", 6, df$UsageTablet)

df = fill.in.missings(df, "UsageTablet")

df$UsagePhone = ifelse(panel$`Usage Video Phone` == "Do not use", 0, NA)
df$UsagePhone = ifelse(panel$`Usage Video Phone` == "1-2 days per week", 1, df$UsagePhone)
df$UsagePhone = ifelse(panel$`Usage Video Phone` == "1 hour most days", 2, df$UsagePhone)
df$UsagePhone = ifelse(panel$`Usage Video Phone` == "2 hours most days", 3, df$UsagePhone)
df$UsagePhone = ifelse(panel$`Usage Video Phone` == "3 hours most days", 4, df$UsagePhone)
df$UsagePhone = ifelse(panel$`Usage Video Phone` == "4 hours most days", 5, df$UsagePhone)
df$UsagePhone = ifelse(panel$`Usage Video Phone` == "5+ hours most days", 6, df$UsagePhone)

df = fill.in.missings(df, "UsagePhone")

df$UsagePC = ifelse(panel$`Usage Video PC` == "Do not use", 0, NA)
df$UsagePC = ifelse(panel$`Usage Video PC` == "1-2 days per week", 1, df$UsagePC)
df$UsagePC = ifelse(panel$`Usage Video PC` == "1 hour most days", 2, df$UsagePC)
df$UsagePC = ifelse(panel$`Usage Video PC` == "2 hours most days", 3, df$UsagePC)
df$UsagePC = ifelse(panel$`Usage Video PC` == "3 hours most days", 4, df$UsagePC)
df$UsagePC = ifelse(panel$`Usage Video PC` == "4 hours most days", 5, df$UsagePC)
df$UsagePC = ifelse(panel$`Usage Video PC` == "5+ hours most days", 6, df$UsagePC)

df = fill.in.missings(df, "UsagePC")

df$UsageOther = ifelse(panel$`Usage Video Other` == "Do not use", 0, NA)
df$UsageOther = ifelse(panel$`Usage Video Other` == "1-2 days per week", 1, df$UsageOther)
df$UsageOther = ifelse(panel$`Usage Video Other` == "1 hour most days", 2, df$UsageOther)
df$UsageOther = ifelse(panel$`Usage Video Other` == "2 hours most days", 3, df$UsageOther)
df$UsageOther = ifelse(panel$`Usage Video Other` == "3 hours most days", 4, df$UsageOther)
df$UsageOther = ifelse(panel$`Usage Video Other` == "4 hours most days", 5, df$UsageOther)
df$UsageOther = ifelse(panel$`Usage Video Other` == "5+ hours most days", 6, df$UsageOther)

df = fill.in.missings(df, "UsageOther")

df$UsageHome = ifelse(panel$`Video Location Used` == "At Home", 1, NA)
df$UsageRemote = ifelse(panel$`Video Location Used` == "At Work/School/College/Univers", 1, NA)
df$UsageCommute = ifelse(panel$`Video Location Used` == "While Commuting", 1, NA)
df$UsageCommute = ifelse(panel$`Video Location Used` == "On the Go/Public Transport", 1, df$UsageCommute)

df = fill.in.missings(df, "UsageHome")
df = fill.in.missings(df, "UsageRemote")
df = fill.in.missings(df, "UsageCommute")


#=========================================================
# Merge Data
#=========================================================

not.included = panel %>% select(-c(`Usage Video Other`, `Usage Video PC`, `Usage Video Tablet`, `Usage Video TV`, `Usage Video Phone`, `Video Location Used`))

df = left_join(df, not.included, by = c("Panelist", "month", "weight2", "platform", "sub.CURRENT", "sub.BAND", "SubscriptionTime"))

#=========================================================
# Save Data
#=========================================================

fwrite(df, paste("../../gen/preparation/temp/panelUsage_", paste(arg),".csv", sep = ""))

# done
finished = list("done!")
fwrite(finished, "../../gen/preparation/temp/usage.txt")
