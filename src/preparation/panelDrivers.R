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

# Argument listed in make when running in terminal
arg = commandArgs(trailingOnly = TRUE)
print(paste("Script is currently running for:", arg, "..."))


#=========================================================
# Reading data
#=========================================================

panel = fread(paste("../../gen/preparation/temp/panelSatisfaction_", paste(arg),".csv", sep = ""))


#=========================================================
# Create dataframe
#=========================================================

df = panel %>% select(Panelist, month, weight2, platform, sub.CURRENT, sub.BAND, SubscriptionTime)

#=========================================================
# Drivers From Panel
#=========================================================

setDT(df)

df[, ReasonTechnology := +(stri_detect_fixed(panel$`Video Reason`, "Buffering|download|forward|interface"))]
df[, ReasonSharing := +(stri_detect_fixed(panel$`Video Reason`, "different house"))]
df[, ReasonSearch := +(stri_detect_fixed(panel$`Video Reason`, "Recommendations|ease|search"))]
df[, ReasonSport := +(stri_detect_fixed(panel$`Video Reason`, "sport"))]
df[, ReasonVariety := +(stri_detect_fixed(panel$`Video Reason`, "variety|content|number"))]
df[, ReasonQuality := +(stri_detect_fixed(panel$`Video Reason`, "quality"))]
df[, ReasonValue := +(stri_detect_fixed(panel$`Video Reason`, "Value for money"))]
df[, ReasonNotManyAds := +(stri_detect_fixed(panel$`Video Reason`, "The amount of adverts"))]
df[, ReasonNoAds := +(stri_detect_fixed(panel$`Video Reason`, "without adverts"))]

df[, TopReasonTechnology := +(stri_detect_fixed(panel$`Video Reasons Top`, "Buffering|download|forward|interface"))]
df[, TopReasonSharing := +(stri_detect_fixed(panel$`Video Reasons Top`, "different house"))]
df[, TopReasonSearch := +(stri_detect_fixed(panel$`Video Reasons Top`, "Recommendations|ease|search"))]
df[, TopReasonSport := +(stri_detect_fixed(panel$`Video Reasons Top`, "sport"))]
df[, TopReasonVariety := +(stri_detect_fixed(panel$`Video Reasons Top`, "variety|content|number"))]
df[, TopReasonQuality := +(stri_detect_fixed(panel$`Video Reasons Top`, "quality"))]
df[, TopReasonValue := +(stri_detect_fixed(panel$`Video Reasons Top`, "Value for money"))]
df[, TopReasonNotManyAds := +(stri_detect_fixed(panel$`Video Reasons Top`, "The amount of adverts"))]
df[, TopReasonNoAds := +(stri_detect_fixed(panel$`Video Reasons Top`, "without adverts"))]

df[, PathReviews := +(stri_detect_fixed(panel$`Video Path to Purchase`, "reviews"))]
df[, PathFriends := +(stri_detect_fixed(panel$`Video Path to Purchase`, "spoke"))]
df[, PathAds := +(stri_detect_fixed(panel$`Video Path to Purchase`, "advert"))]
df[, PathTechnology := +(stri_detect_fixed(panel$`Video Path to Purchase`, "App|website|Service"))]
df[, PathSample := +(stri_detect_fixed(panel$`Video Path to Purchase`, "tested"))]
df[, PathTrial := +(stri_detect_fixed(panel$`Video Path to Purchase`, "free trial"))]
df[, PathOffer := +(stri_detect_fixed(panel$`Video Path to Purchase`, "email|bundle"))]
df[, PathSearch := +(stri_detect_fixed(panel$`Video Path to Purchase`, "Searched"))]

df[, TopPathReviews := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "reviews"))]
df[, TopPathFriends := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "spoke"))]
df[, TopPathAds := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "advert"))]
df[, TopPathTechnology := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "App|website|Service"))]
df[, TopPathSample := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "tested"))]
df[, TopPathTrial := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "free trial"))]
df[, TopPathOffer := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "email|bundle"))]
df[, TopPathSearch := +(stri_detect_fixed(panel$`Video Path to Purchase Top`, "Searched"))]


# Cumsum over all columns
initial.df = df[, lapply(.SD, cumsum), by = c("Panelist", "platform", "SubscriptionTime"), 
                               .SDcols = c("ReasonTechnology", "ReasonSharing", "ReasonSearch", "ReasonSport", "ReasonVariety", "ReasonQuality", "ReasonValue", "ReasonNotManyAds", "ReasonNoAds", "TopReasonTechnology", "TopReasonSharing", "TopReasonSearch", "TopReasonSport", "TopReasonVariety", "TopReasonQuality", "TopReasonValue", "TopReasonNotManyAds", "TopReasonNoAds", "PathReviews", "PathFriends", "PathAds", "PathTechnology", "PathSample", "PathTrial", "PathOffer", "PathSearch", "TopPathReviews", "TopPathFriends", "TopPathAds", "TopPathTechnology", "TopPathSample", "TopPathTrial", "TopPathOffer", "TopPathSearch")]

initial.df = initial.df %>% arrange(Panelist, platform, SubscriptionTime)
df = df %>% arrange(Panelist, platform, SubscriptionTime, month)

# Sum the columns for each panelist, platform month in data.table
initial.df = cbind(df %>% select(Panelist, platform, SubscriptionTime, month), initial.df %>% select(-c(Panelist, platform, SubscriptionTime)))

initial.nr.df = as.data.table(initial.df)

initial.nr.df[, `:=`(
  ReasonMentioned = rowSums(.SD, na.rm = TRUE)
), .SDcols = c("ReasonTechnology", "ReasonSharing", "ReasonSearch", "ReasonSport", "ReasonVariety", "ReasonQuality", "ReasonValue", "ReasonNotManyAds", "ReasonNoAds")]

initial.nr.df[, `:=`(
  TopReasonMentioned = rowSums(.SD, na.rm = TRUE)
), .SDcols = c("TopReasonTechnology", "TopReasonSharing", "TopReasonSearch", "TopReasonSport", "TopReasonVariety", "TopReasonQuality", "TopReasonValue", "TopReasonNotManyAds", "TopReasonNoAds")]

initial.nr.df[, `:=`(
  PathMentioned = rowSums(.SD, na.rm = TRUE)
), .SDcols = c("PathReviews", "PathFriends", "PathAds", "PathTechnology", "PathSample", "PathTrial", "PathOffer", "PathSearch")]

initial.nr.df[, `:=`(
  TopPathMentioned = rowSums(.SD, na.rm = TRUE)
), .SDcols = c("TopPathReviews", "TopPathFriends", "TopPathAds", "TopPathTechnology", "TopPathSample", "TopPathTrial", "TopPathOffer", "TopPathSearch")]

# Create initial variables
initial.nr.df = as.data.frame(initial.nr.df)

initial.reason.df = initial.nr.df %>% filter(ReasonMentioned >= 1) %>% 
  distinct(Panelist, platform, SubscriptionTime, .keep_all = T) %>%
  select(Panelist, platform, SubscriptionTime, ReasonTechnology, ReasonSharing, ReasonSearch, ReasonSport, ReasonVariety, ReasonQuality, ReasonValue, ReasonNotManyAds, ReasonNoAds)
colnames(initial.reason.df) = str_replace_all(colnames(initial.reason.df), "Reason", "InitialReason") 

initial.top.reason.df = initial.nr.df %>% filter(TopReasonMentioned >= 1) %>% 
  distinct(Panelist, platform, SubscriptionTime, .keep_all = T) %>%
  select(Panelist, platform, SubscriptionTime, TopReasonTechnology, TopReasonSharing, TopReasonSearch, TopReasonSport, TopReasonVariety, TopReasonQuality, TopReasonValue, TopReasonNotManyAds, TopReasonNoAds)
colnames(initial.top.reason.df) = str_replace_all(colnames(initial.top.reason.df), "TopReason", "InitialTopReason")

initial.path.df = initial.nr.df %>% filter(PathMentioned >= 1) %>% 
  distinct(Panelist, platform, SubscriptionTime, .keep_all = T) %>%
  select(Panelist, platform, SubscriptionTime, PathReviews, PathFriends, PathAds, PathTechnology, PathSample, PathTrial, PathOffer, PathSearch)
colnames(initial.path.df) = str_replace_all(colnames(initial.path.df), "Path", "InitialPath")

initial.top.path.df = initial.nr.df %>% filter(TopPathMentioned >= 1) %>% 
  distinct(Panelist, platform, SubscriptionTime, .keep_all = T) %>%
  select(Panelist, platform, SubscriptionTime, TopPathReviews, TopPathFriends, TopPathAds, TopPathTechnology, TopPathSample, TopPathTrial, TopPathOffer, TopPathSearch)
colnames(initial.top.path.df) = str_replace_all(colnames(initial.top.path.df), "TopPath", "InitialTopPath")

# Merge data to df, and ensure that Initial variables hold over entire Panelist - platform - SubscriptionTime
df = left_join(df, initial.reason.df, by = c("Panelist", "platform", "SubscriptionTime"))
df = left_join(df, initial.top.reason.df, by = c("Panelist", "platform", "SubscriptionTime"))
df = left_join(df, initial.path.df, by = c("Panelist", "platform", "SubscriptionTime"))
df = left_join(df, initial.top.path.df, by = c("Panelist", "platform", "SubscriptionTime"))









#=========================================================
# Merge Data
#=========================================================

not.included = panel %>% select(-c(`Video Reason`, `Video Reasons Top`, `Video Path to Purchase`, `Video Path to Purchase Top`))

df$month = as.Date(df$month, format = "%Y-%m-%d")
not.included$month = as.Date(not.included$month)
df = left_join(df, not.included, by = c("Panelist", "month", "weight2", "platform", "sub.CURRENT", "sub.BAND", "SubscriptionTime"))

#=========================================================
# Save Data
#=========================================================

fwrite(df, paste("../../gen/preparation/temp/panelDrivers_", paste(arg),".csv", sep = ""))

# done
finished = list("done!")
fwrite(finished, "../../gen/preparation/temp/drivers.txt")
