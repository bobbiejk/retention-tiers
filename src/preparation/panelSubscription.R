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
# Raw Data Coding Correct
#=========================================================



# Read in data
if (arg == "UK"){
  panel.raw = fread("../../data/survey_GB202203_202404.csv")
} else {
  panel.raw = fread("../../data/survey_US202201_202405.csv")
}

coding.subscriptions = fread("../../data/subscriptions.csv")
colnames(coding.subscriptions) = c("subscription_code", "platform")

# Panel read in date format
panel.raw$month = as.Date(paste(panel.raw$period, "01", sep = ""), "%Y%m%d")

# Merge Subscription Coding
panel.raw = left_join(panel.raw, coding.subscriptions, by = c("subscription" = "subscription_code"))



#=========================================================
# Data in Wide Format
#=========================================================

# Store weights2
panel.weights = panel.raw %>% ungroup() %>% select(Panelist, month, weight2) %>% distinct(.)

# Group correctly
panel = panel.raw %>% group_by(Panelist, Country, period, subscription, platform, field_description) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  select(-weight2)

# Panel in Wide Shape
wide_data = panel %>% # all unique values from each question have a unique row
  group_by(Panelist, Country, period, subscription, platform) %>% 
  pivot_wider(names_from = field_description, values_from = Field_Answer, values_fn = list(Value = list)) %>%
  unnest(cols = -c( Panelist, Country, period, subscription, platform, row_id))



#=========================================================
# Subscription Variables Creation
#=========================================================

## First metric

# Subscriptions inferred from Subscription Acquire Band
acquisition = wide_data %>% group_by(Panelist) %>% arrange(month) %>% group_by(Panelist, platform, `Subscription Acquire Band`, `Subscription Acquire Month`) %>%
  select(Panelist, platform, month, `Subscription Acquire Band`, `Subscription Acquire Month`) %>% summarise(insertedMonth = min(month), untilMonth = max(month)) %>%
  group_by(Panelist, platform) %>% mutate(groupNO = n(), bandNO = sum(!is.na(`Subscription Acquire Band`))) %>% filter(!is.na(platform))

acquisition$acquisition.month = as.Date(paste(acquisition$`Subscription Acquire Month`, "01", sep = ""), "%Y%m%d")
acquisition$acquisition.month[is.na(acquisition$acquisition.month)] = acquisition$insertedMonth[is.na(acquisition$acquisition.month)]
acquisition$correct = acquisition$acquisition.month < acquisition$untilMonth

acquisition.inferred = setDT(acquisition %>% filter(correct == T))[, .(Panelist, platform, month = seq(as.Date(acquisition.month), as.Date(untilMonth) ,by = "month")),
                                                                   by = .(rn = 1:nrow(acquisition %>% filter(correct == T)))][, rn := NULL][]
single.months = setDT(acquisition %>% filter(correct == F))[, .(Panelist, platform, month = as.Date(acquisition.month)),
                                                            by = .(rn = 1:nrow(acquisition %>% filter(correct == F)))][, rn := NULL][]
acquisition.inferred = rbind(acquisition.inferred, single.months)

acquisition.inferred = acquisition.inferred %>% distinct(Panelist, platform, month, .keep_all = T)
acquisition.inferred$sub.inferred = 1

## Second metric

# Derivative Panel
panel.start = acquisition.inferred %>% group_by(Panelist) %>% 
  summarise(min.month = min(month), 
            max.month = max(month))

panel.total = setDT(panel.start)[, .(Panelist, month = seq(min.month, max.month, by = "month")),by = .(rn = 1:nrow(panel.start))][, rn := NULL][]

# Consider all platforms
platforms.to.include = unique(panel$platform[panel$subscription != 0])

# Create subscription panel with all platforms listed for each panelist-month
panel.total = setDT(panel.total)[, .(platform = platforms.to.include), by = c("Panelist", "month")]

# Get all Subscriptions throught Survey, and other variables.
subscription.active = wide_data %>% ungroup() %>% 
  filter(subscription != 0) %>% distinct(Panelist, month, subscription, .keep_all = T) %>% 
  mutate(sub = 1) %>% distinct(Panelist, month, subscription, .keep_all = T)

# Merge subscriptions to total panel
panel.subscriptions = left_join(panel.total, subscription.active, by = c("Panelist", "month", "platform"))

# If sub is NA, then must be 0, because this panel suggest subscription is only when survey accounts for it
panel.deriv = panel.subscriptions %>% mutate(sub = ifelse(is.na(sub), 0, sub))
rm(panel.subscriptions)
rm(panel.total)
gc()
panel.deriv = panel.deriv %>% group_by(Panelist, month, platform) %>% arrange(desc(sub)) %>% distinct(Panelist, month, platform, .keep_all = T)

## MERGE PANELS

panel.deriv$month = as.Date(panel.deriv$month)                                                                                                                                                
panel.compare = left_join(panel.deriv, acquisition.inferred, by = c("Panelist", "platform", "month"))
panel.compare$sub.inferred[is.na(panel.compare$sub.inferred)] = 0
panel.compare = left_join(panel.compare, panel %>% distinct(Panelist, month), by = c("Panelist", "month"))

panel.compare = panel.compare %>% rename(sub.CURRENT = sub, sub.BAND = sub.inferred)

panel.with.Weights = left_join(panel.compare, panel.weights, by = c("Panelist", "month"))


## Other variables
panel.with.Weights = panel.with.Weights %>% group_by(Panelist, platform) %>% arrange(month) %>% 
  mutate(AddedSubscription = as.numeric(lag(sub.BAND) != 1 & sub.BAND == 1),
         AddedSubscription = ifelse(is.na(AddedSubscription), 1, AddedSubscription)) %>% 
  mutate(SubscriptionTime = cumsum(AddedSubscription)) %>% 
  group_by(Panelist, platform, SubscriptionTime) %>% 
  mutate(SubDurationCurrent_SubscriptionTime = cumsum(sub.BAND)) %>%
  mutate(SubDurationTotal_SubscriptionTime = max(SubDurationCurrent_SubscriptionTime))



fwrite(panel.with.Weights, paste("../../gen/preparation/temp/panelSubscription_", paste(arg),".csv", sep = ""))

#=========================================================

# done
finished = list("done!")
fwrite(finished, "../../gen/preparation/temp/sub.txt")