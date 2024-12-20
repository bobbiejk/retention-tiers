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

panel = fread(paste("../../gen/preparation/temp/panelSubscription_", paste(arg),".csv", sep = ""))


#=========================================================
# Create dataframe
#=========================================================

df = panel %>% select(Panelist, month, weight2, platform, sub.CURRENT, sub.BAND, SubscriptionTime)


#=========================================================
# Plan Selection and Other Information (price, sharing)
#=========================================================

fill.in.missings = function(data, var){
  
  final.data = data %>% group_by(Panelist, platform, SubscriptionTime) %>%
    mutate(!!var := ifelse(is.na(!!sym(var)) & sub.BAND == 1, nafill(!!sym(var), "locf"), !!sym(var))) %>%
    mutate(!!var := ifelse(is.na(!!sym(var)) & sub.BAND == 1, nafill(!!sym(var), "nocb"), !!sym(var)))
  
  return(final.data)
}


# Ad-Supported Tier Variables
df$AdTier = ifelse(panel$`Subcription with Advertisements` == "Ad Free", 0, NA)
df$AdTier = ifelse(panel$`Subcription with Advertisements` == "Don't know", NA, df$AdTier)
df$AdTier = ifelse(panel$`Subcription with Advertisements` == "Ad Supported", 1, df$AdTier)

df = df %>% group_by(Panelist, platform, SubscriptionTime) %>%
  mutate(AdTier := ifelse(is.na(AdTier) & sub.BAND == 1, nafill(AdTier, "locf"), AdTier)) %>%
  mutate(AdTier := ifelse(is.na(AdTier) & sub.BAND == 1, nafill(AdTier, "nocb"), AdTier)) %>% ungroup()

# Netflix advertising tier adding
netflix_date = as.Date("2022-11-01")

# Disney advertising tier adding
disney_date = as.Date("2022-12-01")

# Amazon advertising tier
amazon_date = as.Date("2024-01-01")

# Consider launch
df = df %>% mutate(AdTier = ifelse(platform == "Netflix" & month < netflix_date & AdTier == 1, 0, AdTier), # Netflix Ad Tier can only start at netflix_date
                   AdTier = ifelse(platform == "Disney+" & month < disney_date & AdTier == 1, 0, AdTier), # Disney Ad Tier can only start at disney_date
                   AdTier = ifelse(platform == "Amazon Prime Video" & month < amazon_date & AdTier == 1, 0, AdTier)) # Amazon Ad Tier can only start at amazon_date


# Price Paid
df$Price = as.numeric(panel$`Subscription Price Paid for`)
df = fill.in.missings(df, "Price")

# Price AVG per month
df$PriceAVG = as.numeric(panel$`Subscription Price Paid AvgPerMonth`)
df = fill.in.missings(df, "PriceAVG")

# Payment Frequency
df$PaymentYearly = ifelse(panel$`Subscription Payment Frequency` == "Once a year", 1, NA)
df$PaymentWeekly = ifelse(panel$`Subscription Payment Frequency` == "Every week", 1, NA)
df$PaymentBiweekly = ifelse(panel$`Subscription Payment Frequency` == "Every 2 weeks", 1, NA)
df$PaymentMonthly = ifelse(panel$`Subscription Payment Frequency` == "Every month", 1, NA)

df = fill.in.missings(df, "PaymentYearly")
df = fill.in.missings(df, "PaymentWeekly")
df = fill.in.missings(df, "PaymentBiweekly")
df = fill.in.missings(df, "PaymentMonthly")

df$PaymentYearly = ifelse(is.na(df$PaymentYearly), 0, df$PaymentYearly)
df$PaymentWeekly = ifelse(is.na(df$PaymentWeekly), 0, df$PaymentWeekly)
df$PaymentBiweekly = ifelse(is.na(df$PaymentBiweekly), 0, df$PaymentBiweekly)
df$PaymentMonthly = ifelse(is.na(df$PaymentMonthly), 0, df$PaymentMonthly)

# Subscription Type
df$FreeTrial = ifelse(panel$`Subscription Type` == "Free Trial", 1, NA)
df$SubscriptionFree = ifelse(panel$`Subscription Type` == "Free Version", 1, NA)
df$SubscriptionPaid = ifelse(panel$`Subscription Type` == "I pay/share cost", 1, NA)
df$SubscriptionShared = ifelse(panel$`Subscription Type` == "Share cost", 1, NA)
df$SubscriptionElsePaid = ifelse(panel$`Subscription Type` == "Someone else pays", 1, NA)

df = fill.in.missings(df, "FreeTrial")
df = fill.in.missings(df, "SubscriptionFree")
df = fill.in.missings(df, "SubscriptionPaid")
df = fill.in.missings(df, "SubscriptionShared")
df = fill.in.missings(df, "SubscriptionElsePaid")

df$FreeTrial = ifelse(is.na(df$FreeTrial), 0, df$FreeTrial)
df$SubscriptionFree = ifelse(is.na(df$SubscriptionFree), 0, df$SubscriptionFree)
df$SubscriptionPaid = ifelse(is.na(df$SubscriptionPaid), 0, df$SubscriptionPaid)
df$SubscriptionShared = ifelse(is.na(df$SubscriptionShared), 0, df$SubscriptionShared)
df$SubscriptionElsePaid = ifelse(is.na(df$SubscriptionElsePaid), 0, df$SubscriptionElsePaid)

# Shared Subscription Type
df$SubscriptionSharedOutsideHousehold = ifelse(panel$`Subscription Type Share` == "Outside my household", 1, NA)
df$SubscriptionSharedWithinHousehold = ifelse(panel$`Subscription Type Share` == "In the same household", 1, NA)
df$SubscriptionSharedInAndOutHousehold = ifelse(panel$`Subscription Type Share` == "Both of these", 1, NA)

df = fill.in.missings(df, "SubscriptionSharedOutsideHousehold")
df = fill.in.missings(df, "SubscriptionSharedWithinHousehold")
df = fill.in.missings(df, "SubscriptionSharedInAndOutHousehold")

df$SubscriptionSharedOutsideHousehold = ifelse(is.na(df$SubscriptionSharedOutsideHousehold), 0, df$SubscriptionSharedOutsideHousehold)
df$SubscriptionSharedWithinHousehold = ifelse(is.na(df$SubscriptionSharedWithinHousehold), 0, df$SubscriptionSharedWithinHousehold)
df$SubscriptionSharedInAndOutHousehold = ifelse(is.na(df$SubscriptionSharedInAndOutHousehold), 0, df$SubscriptionSharedInAndOutHousehold)

#=========================================================
# Merge Data
#=========================================================

not.included = panel %>% select(-c(`Subcription with Advertisements`, `Subscription Price Paid AvgPerMonth`, 
                                   `Subscription Payment Frequency`, `Subscription Type`, `Subscription Type Share`))

df = left_join(df, not.included, by = c("Panelist", "month", "weight2", "platform", "sub.CURRENT", "sub.BAND", "SubscriptionTime"))

#=========================================================
# Save Data
#=========================================================

fwrite(df, paste("../../gen/preparation/temp/panelSubscriptionPlan_", paste(arg),".csv", sep = ""))

# done
finished = list("done!")
fwrite(finished, "../../gen/preparation/temp/subplan.txt")
