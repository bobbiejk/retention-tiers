library(data.table)
library(dplyr)
library(lubridate)


# read ilubridate# read in content
content = fread("../../data/streaming_libraries.csv")
content = content %>% filter(collection != "3-contract")
content = content %>% rename(imdb_id = tconst)

# extract original content
original = content %>% filter(original == 1)
original = original %>% distinct(imdb_id, service, month, .keep_all = T)

# extract inflow of titles
inflow.season = fread("../../data/titles_newseason.csv")
inflow.season = inflow.season %>% select(imdb_id, month, season_no) %>% mutate(newseason = 1)
inflow.season$month = as.Date(inflow.season$month)
original$month = as.Date(original$month)
inflow.season = left_join(original, inflow.season, by = c("imdb_id", "month"))
inflow.season = inflow.season %>% filter(original == 1 & newseason == 1)%>% distinct(imdb_id, service, month, .keep_all = T)
inflow.season3 = inflow.season
inflow.season1 = inflow.season %>% group_by(imdb_id, service, season_no) %>% 
  mutate(withinID = 1:n()) %>% filter(withinID < 2)

inflow.original.only3 = original %>% distinct(imdb_id, service, month, .keep_all = T) %>%
  group_by(imdb_id) %>% arrange(month) %>%
  mutate(withinID = 1:n()) %>% filter(withinID < 4) # season new + movie (3months)

inflow.original.only1 = original %>% distinct(imdb_id, service, month, .keep_all = T) %>%
  group_by(imdb_id) %>% arrange(month) %>%
  mutate(withinID = 1:n()) %>% filter(withinID < 2) # season new + movie (3months)

original.inflow3 = rbind(inflow.season3 %>% select(imdb_id, service, month), inflow.original.only3 %>% select(imdb_id, service, month))
original.inflow3 = original.inflow3 %>% distinct(imdb_id, service, month, .keep_all = T)

original.inflow1 = rbind(inflow.season1 %>% select(imdb_id, service, month), inflow.original.only1 %>% select(imdb_id, service, month))
original.inflow1 = original.inflow1 %>% distinct(imdb_id, service, month, .keep_all = T)

content.inflow1 = content %>% arrange(day) %>% select(imdb_id, service, month) %>% distinct(imdb_id, service, .keep_all = T)

# link characteristics to original and content
merge_char = function(data){
  
  merged = as.data.table(informative)[as.data.table(data), on = c("imdb_id"), allow.cartesian = T]
  merged = as.data.table(family)[as.data.table(merged), on = c("imdb_id"), allow.cartesian = T]
  merged = as.data.table(based)[as.data.table(merged), on = c("imdb_id"), allow.cartesian = T]
  merged = as.data.table(prod)[as.data.table(merged), on = c("imdb_id"), allow.cartesian = T]
  merged = as.data.table(reboot)[as.data.table(merged), on = c("imdb_id"), allow.cartesian = T]
  merged = as.data.table(cinema)[as.data.table(merged), on = c("imdb_id"), allow.cartesian = T]
  merged = as.data.table(season)[as.data.table(merged), on = c("imdb_id", "month"), allow.cartesian = T]
  merged = as.data.table(awrd)[as.data.table(merged), on = c("imdb_id"), allow.cartesian = T]
  merged$award = ifelse(merged$month >= merged$awards.month, 1, 0)
  merged = merged %>% select(-awards.month)
  merged = as.data.table(actr)[as.data.table(merged), on = c("imdb_id"), allow.cartesian = T]
  merged = as.data.table(breadth.season)[as.data.table(merged), on = c("imdb_id", "month")]
  
  return(merged)
}

agg_char = function(merged){
  aggregate = merged %>% group_by(month, service) %>% distinct(imdb_id, .keep_all = T) %>%
    summarise(assortment.content.nr.titles = length(unique(imdb_id)),
              assortment.content.nr.informative = sum(informative, na.rm = T),
              assortment.content.nr.family = sum(family, na.rm = T),
              assortment.content.nr.based = sum(based.on, na.rm = T),
              assortment.content.nr.season = sum(newseason, na.rm = T),
              assortment.content.nr.cinema = sum(cinema, na.rm = T),
              assortment.content.nr.reboot = sum(reboot, na.rm = T),
              assortment.content.nr.award = sum(award, na.rm = T),
              assortment.content.nr.actor = sum(actors, na.rm = T)
    )
  
  return(aggregate)
}



# characteristics to consider
informative = fread("../../data/titles_informative.csv")
family = fread("../../data/titles_family.csv")
based = fread("../../data/titles_based.csv")
season = fread("../../data/titles_newseason.csv")
season = season %>% select(imdb_id, month)
season$newseason = 1
cinema = fread("../../data/titles_cinema.csv")
reboot = fread("../../data/titles_reboot.csv")
awrd = fread("../../data/titles_awards.csv") %>% filter(month > as.Date("1980-01-01"))
colnames(awrd) = c("imdb_id", "awards.month", "award")
actr = fread("../../data/titles_actors.csv")
colnames(actr) = c("imdb_id", "actors")
breadth.season = fread("../../data/season_data.csv")
colnames(breadth.season) = c("imdb_id", "season_no", "month")
prod = fread("../../data/titles_production.csv")



# merge characteristics
merge_flow1 = merge_char(original.inflow1)
merge_content_flow1 = merge_char(content.inflow1)

content_Aggregated_flow1 = agg_char(merge_content_flow1)
content_Aggregated_stock = agg_char(merge_char(content %>% distinct(imdb_id, service, month)))
original_Aggregated_flow1 = agg_char(merge_flow1)
original_Aggregated_stock = agg_char(merge_char(original))

colnames(original_Aggregated_flow1) = gsub("content", "original.flow1", colnames(original_Aggregated_flow1))
colnames(original_Aggregated_stock) = gsub("content", "original.stock", colnames(original_Aggregated_stock))
colnames(content_Aggregated_flow1) = gsub("content", "content.flow1", colnames(content_Aggregated_flow1))
colnames(content_Aggregated_stock) = gsub("content", "content.stock", colnames(content_Aggregated_stock))
                                

# describe the characteristics
correct_order = function(df){
  columns <- colnames(df)
  prefixes <- unique(gsub("(_mean|_sd)$", "", columns))
  new_order <- unlist(lapply(prefixes, function(prefix) {
    grep(prefix, columns, value = TRUE)
  }))
  
  df_reordered <- df %>%
    select(all_of(new_order))
  return(df_reordered)
}

meanSD_flow = original_Aggregated_flow1 %>% group_by(service) %>% filter(month >= as.Date("2022-01-01")) %>%
  summarise_at(vars(grep("content", colnames(original_Aggregated_flow1))-1),
               funs(mean, sd))
meanSD_flow = correct_order(meanSD_flow)

meanSD_stock = original_Aggregated_stock %>% group_by(service) %>% filter(month >= as.Date("2022-01-01")) %>%
    summarise_at(vars(grep("content", colnames(original_Aggregated_stock))-1),
                 funs(mean, sd))
meanSD_stock = correct_order(meanSD_stock)

meanSD_flow_content = content_Aggregated_flow1 %>% group_by(service) %>% filter(month >= as.Date("2022-01-01")) %>%
  summarise_at(vars(grep("content", colnames(content_Aggregated_flow1))-1),
               funs(mean, sd))

meanSD_stock_content = content_Aggregated_stock %>% group_by(service) %>% filter(month >= as.Date("2022-01-01")) %>%
  summarise_at(vars(grep("content", colnames(content_Aggregated_stock))-1),
               funs(mean, sd))

# consider the flow variables of the last three months
get_last_3_months_summary <- function(dt, current_month, serviceID) {
  start_date <- as.Date(format(current_month, "%Y-%m-01")) - months(2)
  
  # Filter data for the last 3 months and get unique product_IDs
  unique_products <- unique(dt[month >= start_date & month <= current_month & serviceID == service, .(imdb_id, informative, family, based.on, newseason, cinema, reboot, award, actors)], by = "imdb_id")
  
  
  # Calculate summary statistics for the dummy variables
  dummy_summary <- unique_products[, .(
    assortment.content.nr.titles = .N,
    assortment.content.nr.informative = sum(informative, na.rm = T),
    assortment.content.nr.family = sum(family, na.rm = T),
    assortment.content.nr.based = sum(based.on, na.rm = T),
    assortment.content.nr.season = sum(newseason, na.rm = T),
    assortment.content.nr.cinema = sum(cinema, na.rm = T),
    assortment.content.nr.reboot = sum(reboot, na.rm = T),
    assortment.content.nr.award = sum(award, na.rm = T),
    assortment.content.nr.actor = sum(actors, na.rm = T)
  )]
  
  return(dummy_summary)
}

# Apply the function to each month and combine results
original_Aggregated_flow3 = merge_flow1[, get_last_3_months_summary(merge_flow1, month, service), by = .(month, service)]
colnames(original_Aggregated_flow3) = gsub("content", "original.flow3", colnames(original_Aggregated_flow3))

content_Aggregated_flow3 = merge_content_flow1[, get_last_3_months_summary(merge_content_flow1, month, service), by = .(month, service)]
colnames(content_Aggregated_flow3) = gsub("content", "content.flow3", colnames(content_Aggregated_flow3))

# Merge the Data Frames
content_Aggregated_stock$month = as.Date(content_Aggregated_stock$month)
content_Aggregated_flow1$month = as.Date(content_Aggregated_flow1$month)
content_Aggregated_flow3$month = as.Date(content_Aggregated_flow3$month)
original_Aggregated_stock$month = as.Date(original_Aggregated_stock$month)
original_Aggregated_flow1$month = as.Date(original_Aggregated_flow1$month)
original_Aggregated_flow3$month = as.Date(original_Aggregated_flow3$month)

final.df = left_join(original_Aggregated_flow1, original_Aggregated_flow3, by = c("month", "service"))
final.df = left_join(final.df, content_Aggregated_flow1, by = c("month", "service"))
final.df = left_join(final.df, content_Aggregated_flow3, by = c("month", "service"))
final.df = left_join(final.df, content_Aggregated_stock, by = c("month", "service"))
final.df = left_join(original_Aggregated_stock, final.df, by = c("month", "service"))

final.df = final.df %>% filter(month >= as.Date("2022-01-01"))
final.df[is.na(final.df)] = 0


dir.create("../../gen/preparation/temp/", recursive = T)
fwrite(final.df, "../../gen/preparation/temp/contentData.csv")
s