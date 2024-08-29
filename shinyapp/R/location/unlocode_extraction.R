# 
# 
# library(readr)
# library(stringr)
# library(tidyverse)
# 
# # -- parameters
# filename <- "2023-2 UNLOCODE CodeListPart1.csv"
# path <- "E:/Portfolio/R/Projects/the-world/shinyapp/resources"
# 
# # -- read file
# unlocode <- read_csv(file.path(path, filename), 
#                      col_names = FALSE, locale = locale(encoding = "ISO-8859-1"))
# 
# # -- slice & rename columns
# unlocode <- unlocode[c(2:5, 7, 10, 11)]
# colnames(unlocode) <- c("country", "locode", "name", "name_wo_accent", "function", "iata", "coordinates")
# 
# # -- split function column
# unlocode[c("port", "rail", "road", "airport", "postal", "multimodal", "fixed", "border")] <- str_split(unlocode$`function`, "", simplify = T) %>% 
#   data.frame
# 
# # -- filter out NA (empty rows with country name)
# unlocode <- unlocode[!is.na(unlocode$locode), ]
# unlocode <- unlocode[!is.na(unlocode$coordinates), ]
# 
# # -- one hot encoding
# unlocode['port'] <- ifelse(unlocode$port == "-", FALSE, TRUE)
# unlocode['rail'] <- ifelse(unlocode$rail == "-", FALSE, TRUE)
# unlocode['road'] <- ifelse(unlocode$road == "-", FALSE, TRUE)
# unlocode['airport'] <- ifelse(unlocode$airport == "-", FALSE, TRUE)
# unlocode['postal'] <- ifelse(unlocode$postal == "-", FALSE, TRUE)
# unlocode['multimodal'] <- ifelse(unlocode$multimodal == "-", FALSE, TRUE)
# unlocode['fixed'] <- ifelse(unlocode$fixed == "-", FALSE, TRUE)
# unlocode['border'] <- ifelse(unlocode$border == "-", FALSE, TRUE)
# 
# # -- coordinates
# # issue: one location is provided per locode while the entry can be tagged as port, airport, railway
# # example AUH is in the middle of the city while aiport is outside...
# # 
