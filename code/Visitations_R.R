library(tidyverse); library(ggplot2)
library(dplyr)

pea <- read.csv("C:/Users/bryce/Box Sync/BryceWork/Bryce Thesis/Data/Excel Sheets/csv_data/Buck_Visitation_Peas.csv")
pea$Date <- as.Date(pea$Date, format="%m/%d/%Y")

pea <- mutate(pea, DateNew=as.Date(gsub(Date, pattern='2018-07-30', replacement='2018-07-29'))) %>%
  select(-Date)

cor <- read.csv("C:/Users/bryce/Box Sync/BryceWork/Bryce Thesis/Data/Excel Sheets/csv_data/Buck_Visitation_Coreopsis.csv")
cor$Date <- as.Date(cor$Date, format="%m/%d/%Y")
cor <- mutate(cor, DateNew=as.Date(gsub(gsub(Date, pattern='2018-08-09', replacement='2018-08-10'), pattern='2018-08-19', replacement='2018-08-17'))) %>%
  select(-Date)

flowers <- read.csv("C:/Users/bryce/Box Sync/BryceWork/Bryce Thesis/Data/Excel Sheets/csv_data/Buck_FlowerNumber.csv")

#remove empty columns beyond last date column
flowers <- flowers[,1:11]

#reformat data frame so there is one 'date' column to use for plotting

longflowers <- gather(flowers, key=date , value=flowers, -SoilTreatment, -SeedType)

longflowers$date <- substr(longflowers$date, start=2, stop=20) %>%
  as.Date(format="%m.%d.%y")

peaflowers <- filter(longflowers, str_detect(SeedType, "P")) %>%
  mutate(DateNew=as.Date(gsub(date, pattern='2018-07-27', replacement='2018-07-29')))

pea_merged <- select(peaflowers, -date) %>%
  right_join(pea) %>%
  gather(key=Taxa, value=NVisits, Hbee, Bumble, OtherBeeP, OtherBeesN) %>%
  mutate(NVisitsFlower = NVisits/flowers) %>%
  dplyr::filter(Taxa %in% c('Bumble', 'OtherBeeP', 'OtherBeesN') & Time == 'AM') %>%
  dplyr::mutate(Taxa = if_else(Taxa %in% c('OtherBeeP', 'OtherBeesN'), 'OtherBee', Taxa))
  
pea_merged$NVisitsFlower[pea_merged$NVisitsFlower == Inf | is.na(pea_merged$NVisitsFlower)] <- 0

pea_merged <- dplyr::group_by(pea_merged, SoilTreatment, Taxa)

pea_graph  <- dplyr::summarize(pea_merged, MeanVisitsFlower=mean(NVisitsFlower, na.rm=T))
pea_graph$SoilTreatment <- gsub(pea_graph$SoilTreatment, pattern="_", replacement=" ")

g <- ggplot(pea_graph, aes(SoilTreatment, MeanVisitsFlower, fill=Taxa)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Soil  Treatment", y = "Mean  AM  Bee  Visits  Pea (per  Flower)") + theme (axis.title = element_text(size = 13
                                                                                                                     )) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00"))
g + geom_col()


corflowers <- filter(longflowers, str_detect(SeedType, "C")) %>%
  mutate(DateNew=date)

cor_merged <- select(corflowers, -date) %>%
  right_join(cor) %>%
  gather(key=Taxa, value=NVisits, Hbee, Bumble, OtherBee, Flies, Wasps, Leps) %>%
  mutate(NVisitsFlower = NVisits/flowers) %>%
  dplyr::filter(Taxa %in% c('Bumble', 'OtherBee') & Time == 'AM') %>%
  group_by(SoilTreatment, Taxa)

cor_merged$NVisitsFlower[cor_merged$NVisitsFlower == Inf | is.na(cor_merged$NVisits)] <- 0

cor_graph  <- dplyr::summarize(cor_merged, MeanVisitsFlower=mean(NVisitsFlower, na.rm=T))
cor_graph$SoilTreatment <- gsub(cor_graph$SoilTreatment, pattern="_", replacement=" ")

g <- ggplot(cor_graph, aes(SoilTreatment, MeanVisitsFlower, fill=Taxa)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Soil  Treatment", y = "Mean  AM  Bee  Visits  to  Coreopsis (per  Flower)") + theme (axis.title = element_text(size = 24)) +
  theme (axis.title = element_text(size = 24)) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00"))
g + geom_col()

#theme(legend.position = "bottom")