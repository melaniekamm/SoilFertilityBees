library(tidyverse); library(ggplot2)

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
  as.Date(format="%m.%d.%y") %>%
  gsub(pattern="2020", replacement="2018")



peaflowers <- filter(longflowers, str_detect(SeedType, "P")) %>%
              mutate(DateNew=as.Date(gsub(date, pattern='2018-07-27', replacement='2018-07-29')))

pea_merged <- select(peaflowers, -date) %>%
              right_join(pea) %>%
              gather(key=Taxa, value=NVisits, Hbee, Bumble, OtherBeeP, OtherBeesN, FliesP, FliesN, Wasps, Leps) %>%
              mutate(NVisitsFlower = NVisits/flowers) %>%
              group_by(SoilTreatment, Time, Taxa)

pea_merged$NVisitsFlower[pea_merged$NVisitsFlower == Inf] <- NA
pea_graph  <- summarize(pea_merged, MeanVisitsFlower=mean(NVisitsFlower, na.rm=T))

pea_scatter <- select(peaflowers, -date) %>%
  right_join(pea) %>%
  gather(key=Taxa, value=NVisits, Hbee, Bumble, OtherBeeP, OtherBeesN, FliesP, FliesN, Wasps, Leps) %>%
  mutate(NVisitsFlower = NVisits/flowers) %>%
  filter(Time == 'AM' & Taxa %in% c('Hbee', 'Bumble', 'OtherBeeP', 'OtherBeeN')) %>%
  group_by(SoilTreatment)

pea_scatter$NVisitsFlower[pea_scatter$NVisitsFlower == Inf] <- NA

pea_scatter_graph  <- summarize(pea_scatter, MeanVisitsPea=mean(NVisitsFlower, na.rm=T))

g <- ggplot(pea_graph, aes(SoilTreatment, MeanVisitsFlower, fill=Taxa)) +
     facet_wrap(~Time) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g + geom_col()



longflowers <- gather(flowers, key=date , value=flowers, -SoilTreatment, -SeedType)

longflowers$date <- substr(longflowers$date, start=2, stop=20) %>%
  as.Date(format="%m.%d.%y") %>%
  gsub(pattern="2020", replacement="2018")


corflowers <- filter(longflowers, str_detect(SeedType, "C")) %>%
              mutate(DateNew=as.Date(date))

cor_merged <- select(corflowers, -date) %>%
  right_join(cor) %>%
  gather(key=Taxa, value=NVisits, Hbee, Bumble, OtherBee, Flies, Wasps, Leps) %>%
  mutate(NVisitsFlower = NVisits/flowers) %>%
  group_by(SoilTreatment, Time, Taxa)

cor_merged$NVisitsFlower[cor_merged$NVisitsFlower == Inf] <- NA

cor_graph  <- summarize(cor_merged, MeanVisitsFlower=mean(NVisitsFlower, na.rm=T))

g <- ggplot(cor_graph, aes(SoilTreatment, MeanVisitsFlower, fill=Taxa)) +
  facet_wrap(~Time) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g + geom_col()


cor_scatter <- select(corflowers, -date) %>%
  right_join(cor) %>%
  gather(key=Taxa, value=NVisits, Hbee, Bumble, OtherBee, Flies, Wasps, Leps) %>%
  mutate(NVisitsFlower = NVisits/flowers) %>%
  filter(Time == 'AM' & Taxa %in% c('Hbee', 'Bumble', 'OtherBee')) %>%
  group_by(SoilTreatment)

cor_scatter$NVisitsFlower[cor_scatter$NVisitsFlower == Inf] <- NA

cor_scatter_graph  <- summarize(cor_scatter, MeanVisitsCor=mean(NVisitsFlower, na.rm=T))


mean_visits <- full_join(cor_scatter_graph, pea_scatter_graph)


total_visits <- group_by(cor_scatter, SoilTreatment, SeedType) %>%
                summarize(TotalVisitsCor=sum(NVisitsFlower, na.rm=T))
