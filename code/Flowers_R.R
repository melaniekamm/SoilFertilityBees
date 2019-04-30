#make plot of flower number over time for different soil treatments
flowers <- read.csv("C:/Users/bryce/Box Sync/BryceWork/Bryce Thesis/Data/Excel Sheets/csv_data/Buck_FlowerNumber.csv")

#remove empty columns beyond last date column
flowers <- flowers[,1:11]

#reformat data frame so there is one 'date' column to use for plotting
longflowers <- gather(flowers, key=date , value=flowers, -SoilTreatment, -SeedType)

longflowers$date <- substr(longflowers$date, start=2, stop=10) %>%
  as.Date(format="%m.%d.%y")

foo <- data.frame(do.call('rbind', strsplit(as.character(longflowers$SoilTreatment),'_',fixed=TRUE)))
longflowers$HabType <- foo[,1]

#split pea and coreposis into two different data frames
library(plotrix)

#calculate average number flowers across replicates for each date
pea_flowers <- filter(longflowers, str_detect(SeedType, "P")) %>%
  group_by(HabType, SoilTreatment, date) %>%
  summarize(mean_flowers=mean(flowers, na.rm=T), sterror=std.error(flowers))

cor_flowers <- filter(longflowers, str_detect(SeedType, "C")) %>%
  group_by(HabType, SoilTreatment, date) %>%
  summarize(mean_flowers=mean(flowers, na.rm=T), sterror=std.error(flowers))

#make plots of number flowers for each species
#pea plot
pplot <- ggplot(data=pea_flowers, aes(x=date, y=mean_flowers, group=SoilTreatment)) +
  geom_line(aes(color=SoilTreatment)) + geom_point(aes(color=SoilTreatment), shape = 15, size =6) +
  labs(x = "Date", y = "Mean  #  of  Flowers- Coreopsis") + theme (axis.title = element_text(size = 24)) +
  scale_color_manual(values = c("#636363", "grey", "#762a83", "#9970ab", "#c2a5cf", "#1b7837", "#5aae61", "#a6dba0" ))
pplot

#coreo plot
cplot <- ggplot(data=cor_flowers, aes(x=date, y=mean_flowers, group=SoilTreatment)) +
  geom_line(aes(color=SoilTreatment)) + geom_point(aes(color=SoilTreatment), shape = 15, size =6) +
  labs(x = "Date", y = "Mean  #  of  Flowers- Pea") + theme (axis.title = element_text(size = 24)) +
  scale_color_manual(values = c("#636363", "grey", "#762a83", "#9970ab", "#c2a5cf", "#1b7837", "#5aae61", "#a6dba0" ))
cplot

 