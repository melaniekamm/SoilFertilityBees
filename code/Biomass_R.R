library(tidyverse); library(ggplot2)

rawdata <- read.csv("C:/Users/bryce/Box Sync/BryceWork/Bryce Thesis/Data/Excel Sheets/Buck_Biomass.csv")
summary(rawdata)

rawdata$Soil_Seed_Type <- as.factor(paste(rawdata$Soil_Treatment, rawdata$Seed._Type, sep="_"))
rawdata$Species <- as.factor(substr(rawdata$Seed._Type, start=1, stop=1))

pea <- filter(rawdata, Species == "P")
coreo <- filter(rawdata, Species == "C")


#make graph for coreo
p <- ggplot(coreo, aes(Soil_Treatment, Dry_Biomass_.g., fill=Soil_Treatment))
p + geom_boxplot(show.legend = T)+ theme(axis.text.x = element_blank()) +
  labs(x = "Soil  Treatment", y = "Dry  Biomass  (g)- Coreopsis")+ theme (axis.title = element_text(size = 24)) +
  scale_fill_manual(values = c("#636363", "grey", "#762a83", "#9970ab", "#c2a5cf", "#1b7837", "#5aae61", "#a6dba0" ))

#make graph for pea
p <- ggplot(pea, aes(Soil_Treatment, Dry_Biomass_.g., fill=Soil_Treatment))
p + geom_boxplot(show.legend = T)+ theme(axis.text.x = element_blank()) +
  labs(x = "Soil  Treatment", y = "Dry  Biomass  (g)- Pea") + theme (axis.title = element_text(size = 24)) + 
  scale_fill_manual(values = c("#636363", "grey", "#762a83", "#9970ab", "#c2a5cf", "#1b7837", "#5aae61", "#a6dba0" ))