#######################################################################
#####Analysis of drought experiment. Fungal part. This is also part
#####of the thesis of Daniel and Christian
########################################################################

#loading packages


library(tidyverse)



#loading file

BiomassArea<-read.csv("Biomass_Area.csv",header = TRUE, stringsAsFactors = FALSE)

#changing comma to decimal point
BiomassArea$radius1_cm<-sub("\\,","\\.",BiomassArea$radius1_cm)
BiomassArea$radius1_cm<-as.numeric(BiomassArea$radius1_cm)

BiomassArea$radius2_cm<-sub("\\,","\\.",BiomassArea$radius2_cm)
BiomassArea$radius2_cm<-as.numeric(BiomassArea$radius2_cm)

BiomassArea[is.na(BiomassArea$radius2_cm),6]<-BiomassArea[is.na(BiomassArea$radius2_cm),5]


#calculating the area of the colony

BiomassArea$Area_cm2<-pi*(BiomassArea$radius1_cm)*(BiomassArea$radius2_cm)

BiomassArea$Treatment<-NA
BiomassArea$Treatment[1:60]<-"W"
BiomassArea$Treatment[61:157]<-"D"

#correcting the area of the colony P73-28 the data entry is the average of a single colony out 10 colonies (more ore less)
BiomassArea[17,7]<-BiomassArea[17,7]*10

BiomassArea$Density_mg_cm2<-BiomassArea$weight_netto_mg/
                      BiomassArea$Area_cm2

#length(BiomassArea$Treatment)
#names(BiomassArea)
# BiomassArea$Treatment<-NULL
names(BiomassArea)
BiomassArea[104,8]<-"weight calculated from one quarter of the colony"

######

ggplot(data=BiomassArea,
       aes(x=Treatment, y=weight_netto_mg, fill=Treatment)) +
  geom_boxplot()

ggplot(data=BiomassArea,
       aes(x=Treatment, y=Density_mg_cm2, fill=Treatment)) +
  geom_boxplot()


ggplot(data = BiomassArea)+
  geom_point(mapping = aes(x=Area_cm2,
                           y=Density_mg_cm2,colour=Treatment))+
  #ggtitle("Relationship colony area and spore area")+
  labs(x="Colony area (cm^2)",y="Dichte")

BiomassArea[order(BiomassArea$Density_mg_cm2),c(1,7,9,10)]
names(BiomassArea)
