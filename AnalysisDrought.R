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
