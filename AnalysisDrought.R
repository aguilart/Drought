#######################################################################
#####Analysis of drought experiment. Fungal part. This is also part
#####of the thesis of Daniel and Christian
########################################################################

#loading packages

library(tidyverse)


#loading files

#Biomass and area data
BiomassArea<-read.csv("BiomassArea_undMetaData/Biomass_Area.csv",
                      header = TRUE, stringsAsFactors = FALSE)

              #changing comma to decimal point
              BiomassArea$radius1_cm<-sub("\\,","\\.",BiomassArea$radius1_cm)
              BiomassArea$radius1_cm<-as.numeric(BiomassArea$radius1_cm)
              
              BiomassArea$radius2_cm<-sub("\\,","\\.",BiomassArea$radius2_cm)
              BiomassArea$radius2_cm<-as.numeric(BiomassArea$radius2_cm)
              
              BiomassArea[is.na(BiomassArea$radius2_cm),6]<-BiomassArea[is.na(BiomassArea$radius2_cm),5]
              
              #calculating the area of the colony
              BiomassArea$Area_cm2<-pi*(BiomassArea$radius1_cm)*(BiomassArea$radius2_cm)
              
              #creating a new column with for treatement W= wet, D= drought
              BiomassArea$Treatment<-NA
              BiomassArea$Treatment[1:60]<-"W"
              BiomassArea$Treatment[61:157]<-"D"
              
              #correcting the area of the colony P73-28 the data entry is the average of a single colony out 10 colonies (more ore less)
              BiomassArea[17,7]<-BiomassArea[17,7]*10
              
              BiomassArea$Density_mg_cm2<-BiomassArea$weight_netto_mg/
                BiomassArea$Area_cm2
              
              BiomassArea$Area_cm2<-pi*(BiomassArea$radius1_cm)*(BiomassArea$radius2_cm)
              
             #Other changes
              names(BiomassArea)
              BiomassArea[104,8]<-"weight calculated from one quarter of the colony"
              
         
#loading culturing date data
CulturingDate<-read.csv("BiomassArea_undMetaData/Fungi_Datum.csv",
                    header = TRUE,sep = ";",stringsAsFactors = FALSE)

#loading harvest date data
HarvestDate<-read.csv("BiomassArea_undMetaData/HarvestDay.csv",
         header = TRUE,sep=";",stringsAsFactors = FALSE)

#loading plate number and position
PlateNumber<-read.csv("BiomassArea_undMetaData/MicroRespPlatePositionID.csv",
                      header = TRUE,sep=";",stringsAsFactors = FALSE)

              #just to fill the empty spaces with correct ID´s
              PlateNumber[PlateNumber$Position=="B",2]<-PlateNumber[PlateNumber$Position=="A",2]
              PlateNumber[PlateNumber$Position=="D",2]<-PlateNumber[PlateNumber$Position=="C",2]
              PlateNumber[PlateNumber$Position=="F",2]<-PlateNumber[PlateNumber$Position=="E",2]
              PlateNumber[PlateNumber$Position=="H",2]<-PlateNumber[PlateNumber$Position=="G",2]
              PlateNumber<-PlateNumber[1:240,]

              #adding the harvest date to plate number
              PlateNumber<-left_join(PlateNumber,HarvestDate)
                            PlateNumber$ID<-sub("^P","p",PlateNumber$ID)
              
                            AllData$ID<-sub("-0","-",AllData$ID)

#merging all the info in one big dataset called "AllData"(to this point everything but microresp data)
AllData<-left_join(BiomassArea,CulturingDate)#biomass and culturing date
          AllData$HarvestDate<-NULL

          AllData<-left_join(AllData,PlateNumber)#biomass + the plate number and harvest date

#loading the data for the microresp
#decolorations<-
      #First we load all raw data, this code creates a list with all readings, both at t=0 and t=6
      temp<-list.files(pattern = "*.csv")
      microrespfiles<-lapply(temp,read.csv,header = FALSE,sep=",")
      #giving names to the list
      names(microrespfiles)<-temp
      
      #some files are separated by ";" instead of ",". Thus I wrote this to make all files separated by ","
      micro2<-lapply(temp,read.csv,header = FALSE,sep=";")
      names(micro2)<-temp
      microrespfiles[which(lapply(microrespfiles,length)==1)]<-micro2[which(lapply(micro2,length)==12)]
      rm(micro2)
    
      #this might not be necessary, I just convert each element in the list into a matrix
      #microrespfiles<-lapply(microrespfiles,as.matrix)
    
      #now I need to calculate the percentage of decoloration, following the microresp manual
      #This consists of dividing the corresponding plates t6/t0
      O<-seq(1,59,2)
      E<-seq(2,60,2)
    
      prueba<-list(0)
      for(i in 1:59){
        prueba[[i]]<-microrespfiles[[i+1]]/microrespfiles[[i]]
        }
      
      decolorations<-prueba[O]
      names(decolorations)<-paste(names(microrespfiles[E]),"/",names(microrespfiles[O]),sep="")
      
      #Just testing wheter the code is doing the appropriate calculations
      decolorations$`MicroResp_Fun_P18_t6.csv/MicroResp_Fun_P18_t0.csv`==
      microrespfiles$MicroResp_Fun_P18_t6.csv/microrespfiles$MicroResp_Fun_P18_t0.csv

      #merging all the dataframes into one
      decolorations<-do.call(rbind,decolorations)
      #extracting the names of the plates in exactly the same order they are organized in R
      plates<-sapply(strsplit(names(microrespfiles[O]),"_"),function(x)x[3])
      
      #addint the names of the plates
      decolorations$Plate_nr<-rep(plates, each = 8)
      decolorations$Position<-rep(letters[1:8],30)
      rownames(decolorations)<-NULL
      
      names(decolorations)[1:12]<-c("H2O","H2O_Fung","Xylan","Cellulose","Proline",
                                    "Arginin","Oxalic_acid","Malic_acid","Citric_acid",
                                    "Arabinose","Galactose","Glucose")

#data visualization      
decolorations[decolorations$Plate_nr=="P1",]%>%
  ggplot(aes(Position,Cellulose))+
  geom_bar(stat = "identity")



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
