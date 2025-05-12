######install necessary libraries#####
install.packages("tidyverse")
install.packages("rcompanion")
install.packages("writexl")
install.packages("rstatix")
install.packages("ggpmisc")
install.packages("zoo")
install.packages("stats")
install.packages("Metrics")
install.packages('devtools')
install.packages('RSQLite')
install.packages("rcompanion")
install.packages('plyr')
install.packages('meta')
install.packages("ggh4x")
install.packages('ggpubr')
install.packages('EnvStats')
install.packages('ggvenn')
install.packages('ggVennDiagram')
install.packages('pheatmap') #pheatmap packages
install.packages('VennDiagram')
install.packages("pdftools")
install.packages("magick")
install.packages('scales')
install_github("dgrapov/CTSgetR")
install.packages('eulerr')
install.packages("readxl")
install.packages('svglite')

library(progress)
library(pheatmap)
library(scales)
library(VennDiagram)
library(pdftools)
library(magick)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(writexl)
library(rstatix)
library(tibble)
library(lmtest)
library(gridExtra)
library(grid)
library(dplyr)
library(svglite)
library(ggpubr)
library(tidyr)
library(stringr)
library(broom)
library(Metrics)
library(devtools)
library(gridExtra)
library(cowplot)
library(plyr)
library(naniar)
library(ggh4x)
library(EnvStats)
library(readr)
library(ggvenn)
library(ggVennDiagram)
library(readxl)
library(plotly)

remotes::install_github('aberHRML/classyfireR')
install.packages('classyfireR')
library(classyfireR)

devtools::install_github("selcukorkmaz/PubChemR")
library(PubChemR)

######1. Get internal standard and surrogate standard areas#######
##upload manual integration data##
#C18 Positive#
C18_Positive_Manual <- read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Manual Integrations/ContactLensNontarget_Quant_C18Positive_20250128.csv", col_types = cols(.default = "c"))
Cholined9_C18Pos <- C18_Positive_Manual %>% subset(select = c(1:4, 10)) #select columns containingsurrogate standard and internal standard
Cholined9_C18Pos[,'Type'] = NA #formatting so later function works#
Cholined9_C18Pos_reorder <- Cholined9_C18Pos[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

#C18 Negative#
C18_Negative_Manual <- read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Manual Integrations/ContactLensNontarget_Quant_C18Negative_20250128.csv", col_types = cols(.default = "c"))
Myristicd27_C18Neg <- C18_Negative_Manual %>% subset(select = c(1:2, 9:10, 12)) #select columns containingsurrogate standard and internal standard
Myristicd27_C18Neg[,'Type'] = NA #formatting so later function works#
Myristicd27_C18Neg_reorder <- Myristicd27_C18Neg[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

#Hilic Positive#
Hilic_Positive_Manual <-read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Manual Integrations/ContactLensNontarget_Quant_HILICPos_20250128.csv", col_types = cols(.default = "c"))
Cholined9_HilicPos <- Hilic_Positive_Manual %>% subset(select = c(1:2, 9:10, 26)) #select columns containingsurrogate standard and internal standard
Cholined9_HilicPos[,'Type'] = NA #formatting so later function works#
Cholined9_HilicPos_reorder <- Cholined9_HilicPos[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

MeHisd3_HilicPos <- Hilic_Positive_Manual %>% subset(select = c(1:2, 33:34, 26)) #select columns containingsurrogate standard and internal standard
MeHisd3_HilicPos[,'Type'] = NA #formatting so later function works#
MeHisd3_HilicPos_reorder <- MeHisd3_HilicPos[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

Carnd3_HilicPos <- Hilic_Positive_Manual %>% subset(select = c(1:2, 23:24, 26)) #select columns containingsurrogate standard and internal standard
Carnd3_HilicPos[,'Type'] = NA #formatting so later function works#
Carnd3_HilicPos_reorder <- Carnd3_HilicPos[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

GABAd6_HilicPos <- Hilic_Positive_Manual %>% subset(select = c(1:2, 31:32, 26)) #select columns containingsurrogate standard and internal standard
GABAd6_HilicPos[,'Type'] = NA #formatting so later function works#
GABAd6_HilicPos_reorder <- GABAd6_HilicPos[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

FTyr_HilicPos <- Hilic_Positive_Manual %>% subset(select = c(1:2, 19:20, 26))
FTyr_HilicPos[,'Type'] = NA #formatting so later function works#
FTyr_HilicPos_reorder <- FTyr_HilicPos[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#


#Hilic Negative#
Hilic_Negative_Manual <-read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Manual Integrations/ContactLensNontarget_Quant_HILICNeg_20250128.csv", col_types = cols(.default = "c"))
FTyr_HilicNeg <- Hilic_Negative_Manual %>% subset(select = c(1:2, 11:12, 4)) #select columns containingsurrogate standard and internal standard
FTyr_HilicNeg[,'Type'] = NA #formatting so later function works#
FTyr_HilicNeg_reorder <- FTyr_HilicNeg[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#


ClTyr_HilicNeg <- Hilic_Negative_Manual %>% subset(select = c(1:2, 9:10, 4)) #select columns containingsurrogate standard and internal standard
ClTyr_HilicNeg[,'Type'] = NA #formatting so later function works#
ClTyr_HilicNeg_reorder <- ClTyr_HilicNeg[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

FPhe_HilicNeg <- Hilic_Negative_Manual %>% subset(select = c(1:2, 7:8, 4)) #select columns containingsurrogate standard and internal standard
FPhe_HilicNeg[,'Type'] = NA #formatting so later function works#
FPhe_HilicNeg_reorder <- FPhe_HilicNeg[,c(1, 2, 6, 3, 4, 5)] #reorder for consistency#

#functions for manual integrations of internal and surrogate standards#

manual_integration_clean <- function(df){
  df_clean <- df[-c(1),] #remove first row
  names(df_clean) <- df[1,] #set removed first row as column names
  names(df_clean) <- gsub(" ", "_", names(df_clean))#replace spaces in column names with underscore#
  colnames(df_clean)[5] = 'Area' #duplicate row names not compatible with rename function#
  colnames(df_clean)[6] = 'ISTD_Area'
  df_clean <- df_clean %>% mutate(Data_File = ifelse(Data_File =='JOQT31508_10b.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
    mutate(Data_File = ifelse(Data_File =='JOQT31508_11b.d','P15_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_12b.d','P15_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_13b.d','P16_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_14b.d','P16_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_15b.d','P16_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_16b.d','P16_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1b.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_2b.d','P12_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_3b.d','P12_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_4b.d','P12_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_5b.d','P14_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1b.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_6b.d','P14_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_7b.d','P14_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_8b.d','P14_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_9b.d','P15_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1b.d','Blank_1_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1b.d','Blank_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1b.d','Blank_2_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2b.d','Blank_2', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1b.d','QCB_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blankb.d','QCB_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2b.d','QCP_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooledb.d','QCP_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1b.d','Spk1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2b.d','Spk2_002', Data_File)) %>%
    dplyr::rename(Sample = Data_File)
  df_clean$Area <- as.numeric(df_clean$Area)
  df_clean$ISTD_Area <- as.numeric(df_clean$ISTD_Area)
  df_clean[,'ISTD_Normalized_Area'] = df_clean$Area/df_clean$ISTD_Area
  df_clean[,'Participant_Number'] = NA
  df_clean[,'Sample_Type'] = NA
  df <- df_clean %>% mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #assign participant numbers
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
    mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('QCB', Sample), 'QC Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('QCP', Sample), 'QC Pooled', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('Blank', Sample), 'Solvent Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('Spk', Sample), 'Spike Blank', Sample_Type))
  df
} #write function for cleaning manual integrations, for if sample file has b.d#

manual_integration_cleana <- function(df){
  df_clean <- df[-c(1),] #remove first row
  names(df_clean) <- df[1,] #set removed first row as column names
  names(df_clean) <- gsub(" ", "_", names(df_clean))#replace spaces in column names with underscore#
  colnames(df_clean)[5] = 'Area' #duplicate row names not compatible with rename function#
  colnames(df_clean)[6] = 'ISTD_Area'
  df_clean <- df_clean %>% mutate(Data_File = ifelse(Data_File =='JOQT31508_10a.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
    mutate(Data_File = ifelse(Data_File =='JOQT31508_11a.d','P15_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_12a.d','P15_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_13a.d','P16_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_14a.d','P16_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_15a.d','P16_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_16a.d','P16_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1a.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_2a.d','P12_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_3a.d','P12_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_4a.d','P12_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_5a.d','P14_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1a.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_6a.d','P14_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_7a.d','P14_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_8a.d','P14_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_9a.d','P15_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1a.d','Blank_1_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1a.d','Blank_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1a.d','Blank_2_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2a.d','Blank_2', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1a.d','QCB_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blanka.d','QCB_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2a.d','QCP_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooleda.d','QCP_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1a.d','Spk1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2a.d','Spk2_002', Data_File)) %>%
    dplyr::rename(Sample = Data_File)
  df_clean$Area <- as.numeric(df_clean$Area)
  df_clean$ISTD_Area <- as.numeric(df_clean$ISTD_Area)
  df_clean[,'ISTD_Normalized_Area'] = df_clean$Area/df_clean$ISTD_Area
  df_clean[,'Participant_Number'] = NA
  df_clean[,'Sample_Type'] = NA
  df <- df_clean %>% mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #assign participant numbers
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
    mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type))%>%
    mutate(Sample_Type = ifelse(grepl('QCB', Sample), 'QC Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('QCP', Sample), 'QC Pooled', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('Blank', Sample), 'Solvent Blank', Sample_Type)) %>%
    dplyr::rename(Type = 'NA') %>%
    mutate(Sample_Type = ifelse(grepl('Spk', Sample), 'Spike Blank', Sample_Type))
  df
} #write function for cleaning manual integrations, for if sample file has a.d#

manual_integration_cleanc <- function(df){
  df_clean <- df[-c(1),] #remove first row
  names(df_clean) <- df[1,] #set removed first row as column names
  names(df_clean) <- gsub(" ", "_", names(df_clean))#replace spaces in column names with underscore#
  colnames(df_clean)[5] = 'Area' #duplicate row names not compatible with rename function#
  colnames(df_clean)[6] = 'ISTD_Area'
  df_clean <- df_clean %>% mutate(Data_File = ifelse(Data_File =='JOQT31508_10c.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
    mutate(Data_File = ifelse(Data_File =='JOQT31508_11c.d','P15_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_12c.d','P15_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_13c.d','P16_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_14c.d','P16_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_15c.d','P16_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_16c.d','P16_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1c.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_2c.d','P12_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_3c.d','P12_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_4c.d','P12_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_5c.d','P14_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1c.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_6c.d','P14_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_7c.d','P14_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_8c.d','P14_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_9c.d','P15_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1c.d','Blank_1_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1c.d','Blank_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1c.d','Blank_2_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2c.d','Blank_2', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1c.d','QCB_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blankc.d','QCB_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2c.d','QCP_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooledc.d','QCP_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1c.d','Spk1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2c.d','Spk2_002', Data_File)) %>%
    dplyr::rename(Sample = Data_File)
  df_clean$Area <- as.numeric(df_clean$Area)
  df_clean$ISTD_Area <- as.numeric(df_clean$ISTD_Area)
  df_clean[,'ISTD_Normalized_Area'] = df_clean$Area/df_clean$ISTD_Area
  df_clean[,'Participant_Number'] = NA
  df_clean[,'Sample_Type'] = NA
  df <- df_clean %>% mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #assign participant numbers
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
    mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type))%>%
    mutate(Sample_Type = ifelse(grepl('QCB', Sample), 'QC Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('QCP', Sample), 'QC Pooled', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('Blank', Sample), 'Solvent Blank', Sample_Type)) %>%
    dplyr::rename(Type = 'NA') %>%
    mutate(Sample_Type = ifelse(grepl('Spk', Sample), 'Spike Blank', Sample_Type))
  df
} #write function for cleaning manual integrations, for if sample file has c.d#

manual_integration_clean_ <- function(df){ #write function for cleaning manual integrations with no suffix
  df_clean <- df[-c(1),] #remove first row
  names(df_clean) <- df[1,] #set removed first row as column names
  names(df_clean) <- gsub(" ", "_", names(df_clean))#replace spaces in column names with underscore#
  colnames(df_clean)[5] = 'Area' #duplicate row names not compatible with rename function#
  colnames(df_clean)[6] = 'ISTD_Area'
  df_clean <- df_clean %>% mutate(Data_File = ifelse(Data_File =='JOQT31508_10.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
    mutate(Data_File = ifelse(Data_File =='JOQT31508_11.d','P15_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_12.d','P15_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_13.d','P16_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_14.d','P16_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_15.d','P16_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_16.d','P16_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_2.d','P12_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_3.d','P12_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_4.d','P12_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_5.d','P14_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_6.d','P14_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_7.d','P14_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_8.d','P14_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_9.d','P15_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1.d','Blank_1_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1.d','Blank_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1.d','Blank_2_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2.d','Blank_2', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1.d','QCB_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank.d','QCB_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2.d','QCP_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled.d','QCP_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1.d','Spk1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2.d','Spk2_002', Data_File)) %>%
    dplyr::rename(Sample = Data_File)
  df_clean$Area <- as.numeric(df_clean$Area)
  df_clean$ISTD_Area <- as.numeric(df_clean$ISTD_Area)
  df_clean[,'ISTD_Normalized_Area'] = df_clean$Area/df_clean$ISTD_Area
  df_clean[,'Participant_Number'] = NA
  df_clean[,'Sample_Type'] = NA
  df <- df_clean %>% mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #assign participant numbers
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
    mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type))%>%
    mutate(Sample_Type = ifelse(grepl('QCB', Sample), 'QC Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('QCP', Sample), 'QC Pooled', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('Blank', Sample), 'Solvent Blank', Sample_Type)) %>%
    dplyr::rename(Type = 'NA') %>%
    mutate(Sample_Type = ifelse(grepl('Spk', Sample), 'Spike Blank', Sample_Type))
  df
} #write function for cleaning manual integrations, for if sample file has no suffix#

#clean manual integration dfs#
#get istds#
Stearic_Acid <- Hilic_Negative_Manual %>% subset(select = c(2,4)) #select for stearic acid's peak area and sample
Stearic_Acid2 <- Stearic_Acid[-c(1),] #remove first row
names(Stearic_Acid2) <- Stearic_Acid[1,] #set removed first row as column names
names(Stearic_Acid2) <- gsub(" ", "_", names(Stearic_Acid2))#replace spaces in column names with underscore#

Stearic_Acid_Clean_Hilic_neg2<- Stearic_Acid2 %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_10c.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
  mutate(Data_File = ifelse(Data_File =='JOQT31508_11c.d','P15_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_12c.d','P15_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_13c.d','P16_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_14c.d','P16_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_15c.d','P16_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_16c.d','P16_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1c.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_2c.d','P12_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_3c.d','P12_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_4c.d','P12_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_5c.d','P14_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1c.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_6c.d','P14_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_7c.d','P14_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_8c.d','P14_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_9c.d','P15_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1c.d','Blank_1_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1c.d','Blank_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1c.d','Blank_2_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2c.d','Blank_2', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1c.d','QCB_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blankc.d','QCB_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2c.d','QCP_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooledc.d','QCP_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1c.d','Spk1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2c.d','Spk2_002', Data_File)) %>%
  dplyr::rename(Sample = Data_File) %>%
  dplyr::rename(ISTD_Area = Area)

Stearic_Acid_Clean_Hilic_neg <- Stearic_Acid_Clean_Hilic_neg2 #make separate df for later merging#
Stearic_Acid_Clean_Hilic_neg[,'Type'] = 'Hilic_Negative'

#stearic acid for C18 Negative
Stearic_Acid_C18 <- C18_Negative_Manual %>% subset(select = c(2,12))
Stearic_Acid_C18_2 <- Stearic_Acid_C18[-c(1),] #remove first row
names(Stearic_Acid_C18_2) <- Stearic_Acid_C18[1,] #set removed first row as column names
names(Stearic_Acid_C18_2) <- gsub(" ", "_", names(Stearic_Acid_C18_2))#replace spaces in column names with underscore#

Stearic_Acid_Clean_C18<- Stearic_Acid_C18_2 %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_10a.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
  mutate(Data_File = ifelse(Data_File =='JOQT31508_11a.d','P15_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_12a.d','P15_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_13a.d','P16_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_14a.d','P16_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_15a.d','P16_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_16a.d','P16_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1a.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_2a.d','P12_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_3a.d','P12_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_4a.d','P12_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_5a.d','P14_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1a.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_6a.d','P14_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_7a.d','P14_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_8a.d','P14_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_9a.d','P15_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1a.d','Blank_1_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1a.d','Blank_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1a.d','Blank_2_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2a.d','Blank_2', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1a.d','QCB_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blanka.d','QCB_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2a.d','QCP_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooleda.d','QCP_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1a.d','Spk1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2a.d','Spk2_002', Data_File)) %>%
  dplyr::rename(Sample = Data_File) %>%
  dplyr::rename(ISTD_Area = Area)
Stearic_Acid_Clean_C18[,'Type'] = 'C18_Negative'

#Creatine d3 for hilic positive#
Creatine_d3_hilic <- Hilic_Positive_Manual %>% subset(select = c(2, 26))
Creatine_d3_2_hilic <- Creatine_d3_hilic[-c(1),] #remove first row
names(Creatine_d3_2_hilic) <- Creatine_d3_hilic[1,] #set removed first row as column names
names(Creatine_d3_2_hilic) <- gsub(" ", "_", names(Creatine_d3_2_hilic))#replace spaces in column names with underscore#

Creatine_d3_Clean_Hilic <- Creatine_d3_2_hilic %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_10b.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
  mutate(Data_File = ifelse(Data_File =='JOQT31508_11b.d','P15_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_12b.d','P15_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_13b.d','P16_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_14b.d','P16_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_15b.d','P16_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_16b.d','P16_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1b.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_2b.d','P12_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_3b.d','P12_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_4b.d','P12_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_5b.d','P14_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1b.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_6b.d','P14_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_7b.d','P14_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_8b.d','P14_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_9b.d','P15_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1b.d','Blank_1_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1b.d','Blank_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1b.d','Blank_2_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2b.d','Blank_2', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1b.d','QCB_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blankb.d','QCB_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2b.d','QCP_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooledb.d','QCP_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1b.d','Spk1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2b.d','Spk2_002', Data_File)) %>%
  dplyr::rename(Sample = Data_File) %>%
  dplyr::rename(ISTD_Area = Area)
Creatine_d3_Clean_Hilic[,'Type'] = 'Hilic_Positive'

#Creatine d3 for C18 positive#
Creatine_d3_C18 <- C18_Positive_Manual %>% subset(select = c(2, 10))
Creatine_d3_C18_2 <- Creatine_d3_C18[-c(1),] #remove first row
names(Creatine_d3_C18_2) <- Creatine_d3_C18[1,] #set removed first row as column names
names(Creatine_d3_C18_2) <- gsub(" ", "_", names(Creatine_d3_C18_2))#replace spaces in column names with underscore#

Creatine_d3_Clean_C18 <- Creatine_d3_C18_2 %>% mutate(Data_File = ifelse(Data_File =='JOQT31508_10.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
  mutate(Data_File = ifelse(Data_File =='JOQT31508_11.d','P15_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_12.d','P15_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_13.d','P16_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_14.d','P16_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_15.d','P16_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_16.d','P16_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_2.d','P12_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_3.d','P12_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_4.d','P12_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_5.d','P14_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_1.d','P12_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_6.d','P14_D1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_7.d','P14_D2_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_8.d','P14_D3_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_9.d','P15_B_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1.d','Blank_1_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1.d','Blank_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1.d','Blank_2_1', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2.d','Blank_2', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1.d','QCB_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank.d','QCB_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2.d','QCP_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled.d','QCP_002', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1.d','Spk1_001', Data_File)) %>%
  mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2.d','Spk2_002', Data_File)) %>%
  dplyr::rename(Sample = Data_File) %>%
  dplyr::rename(ISTD_Area = Area)
Creatine_d3_Clean_C18[,'Type'] = 'C18_Positive'

All_ISTDs <- rbind(Creatine_d3_Clean_C18, Creatine_d3_Clean_Hilic, Stearic_Acid_Clean_C18, Stearic_Acid_Clean_Hilic_neg) %>% #bind istds together for later use
  filter(Sample != 'Blank_1' & Sample != 'Blank_2' & Sample != 'Blank_2_1' & Sample != 'Blank_1_1') #filter out blanks without istd

##get all hilic positive recoveries##

#rename every istd column to the sample name + istd#
manual_integration_clean_no_filters <- function(df){
  df_clean <- df[-c(1),] #remove first row
  names(df_clean) <- gsub(" ", "_", names(df_clean))#replace spaces in column names with underscore#
  names(df_clean) <- gsub("-", "!", names(df_clean))#replace spaces in column names with underscore#

  df_clean <- df_clean %>%dplyr::rename(Data_File = '...2') %>%
    subset(select = -c(Sample)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_10b.d','P15_D1_001', Data_File)) %>% #rename samples to match other data frames#
    mutate(Data_File = ifelse(Data_File =='JOQT31508_11b.d','P15_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_12b.d','P15_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_13b.d','P16_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_14b.d','P16_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_15b.d','P16_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_16b.d','P16_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1b.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_2b.d','P12_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_3b.d','P12_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_4b.d','P12_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_5b.d','P14_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_1b.d','P12_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_6b.d','P14_D1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_7b.d','P14_D2_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_8b.d','P14_D3_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_9b.d','P15_B_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1_1b.d','Blank_1_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_1b.d','Blank_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2_1b.d','Blank_2_1', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Blank_2b.d','Blank_2', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blank_1b.d','QCB_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Blankb.d','QCB_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooled_2b.d','QCP_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_QC-Pooledb.d','QCP_002', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-1b.d','Spk1_001', Data_File)) %>%
    mutate(Data_File = ifelse(Data_File =='JOQT31508_Spike_Blank-2b.d','Spk2_002', Data_File)) %>%
    dplyr::rename(Sample = Data_File)
  df_clean[,'Participant_Number'] = NA
  df_clean[,'Sample_Type'] = NA
  df <- df_clean %>% mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #assign participant numbers
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
    mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('QCB', Sample), 'QC Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('QCP', Sample), 'QC Pooled', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('Blank', Sample), 'Solvent Blank', Sample_Type)) %>%
    mutate(Sample_Type = ifelse(grepl('Spk', Sample), 'Spike Blank', Sample_Type))
  df
} #for if sample file has b.d, #write function for cleaning manual integrations#
Hilic_Positive_Manual2 <- manual_integration_clean_no_filters(Hilic_Positive_Manual)

start_col <- 3
step <- 2
# Loop through the columns starting from `start_col` and rename every other column
for(i in seq(start_col, ncol(Hilic_Positive_Manual2), by = step)) {
  if (i > 1) {
    colnames(Hilic_Positive_Manual2)[i] <- paste0(colnames(Hilic_Positive_Manual2)[i - 1], "_istd")
  }
}
colnames(Hilic_Positive_Manual2)
hilic_pos_tibble_list <- list()
n_cols <- ncol(Hilic_Positive_Manual2)-2
start <-2

#for loop to get every other column and bind together

rename_by_pos = function(df, index, new_name){
  colnames(df)[index] = new_name
  df
} #function to rename columns based on their index

for(i in seq(start, n_cols, by = 2)){
  if (i + 1 <= n_cols) {
    subset_df <- Hilic_Positive_Manual2[,c(1, i:(i + 1), 40, 41)]
  }
  subset_df2<-subset_df %>%
    rename_by_pos(2,"Retention_Time") %>%
    rename_by_pos(3,"Area")
  compound <- colnames(subset_df[,c(2)])
  subset_df2[,'Compound'] = compound
  subset_df2 <- subset_df2 %>%
    separate(col = Compound, into = c("Compound", "Discard"), sep = "_") %>% #split word results into second column
    subset(select = -c(Discard))
  hilic_pos_tibble_list[[i]] <- subset_df2
}

HILIC_Pos_long <- bind_rows(hilic_pos_tibble_list)%>%
  dplyr::rename(Sample_Type = Participant_Number_istd)

HILIC_Pos_Surrogates <- HILIC_Pos_long %>% #filter for surrogate standards
  filter(Compound == 'TMAO!d9' | Compound == 'Choline!d9'|
           Compound == 'F!Phe' | Compound == 'F!Tyr' | Compound == 'Carnitine!d3' |
           Compound == 'GABA!d6' | Compound == 'MeHis!d3' | Compound == 'His!15N') %>%
  subset(select = -c(Retention_Time)) %>%
  dplyr::rename(Surrogate = Compound) %>%
  dplyr::rename(Surrogate_Area = Area)


HILIC_Pos_ISTD <- HILIC_Pos_long %>%
  filter(Compound == 'Creatine!d3') %>% #filter for internal standards
  subset(select = -c(Retention_Time)) %>%
  dplyr::rename(ISTD = Compound) %>%
  dplyr::rename(ISTD_Area = Area)

HILIC_Pos_SSTD_ISTD_Merge <- merge(HILIC_Pos_Surrogates, HILIC_Pos_ISTD, by = c('Sample', 'Participant_Number','Sample_Type')) #merge so surrogate is in one column and internal is in the next
nrow(HILIC_Pos_Surrogates)-nrow(HILIC_Pos_SSTD_ISTD_Merge)

HILIC_Pos_SSTD_ISTD_Merge$Surrogate_Area <- as.numeric(HILIC_Pos_SSTD_ISTD_Merge$Surrogate_Area)
HILIC_Pos_SSTD_ISTD_Merge$ISTD_Area <- as.numeric(HILIC_Pos_SSTD_ISTD_Merge$ISTD_Area)

HILIC_Pos_SSTD_ISTD_Merge[,'ISTD_Corrected_Area'] = HILIC_Pos_SSTD_ISTD_Merge$Surrogate_Area/HILIC_Pos_SSTD_ISTD_Merge$ISTD_Area

#get ideal ratio between surrogate and internal standard
HILIC_Pos_Ideal_Ratio <- HILIC_Pos_SSTD_ISTD_Merge %>% filter(Sample_Type == 'Spike Blank') %>%
  group_by(Surrogate) %>%
  dplyr::summarise(Mean_Spike_Ratio= mean(ISTD_Corrected_Area))

##get recoveries for most representative surrogate from each method##
HILIC_Pos_Samle_Ratios <-
  HILIC_Pos_SSTD_ISTD_Merge %>% filter(Sample_Type == 'Blank' | Sample_Type == 'Sample' | Sample_Type == 'QC Pooled' | Sample_Type == 'QC Blank')

HILIC_Pos_Recovery <- merge(HILIC_Pos_Samle_Ratios, HILIC_Pos_Ideal_Ratio, by = c('Surrogate'))
nrow(HILIC_Pos_Samle_Ratios) - nrow(HILIC_Pos_Recovery)
HILIC_Pos_Recovery[,'Recovery'] = HILIC_Pos_Recovery$ISTD_Corrected_Area*100/HILIC_Pos_Recovery$Mean_Spike_Ratio

HILIC_Pos_Recovery_Stats <- HILIC_Pos_Recovery %>%
  ungroup() %>%
  group_by(Surrogate) %>%
  dplyr::summarise(Mean_Recovery = mean(Recovery), sd_Recovery = sd(Recovery)) %>%
  ungroup()
HILIC_Pos_Recovery_Stats[,'RSD'] = HILIC_Pos_Recovery_Stats$sd_Recovery*100/HILIC_Pos_Recovery_Stats$Mean_Recovery

istd_manual_integration <- merge(Manual_integration_all_no_blank_sub, Stearic_Acid_Clean_Hilic_neg2, by = c('Sample'), all =TRUE) %>%
  mutate(Area = ifelse(is.na(Area), '0', Area)) #replace non detects with 0

istd_manual_integration$Area <- as.numeric(istd_manual_integration$Area)
istd_manual_integration$ISTD_Area <- as.numeric(istd_manual_integration$ISTD_Area)

istd_manual_integration[,'ISTD_Normalized_Area'] = istd_manual_integration$Area/istd_manual_integration$ISTD_Area

#bind into one data frame for calculations#
#C18 Negative
Myristicd27_C18Neg_Clean <- manual_integration_cleana(Myristicd27_C18Neg_reorder)
Myristicd27_C18Neg_Clean[,'Compound'] = 'Myristic-d27'
Myristicd27_C18Neg_Clean[,'Method_Type'] = 'C18_Negative'

#C18 Positive
Cholined9_C18Pos_Clean <- manual_integration_clean_(Cholined9_C18Pos_reorder)
Cholined9_C18Pos_Clean[,'Compound'] = 'Choline-d9'
Cholined9_C18Pos_Clean[,'Method_Type'] = 'C18_Positive'
view(Cholined9_C18Pos_Clean)
#Hilic positive
Cholined9_HilicPos_Clean <- manual_integration_clean(Cholined9_HilicPos_reorder) %>%
  dplyr::rename(Type = 'NA')
Cholined9_HilicPos_Clean[,'Compound'] = 'Choline-d9'
Cholined9_HilicPos_Clean[,'Method_Type'] = 'Hilic_Positive'

MeHisd3_HilicPos_Clean <- manual_integration_clean(MeHisd3_HilicPos_reorder) %>%
  dplyr::rename(Type = 'NA')
MeHisd3_HilicPos_Clean[,'Compound'] = 'MeHis-d3'
MeHisd3_HilicPos_Clean[,'Method_Type'] = 'Hilic_Positive'

Carnd3_HilicPos_Clean <- manual_integration_clean(Carnd3_HilicPos_reorder) %>%
  dplyr::rename(Type = 'NA')
Carnd3_HilicPos_Clean[,'Compound'] = 'Carnitine-d3'
Carnd3_HilicPos_Clean[,'Method_Type'] = 'Hilic_Positive'

GABAd6_HilicPos_Clean <- manual_integration_clean(GABAd6_HilicPos_reorder) %>%
  dplyr::rename(Type = 'NA')
GABAd6_HilicPos_Clean[,'Compound'] = 'GABA-d6'
GABAd6_HilicPos_Clean[,'Method_Type'] = 'Hilic_Positive'

FTyr_HilicPos_Clean <- manual_integration_clean(FTyr_HilicPos_reorder) %>%
  dplyr::rename(Type = 'NA')
FTyr_HilicPos_Clean[,'Compound'] = 'F-Tyr'
FTyr_HilicPos_Clean[,'Method_Type'] = 'Hilic_Positive'

#Hilic Negative
FTyr_HilicNeg_Clean <- manual_integration_cleanc(FTyr_HilicNeg_reorder)
FTyr_HilicNeg_Clean[,'Compound'] = 'F-Tyr'

ClTyr_HilicNeg_Clean <- manual_integration_cleanc(ClTyr_HilicNeg_reorder)
ClTyr_HilicNeg_Clean[,'Compound'] = 'Cl-Tyr'

FPhe_HilicNeg_Clean <- manual_integration_cleanc(FPhe_HilicNeg_reorder)
FPhe_HilicNeg_Clean[,'Compound'] = 'F-Phe'

HilicNeg_Manual_SS_All <- rbind(FTyr_HilicNeg_Clean, ClTyr_HilicNeg_Clean, FPhe_HilicNeg_Clean) #bind hilic ones together
HilicNeg_Manual_SS_All[,'Method_Type'] = 'Hilic_Negative'

#bind all together#
SS_All <- rbind(FTyr_HilicPos_Clean, HilicNeg_Manual_SS_All, Cholined9_HilicPos_Clean, Cholined9_C18Pos_Clean, Myristicd27_C18Neg_Clean, MeHisd3_HilicPos_Clean, GABAd6_HilicPos_Clean, Carnd3_HilicPos_Clean)

#get ideal SS:ISTD ratios#
Spike_Blank_Stats <- SS_All %>% filter(Sample_Type == 'Spike Blank') %>% group_by(Compound, Method_Type) %>% get_summary_stats(ISTD_Normalized_Area, type = 'common')

SS_Samples <- SS_All %>% filter(Sample_Type != 'Spike Blank' & Sample_Type != 'Solvent Blank') #filter out things without recovery

SS_Sample_Calculations <- merge(SS_Samples, Spike_Blank_Stats, by = c('Method_Type', 'Compound'), all = TRUE) %>% #add column for ideal ratio
  dplyr::rename(Ideal_SS = mean) %>%
  filter(Method_Type != 'HILIC_Negative' | Compound == 'F-Phe') %>%
  subset(select = -c(n, sd, se, ci, iqr, median, max, min, n, variable))

#calculate percent recovery#
SS_Sample_Calculations[,'Percent_Recovery'] = SS_Sample_Calculations$ISTD_Normalized_Area*100/SS_Sample_Calculations$Ideal_SS

#get average percent recovery#
Percent_Recovery_Stats <- SS_Sample_Calculations %>%
  group_by(Compound, Method_Type) %>% get_summary_stats(Percent_Recovery, type = 'common')
Percent_Recovery_Stats[,'RSD'] = Percent_Recovery_Stats$sd*100/Percent_Recovery_Stats$mean
#write.csv(Percent_Recovery_Stats,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/SI_Other_Data_Sets/LC_Recoveries.csv", row.names=FALSE) #export final data frame#

##get recovery statistics##
Percent_Recoveries <- SS_Sample_Calculations %>% subset(select = c(Method_Type, Compound, Sample, Percent_Recovery, Participant_Number, Sample_Type)) %>%
  dplyr::rename(SS_Compound = Compound)

Hilic_Neg_recovery <- Percent_Recoveries %>%filter(SS_Compound == 'F-Phe')

#load targeted EI from tracefinder#
CL_EI_manual_integrations <- read_csv('/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Manuscript/Targeted_GCEI_WornParticipantSamples_AllSamples_RawData(All Samples).csv', col_types = cols(.default = "c"))

CL_EI_manual_integrations[,'Type'] = 'EI'

#filter for internal standard
Fluoranthene <- CL_EI_manual_integrations %>% filter(Compound == 'Fluoranthene-d10') %>%
  subset(select = c(Compound, Area, Filename, Type)) %>%
  filter(Filename!='P16_D2_001' & Filename != 'P14_D3_002' & Filename != 'Blk_001' & Filename != 'SolventBlank_001' & Filename != 'SolventBlank_002') %>% #filter out solvent blanks and samples that didn't inject
  dplyr::rename(ISTD_Area = Area) %>% #rename to match LC data#
  dplyr::rename(Sample = Filename) %>%
  subset(select = -c(Compound)) %>%
  mutate(Sample = ifelse(Sample == 'QCB_001', 'QCB_002', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'QCP_001', 'QCP_002', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'Spk2_001', 'Spk2_002', Sample))

#merge GC internal standard with LC ones
All_ISTDs_with_EI <- rbind(All_ISTDs,Fluoranthene)

CL_EI_manual_integrations[,'Sample_Type'] = NA
CL_EI_manual_integrations[,'Participant_Number'] = NA
CL_EI_manual_integrations[,'Day'] = NA

CL_EI_manual_integrations2 <- CL_EI_manual_integrations %>%
  subset(select = c(Compound, Filename, Area, `ISTD Response`, Type, Sample_Type, Participant_Number, Day)) %>%
  dplyr::rename(Sample = Filename) %>%
  dplyr::rename(ISTD_Area = `ISTD Response`) %>%
  mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>% #label samples as blank vs sample
  mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
  mutate(Sample_Type = ifelse(grepl('Spk', Sample), 'Method_Blank', Sample_Type)) %>%
  mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #label samples with participant number
  mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
  mutate(Day = ifelse(grepl('D1', Sample), '1', Day)) %>%
  mutate(Day = ifelse(grepl('D2', Sample), '2', Day)) %>%
  mutate(Day = ifelse(grepl('D3', Sample), '3', Day)) %>%
  filter(!is.na(Sample_Type)) %>%#
  filter(Compound != 'FBDE 100 (2)') %>% #remove duplicated analyte
  filter(Sample != 'P14_D3_002') %>% #filter out a rerun
  filter(Sample != 'P16_D2_001') #filter out failed injection
CL_EI_manual_integrations2$Area <- as.numeric(CL_EI_manual_integrations2$Area) #changes all N/Fs to NAs
CL_EI_manual_integrations2$ISTD_Area <- as.numeric(CL_EI_manual_integrations2$ISTD_Area)

CL_EI_manual_integrations2[,'ISTD_Normalized_Area'] = CL_EI_manual_integrations2$Area/CL_EI_manual_integrations2$ISTD_Area

#get ideal SS:ISTD ratios
CL_EI_manual_MBs <- CL_EI_manual_integrations2 %>% filter(Sample_Type == 'Method_Blank') %>% #filter for method blanks and get ideal recoveries for surrogates
  filter(Compound == 'Tris(2-chloroethyl) phosphate-d12' | Compound == 'Triphenyl phosphate-13C18' | Compound == 'Triethyl phosphate-d15 (TEP-d15)' | Compound == 'Pentabromobenzene-13C6'|
           Compound == 'Hexabromobenzene-13C6' | Compound == 'FBDE 208' | Compound == 'FBDE 157' | Compound == 'FBDE 100' | Compound == 'Di-n-butyl phthalate - d4' |
           Compound == 'Diethyl phthalate-d4' | Compound == 'Bis(2- ethylhexyl) phthalate-d4') %>% #filter for surrogate standards
  mutate(ISTD_Normalized_Area = ifelse(is.na(ISTD_Normalized_Area), 0, ISTD_Normalized_Area)) %>%#NAs were things that weren't found in the sample. turn to 0
  group_by(Compound) %>%
  dplyr::summarise(Ideal_ratio = mean(ISTD_Normalized_Area)) %>% #get the ideal ratio from means of method blanks for each surrogate
  subset(select = c(Compound, Ideal_ratio))

#get recoveries
CL_EI_manual_not_MB <- CL_EI_manual_integrations2 %>% filter(Sample_Type != 'Method_Blank') %>%
  filter(Compound == 'Tris(2-chloroethyl) phosphate-d12' | Compound == 'Triphenyl phosphate-13C18' | Compound == 'Triethyl phosphate-d15 (TEP-d15)' | Compound == 'Pentabromobenzene-13C6'|
           Compound == 'Hexabromobenzene-13C6' | Compound == 'FBDE 208' | Compound == 'FBDE 157' | Compound == 'FBDE 100' | Compound == 'Di-n-butyl phthalate - d4' |
           Compound == 'Diethyl phthalate-d4' | Compound == 'Bis(2- ethylhexyl) phthalate-d4') %>% #filter for surrogates
  mutate(ISTD_Normalized_Area = ifelse(is.na(ISTD_Normalized_Area), 0, ISTD_Normalized_Area))#NAs were things that weren't found in the sample. turn to 0

CL_EI_manual_Recoveries_merged <- merge(CL_EI_manual_not_MB, CL_EI_manual_MBs, by = c('Compound'), all = T) %>%
  filter(Ideal_ratio != 0) #filter out things not detected in method blank
CL_EI_manual_Recoveries_merged[,'Recovery'] = CL_EI_manual_Recoveries_merged$ISTD_Normalized_Area*100/CL_EI_manual_Recoveries_merged$Ideal_ratio

CL_EI_manual_Recoveries_clean <- CL_EI_manual_Recoveries_merged %>% #get just columns needed#
  subset(select = c(Compound, Sample, Recovery))

#get recovery statistics
CL_EI_manual_Recovery_stats <- CL_EI_manual_Recoveries_merged %>%
  filter(Sample_Type == 'Blank' | Sample_Type == 'Sample') %>%
  group_by(Compound) %>%
  get_summary_stats(Recovery, type = 'common')
CL_EI_manual_Recovery_stats['RSD'] = CL_EI_manual_Recovery_stats$sd*100/CL_EI_manual_Recovery_stats$mean

######load and clean non-targeted data files from MSDial and Compound Discoverer######

C18Negative <- read_csv('/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/DIA Full Scan MS Data Participants #12, #14-16 (2024 11 27)/C18Negative_50-2000 - Copy.csv', col_types = cols(.default = "c"))
C18Positive <- read_csv('/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/DIA Full Scan MS Data Participants #12, #14-16 (2024 11 27)/C18Positive_50-2000 - Copy.csv', col_types = cols(.default = "c"))
HILICNegative <- read_csv('/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/DIA Full Scan MS Data Participants #12, #14-16 (2024 11 27)/HILICNegative_50-2000 - Copy.csv', col_types = cols(.default = "c"))
HILICPositive <- read_csv('/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/DIA Full Scan MS Data Participants #12, #14-16 (2024 11 27)/HILICPositive_50-2000 - Copy.csv', col_types = cols(.default = "c"))
EIPositive <- read_csv('/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/DIA Full Scan MS Data Participants #12, #14-16 (2024 11 27)/GCEI_Positive_CD - Copy(in).csv', col_types = cols(.default = "c"))

#Clean C18negative#

#remove first four rows and set 5th as column names#
C18negative_Unclean1 <- C18Negative[-c(1:3),] #create new df to edit and remove unwanted rows#
names(C18negative_Unclean1) <- C18negative_Unclean1[1,] #make first row the column names#
C18negative_Unclean2 <- C18negative_Unclean1[-c(1),] #remove duplicate first row#

names(C18negative_Unclean2) <- gsub(" ", "_", names(C18negative_Unclean2)) #replace spaces in column names with underscore#
names(C18negative_Unclean2) <- gsub("/", ".", names(C18negative_Unclean2)) #replace slashes in column names with period#
names(C18negative_Unclean2) <- gsub("-", "_", names(C18negative_Unclean2)) #replace dashes in column names with underscore#
names(C18negative_Unclean2) <- gsub("%", "percent", names(C18negative_Unclean2))

colnames(C18negative_Unclean2)[62] <- "Sample.Avg" #these columns have duplicate names. one is sd and one is mean.#
colnames(C18negative_Unclean2)[63] <- "Sample.Std"

C18neg_lastcol <- match("Sample.Std",names(C18negative_Unclean2))
colnames(C18negative_Unclean2)[C18neg_lastcol+1] <- "Overflow"
colnames(C18negative_Unclean2)[C18neg_lastcol+2] <- "Overflow2"
colnames(C18negative_Unclean2)[C18neg_lastcol+3] <- "Overflow3"

C18negative_Unclean2_colnames <- C18negative_Unclean2 %>% colnames() #get column names of full df as string#

C18negative_Unclean_Overflow <- C18negative_Unclean2 %>% filter(Overflow != 'NA') #filter for the misformated data#
C18neg_overflowtype1 <- C18negative_Unclean_Overflow %>% filter(is.na(Overflow2))
C18neg_overflowtype1$Metabolite_name <- paste(C18neg_overflowtype1$Metabolite_name, C18neg_overflowtype1$Adduct_type)
C18neg_overflowtype1_cleaned <- subset(C18neg_overflowtype1, select = -c(Adduct_type)) #remove adduct type (empty column#)
names(C18neg_overflowtype1_cleaned) <- c(C18negative_Unclean2_colnames) #rename using column names of full df#

C18neg_overflowtype2 <- C18negative_Unclean_Overflow %>% filter(is.na(Overflow3) & Overflow2 != 'NA') #filters for rows overflowing by 2 columns. will need to adjust code to account for some data frames having this#
C18neg_overflowtype3 <- C18negative_Unclean_Overflow %>% filter(Overflow3 != 'NA') #filters for rows overflowing by 3 columns#

C18neg_overflowtype3$Metabolite_name <- paste(C18neg_overflowtype3$Metabolite_name, C18neg_overflowtype3$Adduct_type, C18neg_overflowtype3$Post_curation_result, C18neg_overflowtype3$Fill_percent)
C18neg_overflowtype3_cleaned <- subset(C18neg_overflowtype3, select = -c(Adduct_type, Post_curation_result, Fill_percent)) #remove not needed columns#
names(C18neg_overflowtype3_cleaned) <- c(C18negative_Unclean2_colnames) #rename using column names of full df#

C18neg_overflowtype1_cleaned <- subset(C18neg_overflowtype1_cleaned, select = -c(Overflow, Overflow2))

C18neg_not_overflow <- C18negative_Unclean2 %>% filter(is.na(Overflow)) %>% subset(select = -c(Overflow, Overflow2, Overflow3))

C18neg_CLEAN <- rbind(C18neg_not_overflow, C18neg_overflowtype1_cleaned, C18neg_overflowtype3_cleaned) %>%
  dplyr::rename(P15_D1_001 = JOQT31508_10a) %>%
  dplyr::rename(P15_D2_001 = JOQT31508_11a) %>%
  dplyr::rename(P15_D3_001 = JOQT31508_12a) %>%
  dplyr::rename(P16_B_001 = JOQT31508_13a) %>%
  dplyr::rename(P16_D1_001 = JOQT31508_14a) %>%
  dplyr::rename(P16_D2_001 = JOQT31508_15a) %>%
  dplyr::rename(P16_D3_001 = JOQT31508_16a) %>%
  dplyr::rename(P12_B_001 = JOQT31508_1a) %>%
  dplyr::rename(P12_D1_001 = JOQT31508_2a) %>%
  dplyr::rename(P12_D2_001 = JOQT31508_3a) %>%
  dplyr::rename(P12_D3_001 = JOQT31508_4a) %>%
  dplyr::rename(P14_B_001 = JOQT31508_5a) %>%
  dplyr::rename(P14_D1_001 = JOQT31508_6a) %>%
  dplyr::rename(P14_D2_001 = JOQT31508_7a) %>%
  dplyr::rename(P14_D3_001 = JOQT31508_8a) %>%
  dplyr::rename(P15_B_001 = JOQT31508_9a) %>%
  dplyr::rename(Blank_1_1 = JOQT31508_Blank_1_1a) %>%
  dplyr::rename(Blank_1 = JOQT31508_Blank_1a) %>%
  dplyr::rename(Blank_2_1 = JOQT31508_Blank_2_1a) %>%
  dplyr::rename(Blank_2 = JOQT31508_Blank_2a) %>%
  dplyr::rename(QCB_001 = JOQT31508_QC_Blank_1a) %>%
  dplyr::rename(QCB_002 = JOQT31508_QC_Blanka) %>%
  dplyr::rename(QCP_001 = JOQT31508_QC_Pooled_2a) %>%
  dplyr::rename(QCP_002 = JOQT31508_QC_Pooleda) %>%
  dplyr::rename(Spk1_001 = JOQT31508_Spike_Blank_1a) %>%
  dplyr::rename(Spk2_002 = JOQT31508_Spike_Blank_2a)
C18neg_CLEAN[,'Type'] = 'C18_Negative' #set type as additional column#
unique(C18neg_CLEAN$Adduct_type)
#Clean C18 Positive######
C18Positive_Unclean1 <- C18Positive[-c(1:3),] #create new df to edit and remove unwanted rows#
names(C18Positive_Unclean1) <- C18Positive_Unclean1[1,] #make first row the column names#
C18Positive_Unclean2 <- C18Positive_Unclean1[-c(1),] #remove duplicate first row#

names(C18Positive_Unclean2) <- gsub(" ", "_", names(C18Positive_Unclean2)) #replace spaces in column names with underscore#
names(C18Positive_Unclean2) <- gsub("/", ".", names(C18Positive_Unclean2)) #replace slashes in column names with period#
names(C18Positive_Unclean2) <- gsub("-", "_", names(C18Positive_Unclean2)) #replace dashes in column names with underscore#
names(C18Positive_Unclean2) <- gsub("%", "percent", names(C18Positive_Unclean2))

colnames(C18Positive_Unclean2)[62] <- "Sample.Avg" #these columns have duplicate names. one is sd and one is mean.#
colnames(C18Positive_Unclean2)[63] <- "Sample.Std"

C18pos_lastcol <- match("Sample.Std",names(C18Positive_Unclean2))
colnames(C18Positive_Unclean2)[C18pos_lastcol+1] <- "Overflow" #every column named with NA should be renamed to an overflow column#
colnames(C18Positive_Unclean2)[C18pos_lastcol+2] <- "Overflow2"
colnames(C18Positive_Unclean2)[C18pos_lastcol+3] <- "Overflow3"
colnames(C18Positive_Unclean2)[C18pos_lastcol+4] <- "Overflow4"
colnames(C18Positive_Unclean2)[C18pos_lastcol+5] <- "Overflow5"
colnames(C18Positive_Unclean2)[C18pos_lastcol+6] <- "Overflow6"
colnames(C18Positive_Unclean2)[C18pos_lastcol+7] <- "Overflow7"
colnames(C18Positive_Unclean2)[C18pos_lastcol+8] <- "Overflow8"
colnames(C18Positive_Unclean2)[C18pos_lastcol+9] <- "Overflow9"
colnames(C18Positive_Unclean2)[C18pos_lastcol+10] <- "Overflow10"
colnames(C18Positive_Unclean2)[C18pos_lastcol+11] <- "Overflow11"
colnames(C18Positive_Unclean2)[C18pos_lastcol+12] <- "Overflow12"
colnames(C18Positive_Unclean2)[C18pos_lastcol+13] <- "Overflow13"
colnames(C18Positive_Unclean2)[C18pos_lastcol+14] <- "Overflow14"
colnames(C18Positive_Unclean2)[C18pos_lastcol+15] <- "Overflow15"
colnames(C18Positive_Unclean2)[C18pos_lastcol+16] <- "Overflow16"
colnames(C18Positive_Unclean2)[C18pos_lastcol+17] <- "Overflow17"
colnames(C18Positive_Unclean2)[C18pos_lastcol+18] <- "Overflow18"
colnames(C18Positive_Unclean2)[C18pos_lastcol+19] <- "Overflow19"
colnames(C18Positive_Unclean2)[C18pos_lastcol+20] <- "Overflow20"
colnames(C18Positive_Unclean2)[C18pos_lastcol+21] <- "Overflow21"
colnames(C18Positive_Unclean2)[C18pos_lastcol+22] <- "Overflow22"
colnames(C18Positive_Unclean2)[C18pos_lastcol+23] <- "Overflow23"
colnames(C18Positive_Unclean2)[C18pos_lastcol+24] <- "Overflow24"
colnames(C18Positive_Unclean2)[C18pos_lastcol+25] <- "Overflow25"
colnames(C18Positive_Unclean2)[C18pos_lastcol+26] <- "Overflow26"
colnames(C18Positive_Unclean2)[C18pos_lastcol+27] <- "Overflow27"

colnames(C18Positive_Unclean2) #check that no NAs remain#

C18Positive_Unclean2_colnames <- C18Positive_Unclean2 %>% subset(select=-c(64:90))%>% colnames()  #get column names of full df as string#

C18Positive_Unclean_Overflow <- C18Positive_Unclean2 %>% filter(Overflow != 'NA')

#filter for the misformated data. use the total number of rows here to later confirm that all overflowed data has been addressed#
result <- data.frame( #check which columns have the overflow#
  Results = names(C18Positive_Unclean2),
  Totals = sapply(C18Positive_Unclean2, function(x) length(grep(".", x)))
)
rownames(result) <- NULL

C18Positive_Unclean_Overflow1 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow2) & Overflow!= 'NA')
C18Positive_Unclean_Overflow3 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow4) & Overflow3!= 'NA')

C18Positive_Unclean_Overflow7 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow8) & Overflow7!= 'NA')
C18Positive_Unclean_Overflow8 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow9) & Overflow8!= 'NA')
C18Positive_Unclean_Overflow9 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow10) & Overflow9!= 'NA')
C18Positive_Unclean_Overflow10 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow11) & Overflow10!= 'NA')
C18Positive_Unclean_Overflow11 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow12) & Overflow11!= 'NA')
C18Positive_Unclean_Overflow12 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow13) & Overflow12!= 'NA')
C18Positive_Unclean_Overflow13 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow14) & Overflow13!= 'NA')
C18Positive_Unclean_Overflow14 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow15) & Overflow14!= 'NA')
C18Positive_Unclean_Overflow15 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow16) & Overflow15!= 'NA')
C18Positive_Unclean_Overflow16 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow17) & Overflow16!= 'NA')
C18Positive_Unclean_Overflow17 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow18) & Overflow17!= 'NA')
C18Positive_Unclean_Overflow18 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow19) & Overflow18!= 'NA')
C18Positive_Unclean_Overflow19 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow20) & Overflow19!= 'NA')
C18Positive_Unclean_Overflow20 <- C18Positive_Unclean_Overflow %>% filter(is.na(Overflow21) & Overflow20!= 'NA')
C18Positive_Unclean_Overflow27 <- C18Positive_Unclean_Overflow %>% filter(Overflow27!= 'NA')

C18Positive_Unclean_Overflow1$Metabolite_name <- apply(C18Positive_Unclean_Overflow1[,4:5], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow3$Metabolite_name <- apply(C18Positive_Unclean_Overflow3[,4:7], 1, paste, collapse = "") #paste overflowing columns together#

C18Positive_Unclean_Overflow7$Metabolite_name <- apply(C18Positive_Unclean_Overflow7[,4:11], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow8$Metabolite_name <- apply(C18Positive_Unclean_Overflow8[,4:12], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow9$Metabolite_name <- apply(C18Positive_Unclean_Overflow9[,4:13], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow10$Metabolite_name <- apply(C18Positive_Unclean_Overflow10[,4:14], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow11$Metabolite_name <- apply(C18Positive_Unclean_Overflow11[,4:15], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow12$Metabolite_name <- apply(C18Positive_Unclean_Overflow12[,4:16], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow13$Metabolite_name <- apply(C18Positive_Unclean_Overflow13[,4:17], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow14$Metabolite_name <- apply(C18Positive_Unclean_Overflow14[,4:18], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow15$Metabolite_name <- apply(C18Positive_Unclean_Overflow15[,4:19], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow16$Metabolite_name <- apply(C18Positive_Unclean_Overflow16[,4:20], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow17$Metabolite_name <- apply(C18Positive_Unclean_Overflow17[,4:21], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow18$Metabolite_name <- apply(C18Positive_Unclean_Overflow18[,4:22], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow19$Metabolite_name <- apply(C18Positive_Unclean_Overflow19[,4:23], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow20$Metabolite_name <- apply(C18Positive_Unclean_Overflow20[,4:24], 1, paste, collapse = "") #paste overflowing columns together#
C18Positive_Unclean_Overflow27$Metabolite_name <- apply(C18Positive_Unclean_Overflow27[,4:31], 1, paste, collapse = "") #paste overflowing columns together#

C18pos_overflowtype1_cleaned <- subset(C18Positive_Unclean_Overflow1, select = -c(5)) #remove the columns that had overflow#
C18pos_overflowtype3_cleaned <- subset(C18Positive_Unclean_Overflow3, select = -c(5:7)) #remove the columns that had overflow#

C18pos_overflowtype7_cleaned <- subset(C18Positive_Unclean_Overflow7, select = -c(5:11)) #remove the columns that had overflow#
C18pos_overflowtype8_cleaned <- subset(C18Positive_Unclean_Overflow8, select = -c(5:12))
C18pos_overflowtype9_cleaned <- subset(C18Positive_Unclean_Overflow9, select = -c(5:13))
C18pos_overflowtype10_cleaned <- subset(C18Positive_Unclean_Overflow10, select = -c(5:14))
C18pos_overflowtype11_cleaned <- subset(C18Positive_Unclean_Overflow11, select = -c(5:15))
C18pos_overflowtype12_cleaned <- subset(C18Positive_Unclean_Overflow12, select = -c(5:16))
C18pos_overflowtype13_cleaned <- subset(C18Positive_Unclean_Overflow13, select = -c(5:17))
C18pos_overflowtype14_cleaned <- subset(C18Positive_Unclean_Overflow14, select = -c(5:18))
C18pos_overflowtype15_cleaned <- subset(C18Positive_Unclean_Overflow15, select = -c(5:19))
C18pos_overflowtype16_cleaned <- subset(C18Positive_Unclean_Overflow16, select = -c(5:20))
C18pos_overflowtype17_cleaned <- subset(C18Positive_Unclean_Overflow17, select = -c(5:21))
C18pos_overflowtype18_cleaned <- subset(C18Positive_Unclean_Overflow18, select = -c(5:22))
C18pos_overflowtype19_cleaned <- subset(C18Positive_Unclean_Overflow19, select = -c(5:23))
C18pos_overflowtype20_cleaned <- subset(C18Positive_Unclean_Overflow20, select = -c(5:24))
C18pos_overflowtype27_cleaned <- subset(C18Positive_Unclean_Overflow27, select = -c(5:31))

names(C18pos_overflowtype1_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype1_cleaned <- C18pos_overflowtype1_cleaned[!is.na(names(C18pos_overflowtype1_cleaned))]

names(C18pos_overflowtype3_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype3_cleaned <- C18pos_overflowtype3_cleaned[!is.na(names(C18pos_overflowtype3_cleaned))]

names(C18pos_overflowtype7_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype7_cleaned <- C18pos_overflowtype7_cleaned[!is.na(names(C18pos_overflowtype7_cleaned))]

names(C18pos_overflowtype8_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype8_cleaned <- C18pos_overflowtype8_cleaned[!is.na(names(C18pos_overflowtype8_cleaned))]

names(C18pos_overflowtype9_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype9_cleaned <- C18pos_overflowtype9_cleaned[!is.na(names(C18pos_overflowtype9_cleaned))]

names(C18pos_overflowtype10_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype10_cleaned <- C18pos_overflowtype10_cleaned[!is.na(names(C18pos_overflowtype10_cleaned))]

names(C18pos_overflowtype11_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype11_cleaned <- C18pos_overflowtype11_cleaned[!is.na(names(C18pos_overflowtype11_cleaned))]

names(C18pos_overflowtype12_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype12_cleaned <- C18pos_overflowtype12_cleaned[!is.na(names(C18pos_overflowtype12_cleaned))]

names(C18pos_overflowtype13_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype13_cleaned <- C18pos_overflowtype13_cleaned[!is.na(names(C18pos_overflowtype13_cleaned))]

names(C18pos_overflowtype14_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype14_cleaned <- C18pos_overflowtype14_cleaned[!is.na(names(C18pos_overflowtype14_cleaned))]

names(C18pos_overflowtype15_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype15_cleaned <- C18pos_overflowtype15_cleaned[!is.na(names(C18pos_overflowtype15_cleaned))]

names(C18pos_overflowtype16_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype16_cleaned <- C18pos_overflowtype16_cleaned[!is.na(names(C18pos_overflowtype16_cleaned))]

names(C18pos_overflowtype17_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype17_cleaned <- C18pos_overflowtype17_cleaned[!is.na(names(C18pos_overflowtype17_cleaned))]

names(C18pos_overflowtype18_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype18_cleaned <- C18pos_overflowtype18_cleaned[!is.na(names(C18pos_overflowtype18_cleaned))]

names(C18pos_overflowtype19_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype19_cleaned <- C18pos_overflowtype19_cleaned[!is.na(names(C18pos_overflowtype19_cleaned))]

names(C18pos_overflowtype20_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype20_cleaned <- C18pos_overflowtype20_cleaned[!is.na(names(C18pos_overflowtype20_cleaned))]

names(C18pos_overflowtype27_cleaned) <- c(C18Positive_Unclean2_colnames) #rename using column names of full df#
C18pos_overflowtype27_cleaned <- C18pos_overflowtype27_cleaned[!is.na(names(C18pos_overflowtype27_cleaned))]

C18pos_not_overflow <- C18Positive_Unclean2 %>% filter(is.na(Overflow)) %>% subset(select = -c(64:90)) #select overflow columns#

C18pos_overflow_cleaned <- rbind(C18pos_overflowtype1_cleaned, C18pos_overflowtype3_cleaned, C18pos_overflowtype7_cleaned, C18pos_overflowtype8_cleaned, C18pos_overflowtype9_cleaned,
                                 C18pos_overflowtype10_cleaned, C18pos_overflowtype11_cleaned, C18pos_overflowtype12_cleaned, C18pos_overflowtype13_cleaned, C18pos_overflowtype14_cleaned,
                                 C18pos_overflowtype15_cleaned, C18pos_overflowtype16_cleaned, C18pos_overflowtype17_cleaned, C18pos_overflowtype18_cleaned, C18pos_overflowtype19_cleaned,
                                 C18pos_overflowtype20_cleaned, C18pos_overflowtype27_cleaned)

C18Positive_CLEAN <- rbind(C18pos_overflow_cleaned, C18pos_not_overflow)  %>%
  dplyr::rename(P15_D1_001 = JOQT31508_10) %>%
  dplyr::rename(P15_D2_001 = JOQT31508_11) %>%
  dplyr::rename(P15_D3_001 = JOQT31508_12) %>%
  dplyr::rename(P16_B_001 = JOQT31508_13) %>%
  dplyr::rename(P16_D1_001 = JOQT31508_14) %>%
  dplyr::rename(P16_D2_001 = JOQT31508_15) %>%
  dplyr::rename(P16_D3_001 = JOQT31508_16) %>%
  dplyr::rename(P12_B_001 = JOQT31508_1) %>%
  dplyr::rename(P12_D1_001 = JOQT31508_2) %>%
  dplyr::rename(P12_D2_001 = JOQT31508_3) %>%
  dplyr::rename(P12_D3_001 = JOQT31508_4) %>%
  dplyr::rename(P14_B_001 = JOQT31508_5) %>%
  dplyr::rename(P14_D1_001 = JOQT31508_6) %>%
  dplyr::rename(P14_D2_001 = JOQT31508_7) %>%
  dplyr::rename(P14_D3_001 = JOQT31508_8) %>%
  dplyr::rename(P15_B_001 = JOQT31508_9) %>%
  dplyr::rename(Blank_1_1 = JOQT31508_Blank_1_1) %>%
  dplyr::rename(Blank_1 = JOQT31508_Blank_1) %>%
  dplyr::rename(Blank_2_1 = JOQT31508_Blank_2_1) %>%
  dplyr::rename(Blank_2 = JOQT31508_Blank_2) %>%
  dplyr::rename(QCB_001 = JOQT31508_QC_Blank_1) %>%
  dplyr::rename(QCB_002 = JOQT31508_QC_Blank) %>%
  dplyr::rename(QCP_001 = JOQT31508_QC_Pooled_2) %>%
  dplyr::rename(QCP_002 = JOQT31508_QC_Pooled) %>%
  dplyr::rename(Spk1_001 = JOQT31508_Spike_Blank_1) %>%
  dplyr::rename(Spk2_002 = JOQT31508_Spike_Blank_2)

C18Positive_CLEAN[,'Type'] = 'C18_Positive' #set type as additional column#

#Clean Hilic Negative#######
Hilicnegative_Unclean1 <- HILICNegative[-c(1:3),] #create new df to edit and remove unwanted rows#
names(Hilicnegative_Unclean1) <- Hilicnegative_Unclean1[1,] #make first row the column names#
Hilicnegative_Unclean2 <- Hilicnegative_Unclean1[-c(1),] #remove duplicate first row#

names(Hilicnegative_Unclean2) <- gsub(" ", "_", names(Hilicnegative_Unclean2)) #replace spaces in column names with underscore#
names(Hilicnegative_Unclean2) <- gsub("/", ".", names(Hilicnegative_Unclean2)) #replace slashes in column names with period#
names(Hilicnegative_Unclean2) <- gsub("-", "_", names(Hilicnegative_Unclean2)) #replace dashes in column names with underscore#
names(Hilicnegative_Unclean2) <- gsub("%", "percent", names(Hilicnegative_Unclean2))

colnames(Hilicnegative_Unclean2)[62] <- "Sample.Avg" #these columns have duplicate names. one is sd and one is mean.#
colnames(Hilicnegative_Unclean2)[63] <- "Sample.Std"

Hilicneg_lastcol <- match("Sample.Std",names(Hilicnegative_Unclean2))
colnames(Hilicnegative_Unclean2)[Hilicneg_lastcol+1] <- "Overflow"
colnames(Hilicnegative_Unclean2)[Hilicneg_lastcol+2] <- "Overflow2"
colnames(Hilicnegative_Unclean2)[Hilicneg_lastcol+3] <- "Overflow3"
colnames(HILICNegative)
Hilicnegative_Unclean2_colnames <- Hilicnegative_Unclean2 %>% subset(select=-c(64:66))%>% colnames()  #get column names of full df as string#

Hilicnegative_Unclean_Overflow <- Hilicnegative_Unclean2 %>% filter(Overflow != 'NA')

#filter for the misformated data. use the total number of rows here to later confirm that all overflowed data has been addressed#
result2 <- data.frame( #check which columns have the overflow#
  Results = names(Hilicnegative_Unclean_Overflow),
  Totals = sapply(Hilicnegative_Unclean_Overflow, function(x) length(grep(".", x)))
)
rownames(result2) <- NULL

Hilicnegative_Unclean_Overflow1 <- Hilicnegative_Unclean_Overflow %>% filter(is.na(Overflow2))
Hilicnegative_Unclean_Overflow3 <- Hilicnegative_Unclean_Overflow %>% filter(Overflow3 != 'NA')

Hilicnegative_Unclean_Overflow1$Metabolite_name <- apply(Hilicnegative_Unclean_Overflow1[,4:5], 1, paste, collapse = "") #paste overflowing columns together#
Hilicnegative_Unclean_Overflow3$Metabolite_name <- apply(Hilicnegative_Unclean_Overflow3[,4:7], 1, paste, collapse = "") #paste overflowing columns together#

Hilicneg_overflowtype1_cleaned <- subset(Hilicnegative_Unclean_Overflow1, select = -c(5)) #remove the columns that had overflow#
Hilicneg_overflowtype3_cleaned <- subset(Hilicnegative_Unclean_Overflow3, select = -c(5:7)) #remove the columns that had overflow#

names(Hilicneg_overflowtype1_cleaned) <- c(Hilicnegative_Unclean2_colnames) #rename using column names of full df#
Hilicneg_overflowtype1_cleaned <- Hilicneg_overflowtype1_cleaned[!is.na(names(Hilicneg_overflowtype1_cleaned))]

names(Hilicneg_overflowtype3_cleaned) <- c(Hilicnegative_Unclean2_colnames) #rename using column names of full df#
Hilicneg_overflowtype3_cleaned <- Hilicneg_overflowtype3_cleaned[!is.na(names(Hilicneg_overflowtype3_cleaned))]

Hilicneg_not_overflow <-Hilicnegative_Unclean2 %>% filter(is.na(Overflow)) %>% subset(select = -c(64:66))
Hilicneg_overflow_cleaned <- rbind(Hilicneg_overflowtype1_cleaned, Hilicneg_overflowtype3_cleaned)

Hilicneg_CLEAN <- rbind(Hilicneg_not_overflow, Hilicneg_overflow_cleaned)  %>%
  dplyr::rename(P15_D1_001 = JOQT31508_10c) %>% #rename to harmonize column names with orbi data#
  dplyr::rename(P15_D2_001 = JOQT31508_11c) %>%
  dplyr::rename(P15_D3_001 = JOQT31508_12c) %>%
  dplyr::rename(P16_B_001 = JOQT31508_13c) %>%
  dplyr::rename(P16_D1_001 = JOQT31508_14c) %>%
  dplyr::rename(P16_D2_001 = JOQT31508_15c) %>%
  dplyr::rename(P16_D3_001 = JOQT31508_16c) %>%
  dplyr::rename(P12_B_001 = JOQT31508_1c) %>%
  dplyr::rename(P12_D1_001 = JOQT31508_2c) %>%
  dplyr::rename(P12_D2_001 = JOQT31508_3c) %>%
  dplyr::rename(P12_D3_001 = JOQT31508_4c) %>%
  dplyr::rename(P14_B_001 = JOQT31508_5c) %>%
  dplyr::rename(P14_D1_001 = JOQT31508_6c) %>%
  dplyr::rename(P14_D2_001 = JOQT31508_7c) %>%
  dplyr::rename(P14_D3_001 = JOQT31508_8c) %>%
  dplyr::rename(P15_B_001 = JOQT31508_9c) %>%
  dplyr::rename(Blank_1_1 = JOQT31508_Blank_1_1c) %>%
  dplyr::rename(Blank_1 = JOQT31508_Blank_1c) %>%
  dplyr::rename(Blank_2_1 = JOQT31508_Blank_2_1c) %>%
  dplyr::rename(Blank_2 = JOQT31508_Blank_2c) %>%
  dplyr::rename(QCB_001 = JOQT31508_QC_Blank_1c) %>%
  dplyr::rename(QCB_002 = JOQT31508_QC_Blankc) %>%
  dplyr::rename(QCP_001 = JOQT31508_QC_Pooled_2c) %>%
  dplyr::rename(QCP_002 = JOQT31508_QC_Pooledc) %>%
  dplyr::rename(Spk1_001 = JOQT31508_Spike_Blank_1c) %>%
  dplyr::rename(Spk2_002 = JOQT31508_Spike_Blank_2c)

Hilicneg_CLEAN[,'Type'] = 'Hilic_Negative' #set type as additional column#

test <- Hilicneg_CLEAN %>% filter(Post_curation_result != 'null') #make sure all alignments were done properly. tst should. have 0 rows.

##Clean Hilic positive##
Hilicpositive_Unclean1 <- HILICPositive[-c(1:3),] #create new df to edit and remove unwanted rows#
names(Hilicpositive_Unclean1) <- Hilicpositive_Unclean1[1,] #make first row the column names#
Hilicpositive_Unclean2 <- Hilicpositive_Unclean1[-c(1),] #remove duplicate first row#

names(Hilicpositive_Unclean2) <- gsub(" ", "_", names(Hilicpositive_Unclean2)) #replace spaces in column names with underscore#
names(Hilicpositive_Unclean2) <- gsub("/", ".", names(Hilicpositive_Unclean2)) #replace slashes in column names with period#
names(Hilicpositive_Unclean2) <- gsub("-", "_", names(Hilicpositive_Unclean2)) #replace dashes in column names with underscore#
names(Hilicpositive_Unclean2) <- gsub("%", "percent", names(Hilicpositive_Unclean2))

colnames(Hilicpositive_Unclean2)[62] <- "Sample.Avg" #these columns have duplicate names. one is sd and one is mean.#
colnames(Hilicpositive_Unclean2)[63] <- "Sample.Std"

Hilicpos_lastcol <- match("Sample.Std",names(Hilicpositive_Unclean2))
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+1] <- "Overflow"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+2] <- "Overflow2"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+3] <- "Overflow3"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+4] <- "Overflow4"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+5] <- "Overflow5"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+6] <- "Overflow6"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+7] <- "Overflow7"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+8] <- "Overflow8"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+9] <- "Overflow9"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+10] <- "Overflow10"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+11] <- "Overflow11"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+12] <- "Overflow12"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+13] <- "Overflow13"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+14] <- "Overflow14"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+15] <- "Overflow15"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+16] <- "Overflow16"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+17] <- "Overflow17"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+18] <- "Overflow18"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+19] <- "Overflow19"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+20] <- "Overflow20"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+21] <- "Overflow21"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+22] <- "Overflow22"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+23] <- "Overflow23"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+24] <- "Overflow24"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+25] <- "Overflow25"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+26] <- "Overflow26"
colnames(Hilicpositive_Unclean2)[Hilicpos_lastcol+27] <- "Overflow27"

Hilicpos_Unclean_Overflow <- Hilicpositive_Unclean2 %>% filter(Overflow != 'NA')

#filter for the misformated data. use the total number of rows here to later confirm that all overflowed data has been addressed#
result5 <- data.frame( #check which columns have the overflow#
  Results = names(Hilicpos_Unclean_Overflow),
  Totals = sapply(Hilicpos_Unclean_Overflow, function(x) length(grep(".", x)))
)
rownames(result5) <- NULL

Hilicpos_Unclean_Overflow1 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow2) & Overflow!= 'NA')
Hilicpos_Unclean_Overflow3 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow4) & Overflow3!= 'NA')
Hilicpos_Unclean_Overflow6 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow7) & Overflow6!= 'NA')
Hilicpos_Unclean_Overflow7 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow8) & Overflow7!= 'NA')
Hilicpos_Unclean_Overflow8<- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow9) & Overflow8!= 'NA')
Hilicpos_Unclean_Overflow9 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow10) & Overflow9!= 'NA')
Hilicpos_Unclean_Overflow10 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow11) & Overflow10!= 'NA')
Hilicpos_Unclean_Overflow11 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow12) & Overflow11!= 'NA')
Hilicpos_Unclean_Overflow12 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow13) & Overflow12!= 'NA')
Hilicpos_Unclean_Overflow13 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow14) & Overflow13!= 'NA')
Hilicpos_Unclean_Overflow14 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow15) & Overflow14!= 'NA')
Hilicpos_Unclean_Overflow15 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow16) & Overflow15!= 'NA')
Hilicpos_Unclean_Overflow16 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow17) & Overflow16!= 'NA')
Hilicpos_Unclean_Overflow17 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow18) & Overflow17!= 'NA')
Hilicpos_Unclean_Overflow18 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow19) & Overflow18!= 'NA')
Hilicpos_Unclean_Overflow19 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow20) & Overflow19!= 'NA')
Hilicpos_Unclean_Overflow20 <- Hilicpos_Unclean_Overflow %>% filter(is.na(Overflow21) & Overflow20!= 'NA')
Hilicpos_Unclean_Overflow27 <- Hilicpos_Unclean_Overflow %>% filter(Overflow27!= 'NA')

Hilicpos_Unclean_Overflow1$Metabolite_name <- apply(Hilicpos_Unclean_Overflow1[,4:5], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow3$Metabolite_name <- apply(Hilicpos_Unclean_Overflow3[,4:7], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow6$Metabolite_name <- apply(Hilicpos_Unclean_Overflow6[,4:10], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow7$Metabolite_name <- apply(Hilicpos_Unclean_Overflow7[,4:11], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow8$Metabolite_name <- apply(Hilicpos_Unclean_Overflow8[,4:12], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow9$Metabolite_name <- apply(Hilicpos_Unclean_Overflow9[,4:13], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow10$Metabolite_name <- apply(Hilicpos_Unclean_Overflow10[,4:14], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow11$Metabolite_name <- apply(Hilicpos_Unclean_Overflow11[,4:15], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow12$Metabolite_name <- apply(Hilicpos_Unclean_Overflow12[,4:16], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow13$Metabolite_name <- apply(Hilicpos_Unclean_Overflow13[,4:17], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow14$Metabolite_name <- apply(Hilicpos_Unclean_Overflow14[,4:18], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow15$Metabolite_name <- apply(Hilicpos_Unclean_Overflow15[,4:19], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow16$Metabolite_name <- apply(Hilicpos_Unclean_Overflow16[,4:20], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow17$Metabolite_name <- apply(Hilicpos_Unclean_Overflow17[,4:21], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow18$Metabolite_name <- apply(Hilicpos_Unclean_Overflow18[,4:22], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow19$Metabolite_name <- apply(Hilicpos_Unclean_Overflow19[,4:23], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow20$Metabolite_name <- apply(Hilicpos_Unclean_Overflow20[,4:24], 1, paste, collapse = "") #paste overflowing columns together#
Hilicpos_Unclean_Overflow27$Metabolite_name <- apply(Hilicpos_Unclean_Overflow27[,4:31], 1, paste, collapse = "") #paste overflowing columns together#

Hilicpos_overflowtype1_cleaned <- subset(Hilicpos_Unclean_Overflow1, select = -c(5)) #remove the columns that had overflow#
Hilicpos_overflowtype3_cleaned <- subset(Hilicpos_Unclean_Overflow3, select = -c(5:7)) #remove the columns that had overflow#
Hilicpos_overflowtype6_cleaned <- subset(Hilicpos_Unclean_Overflow6, select = -c(5:10)) #remove the columns that had overflow#
Hilicpos_overflowtype7_cleaned <- subset(Hilicpos_Unclean_Overflow7, select = -c(5:11)) #remove the columns that had overflow#
Hilicpos_overflowtype8_cleaned <- subset(Hilicpos_Unclean_Overflow8, select = -c(5:12)) #remove the columns that had overflow#
Hilicpos_overflowtype9_cleaned <- subset(Hilicpos_Unclean_Overflow9, select = -c(5:13)) #remove the columns that had overflow#
Hilicpos_overflowtype10_cleaned <- subset(Hilicpos_Unclean_Overflow10, select = -c(5:14)) #remove the columns that had overflow#
Hilicpos_overflowtype11_cleaned <- subset(Hilicpos_Unclean_Overflow11, select = -c(5:15)) #remove the columns that had overflow#
Hilicpos_overflowtype12_cleaned <- subset(Hilicpos_Unclean_Overflow12, select = -c(5:16)) #remove the columns that had overflow#
Hilicpos_overflowtype13_cleaned <- subset(Hilicpos_Unclean_Overflow13, select = -c(5:17)) #remove the columns that had overflow#
Hilicpos_overflowtype14_cleaned <- subset(Hilicpos_Unclean_Overflow14, select = -c(5:18)) #remove the columns that had overflow#
Hilicpos_overflowtype15_cleaned <- subset(Hilicpos_Unclean_Overflow15, select = -c(5:19)) #remove the columns that had overflow#
Hilicpos_overflowtype16_cleaned <- subset(Hilicpos_Unclean_Overflow16, select = -c(5:20)) #remove the columns that had overflow#
Hilicpos_overflowtype17_cleaned <- subset(Hilicpos_Unclean_Overflow17, select = -c(5:21)) #remove the columns that had overflow#
Hilicpos_overflowtype18_cleaned <- subset(Hilicpos_Unclean_Overflow18, select = -c(5:22)) #remove the columns that had overflow#
Hilicpos_overflowtype19_cleaned <- subset(Hilicpos_Unclean_Overflow19, select = -c(5:23)) #remove the columns that had overflow#
Hilicpos_overflowtype20_cleaned <- subset(Hilicpos_Unclean_Overflow20, select = -c(5:24)) #remove the columns that had overflow#
Hilicpos_overflowtype27_cleaned <- subset(Hilicpos_Unclean_Overflow27, select = -c(5:31)) #remove the columns that had overflow#
Hilicpositive_Unclean2_colnames <- Hilicpositive_Unclean2 %>% subset(select = -c(64:90)) %>% colnames() #get column names of full df as string#

names(Hilicpos_overflowtype1_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype1_cleaned <- Hilicpos_overflowtype1_cleaned[!is.na(names(Hilicpos_overflowtype1_cleaned))]

names(Hilicpos_overflowtype3_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype3_cleaned <- Hilicpos_overflowtype3_cleaned[!is.na(names(Hilicpos_overflowtype3_cleaned))]

names(Hilicpos_overflowtype6_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype6_cleaned <- Hilicpos_overflowtype6_cleaned[!is.na(names(Hilicpos_overflowtype6_cleaned))]

names(Hilicpos_overflowtype7_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype7_cleaned <- Hilicpos_overflowtype7_cleaned[!is.na(names(Hilicpos_overflowtype7_cleaned))]

names(Hilicpos_overflowtype8_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype8_cleaned <- Hilicpos_overflowtype8_cleaned[!is.na(names(Hilicpos_overflowtype8_cleaned))]

names(Hilicpos_overflowtype9_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype9_cleaned <- Hilicpos_overflowtype9_cleaned[!is.na(names(Hilicpos_overflowtype9_cleaned))]

names(Hilicpos_overflowtype10_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype10_cleaned <- Hilicpos_overflowtype10_cleaned[!is.na(names(Hilicpos_overflowtype10_cleaned))]

names(Hilicpos_overflowtype11_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype11_cleaned <- Hilicpos_overflowtype11_cleaned[!is.na(names(Hilicpos_overflowtype11_cleaned))]

names(Hilicpos_overflowtype12_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype12_cleaned <- Hilicpos_overflowtype12_cleaned[!is.na(names(Hilicpos_overflowtype12_cleaned))]

names(Hilicpos_overflowtype13_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype13_cleaned <- Hilicpos_overflowtype13_cleaned[!is.na(names(Hilicpos_overflowtype13_cleaned))]

names(Hilicpos_overflowtype14_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype14_cleaned <- Hilicpos_overflowtype14_cleaned[!is.na(names(Hilicpos_overflowtype14_cleaned))]

names(Hilicpos_overflowtype15_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype15_cleaned <- Hilicpos_overflowtype15_cleaned[!is.na(names(Hilicpos_overflowtype15_cleaned))]

names(Hilicpos_overflowtype16_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype16_cleaned <- Hilicpos_overflowtype16_cleaned[!is.na(names(Hilicpos_overflowtype16_cleaned))]

names(Hilicpos_overflowtype17_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype17_cleaned <- Hilicpos_overflowtype17_cleaned[!is.na(names(Hilicpos_overflowtype17_cleaned))]

names(Hilicpos_overflowtype18_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype18_cleaned <- Hilicpos_overflowtype18_cleaned[!is.na(names(Hilicpos_overflowtype18_cleaned))]

names(Hilicpos_overflowtype19_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype19_cleaned <- Hilicpos_overflowtype19_cleaned[!is.na(names(Hilicpos_overflowtype19_cleaned))]

names(Hilicpos_overflowtype20_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype20_cleaned <- Hilicpos_overflowtype20_cleaned[!is.na(names(Hilicpos_overflowtype20_cleaned))]

names(Hilicpos_overflowtype27_cleaned) <- c(Hilicpositive_Unclean2_colnames) #rename using column names of full df#
Hilicpos_overflowtype27_cleaned <- Hilicpos_overflowtype27_cleaned[!is.na(names(Hilicpos_overflowtype27_cleaned))]

Hilicpos_not_overflow <-Hilicpositive_Unclean2 %>% filter(is.na(Overflow)) %>% subset(select = -c(64:90))
Hilicpos_overflow_cleaned <- rbind(Hilicpos_overflowtype1_cleaned, Hilicpos_overflowtype3_cleaned, Hilicpos_overflowtype6_cleaned, Hilicpos_overflowtype7_cleaned, Hilicpos_overflowtype8_cleaned, Hilicpos_overflowtype9_cleaned,
                                   Hilicpos_overflowtype10_cleaned, Hilicpos_overflowtype11_cleaned, Hilicpos_overflowtype12_cleaned, Hilicpos_overflowtype13_cleaned,
                                   Hilicpos_overflowtype14_cleaned, Hilicpos_overflowtype15_cleaned, Hilicpos_overflowtype16_cleaned, Hilicpos_overflowtype17_cleaned, Hilicpos_overflowtype18_cleaned,
                                   Hilicpos_overflowtype19_cleaned, Hilicpos_overflowtype20_cleaned, Hilicpos_overflowtype27_cleaned)

Hilicpos_CLEAN <- rbind(Hilicpos_not_overflow, Hilicpos_overflow_cleaned) %>%
  dplyr::rename(P15_D1_001 = JOQT31508_10b) %>%
  dplyr::rename(P15_D2_001 = JOQT31508_11b) %>%
  dplyr::rename(P15_D3_001 = JOQT31508_12b) %>%
  dplyr::rename(P16_B_001 = JOQT31508_13b) %>%
  dplyr::rename(P16_D1_001 = JOQT31508_14b) %>%
  dplyr::rename(P16_D2_001 = JOQT31508_15b) %>%
  dplyr::rename(P16_D3_001 = JOQT31508_16b) %>%
  dplyr::rename(P12_B_001 = JOQT31508_1b) %>%
  dplyr::rename(P12_D1_001 = JOQT31508_2b) %>%
  dplyr::rename(P12_D2_001 = JOQT31508_3b) %>%
  dplyr::rename(P12_D3_001 = JOQT31508_4b) %>%
  dplyr::rename(P14_B_001 = JOQT31508_5b) %>%
  dplyr::rename(P14_D1_001 = JOQT31508_6b) %>%
  dplyr::rename(P14_D2_001 = JOQT31508_7b) %>%
  dplyr::rename(P14_D3_001 = JOQT31508_8b) %>%
  dplyr::rename(P15_B_001 = JOQT31508_9b) %>%
  dplyr::rename(Blank_1_1 = JOQT31508_Blank_1_1b) %>%
  dplyr::rename(Blank_1 = JOQT31508_Blank_1b) %>%
  dplyr::rename(Blank_2_1 = JOQT31508_Blank_2_1b) %>%
  dplyr::rename(Blank_2 = JOQT31508_Blank_2b) %>%
  dplyr::rename(QCB_001 = JOQT31508_QC_Blank_1b) %>%
  dplyr::rename(QCB_002 = JOQT31508_QC_Blankb) %>%
  dplyr::rename(QCP_001 = JOQT31508_QC_Pooled_2b) %>%
  dplyr::rename(QCP_002 = JOQT31508_QC_Pooledb) %>%
  dplyr::rename(Spk1_001 = JOQT31508_Spike_Blank_1b) %>%
  dplyr::rename(Spk2_002 = JOQT31508_Spike_Blank_2b)

Hilicpos_CLEAN[,'Type'] = 'Hilic_Positive' #set type as additional column#

#Clean EI#######
##Remove 4 rows
EIPositiveUnclean1 <- EIPositive

names(EIPositiveUnclean1) <- EIPositiveUnclean1[1,] #make first row the column names#
EI_Unclean2 <- EIPositiveUnclean1[-c(1),] #remove duplicate first row#

EI_row <- data.frame( #check which columns have the overflow#
  Results = names(EI_Unclean2),
  Totals = sapply(EI_Unclean2, function(x) length(grep(".", x)))
)
rownames(EI_row) <- NULL

names(EI_Unclean2) <- gsub(" ", "_", names(EI_Unclean2)) #replace spaces in column names with underscore#
names(EI_Unclean2) <- gsub("/", ".", names(EI_Unclean2)) #replace slashes in column names with period#
names(EI_Unclean2) <- gsub("-", "_", names(EI_Unclean2)) #replace dashes in column names with underscore#
names(EI_Unclean2) <- gsub("%", "percent", names(EI_Unclean2))
colnames(EI_Unclean2)

EI_CLEAN <- EI_Unclean2 %>% subset(select = -(59:64)) %>%
  dplyr::rename(P15_D1_001 = JOQT31508_10) %>% #rename to harmonize column names with orbi data#
  dplyr::rename(P15_D2_001 = JOQT31508_11) %>%
  dplyr::rename(P15_D3_001 = JOQT31508_12) %>%
  dplyr::rename(P16_B_001 = JOQT31508_13) %>%
  dplyr::rename(P16_D1_001 = JOQT31508_14) %>%
  #dplyr::rename(P16_D2_001 = JOQT31508_15) %>%#
  dplyr::rename(P16_D3_001 = JOQT31508_16) %>%
  dplyr::rename(P12_B_001 = JOQT31508_1) %>%
  dplyr::rename(P12_D1_001 = JOQT31508_2) %>%
  dplyr::rename(P12_D2_001 = JOQT31508_3) %>%
  dplyr::rename(P12_D3_001 = JOQT31508_4) %>%
  dplyr::rename(P14_B_001 = JOQT31508_5) %>%
  dplyr::rename(P14_D1_001 = JOQT31508_6) %>%
  dplyr::rename(P14_D2_001 = JOQT31508_7) %>%
  dplyr::rename(P14_D3_001 = JOQT31508_8) %>%
  dplyr::rename(P15_B_001 = JOQT31508_9) %>%
  dplyr::rename(Blank_1_1 = JOQT31508_Blank_1_1) %>%
  # dplyr::rename(Blank_1 = JOQT31508_Blank_1) %>%#
  #dplyr::rename(Blank_2_1 = JOQT31508_Blank_2_1) %>%#
  #dplyr::rename(Blank_2 = JOQT31508_Blank_2) %>%#
  #dplyr::rename(QCB_001 = JOQT31508_QC_Blank_1) %>%#
  dplyr::rename(QCB_002 = JOQT31508_QC_Blank) %>%
  #dplyr::rename(QCP_001 = JOQT31508_QC_Pooled_2) %>%#
  dplyr::rename(QCP_002 = JOQT31508_QC_Pooled) %>%
  dplyr::rename(Spk1_001 = JOQT31508_Spike_Blank_1) %>%
  dplyr::rename(Spk2_002 = JOQT31508_Spike_Blank_2)

EI_CLEAN[,'Type'] = 'EI' #set type as additional column#

colnames(EI_CLEAN)
#bind all non-targeted data into one dataframe######
replace_lod_initial <- function(df){ #note: this function relies on participant sample names starting with P1, QC for QC samples, Spk for spike samples and JOQ for other#
  #this function can be edited if sample names vary#
  #replacing with lod may be necessary again later on, but don't use this function for that#
  var_name <- colnames(df[,grep("P1", colnames(df))]) #set variable for participant samples#
  var_name2 <- colnames(df[,grep("JOQ", colnames(df))]) #set variable for other sample columns#
  var_name3 <- colnames(df[,grep("QC", colnames(df))]) #set variable for QC sample columns#
  var_namespk <- colnames(df[,grep("Spk", colnames(df))]) #set variable for spike samples#
  df[var_name][df[var_name]==0] <- NA #make all 0s NAs in participant samples#
  df[var_name2][df[var_name2]==0] <- NA #make all 0s NAs in other samples#
  df[var_name3][df[var_name3]==0] <- NA #make all 0s NAs in QC samples#
  df[var_namespk][df[var_namespk]==0] <- NA #make all 0s NA in spike samples#
  df<-df %>%
    dplyr::mutate(across(all_of(var_name), as.numeric)) %>% #change all sample columns to numeric. MUST ADD DPLYR OR IT DOESN'T WORK. this is needed to do calculations#
    dplyr::mutate(across(all_of(var_name2), as.numeric)) %>%
    dplyr::mutate(across(all_of(var_name3), as.numeric)) %>%
    dplyr::mutate(across(all_of(var_namespk), as.numeric))
  index2 <- match("Sample.Avg",names(df)) #create variable for column index for the end of sample columns#
  index3 <- match("MS.MS_spectrum",names(df)) #create variable for column index for the start of sample columns#
  start_sample_column = index3 + 1
  end_sample_column = index2-1
  df[, "non_zero_min"] <- apply(df[start_sample_column:end_sample_column], 1, min, na.rm = TRUE) #get minimum value across sample columns into a new column, ignoring NAs#  df <- df %>%
  df <- df %>% mutate(non_zero_min = as.numeric(non_zero_min))%>% #turn the non-zero-min column into numeric#
    mutate(non_zero_min = non_zero_min/5) #get LOD and enter it in the non_zero_min column#
  for (var in var_name){
    df[[var]][is.na(df[[var]])] <- df$non_zero_min[is.na(df[[var]])]
  }#turn NAs to LOD#
  for (var in var_name2){
    df[[var]][is.na(df[[var]])] <- df$non_zero_min[is.na(df[[var]])]
  }#turn NAs to LOD#
  for (var in var_name3){
    df[[var]][is.na(df[[var]])] <- df$non_zero_min[is.na(df[[var]])]
  }#turn NAs to LOD#
  for (var in var_namespk){
    df[[var]][is.na(df[[var]])] <- df$non_zero_min[is.na(df[[var]])]
  }#turn NAs to LOD#
  df
}

CLEAN_MERGED_ALL_WITH_DUPS_initial_wide <- dplyr::bind_rows(Hilicpos_CLEAN, Hilicneg_CLEAN, C18Positive_CLEAN, C18neg_CLEAN, EI_CLEAN)
CLEAN_MERGED_ALL_WITH_DUPS_initial <- CLEAN_MERGED_ALL_WITH_DUPS_initial_wide %>% #bind rows because EI is missing some columns
  pivot_longer(cols = c(P15_D1_001:P15_B_001, QCB_001:Spk2_002), values_to = 'Uncorrected_Area', names_to = 'Sample') %>%
  mutate(Uncorrected_Area = ifelse(is.na(Uncorrected_Area), 0, Uncorrected_Area))
####Normalize non-targeted data to internal standard######
CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1 <- merge(All_ISTDs_with_EI, CLEAN_MERGED_ALL_WITH_DUPS_initial, by = c('Sample', 'Type'), all = TRUE)

CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1$Uncorrected_Area <-as.numeric(CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1$Uncorrected_Area)
CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1$ISTD <-as.numeric(CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1$ISTD)

CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1[,'ISTD_Corrected_Value'] = CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1$Uncorrected_Area/CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1$ISTD

Dups_Per_Method_ISTD_Corrected <- CLEAN_MERGED_ALL_WITH_DUPS_initial_merged1 %>%
  subset(select = -c(Uncorrected_Area, ISTD, ISTD_Area)) %>% #remove columns no longer needed
  mutate(ISTD_Corrected_Value = ifelse(is.na(ISTD_Corrected_Value), 0, ISTD_Corrected_Value)) %>% #change thigns without ISTDs to 0
  pivot_wider(values_from = ISTD_Corrected_Value, names_from = Sample)
Dups_Per_Method_ISTD_Corrected

######add general use columns#####
Dups_Per_Method_ISTD_Corrected$Sample.Avg = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$Sample.Avg)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$Average_Mz = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$Average_Mz)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$QCP_001 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$QCP_001)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$QCB_002 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$QCB_002)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$QCB_001 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$QCB_001)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$QCP_002 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$QCP_002)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$Blank_1_1 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$Blank_1_1)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$Blank_1 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$Blank_1)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$Blank_2_1 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$Blank_2_1)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$Blank_2 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$Blank_2)) #turn to numeric#
Dups_Per_Method_ISTD_Corrected$Spk1_001 = as.numeric(as.character(Dups_Per_Method_ISTD_Corrected$Spk1_001)) #turn to numeric#

#calculate average for pooled and blank of LC and GC separately because of fewer runs in GC#
CLEAN_MERGED_ALL_WITH_DUPS1_LC <- Dups_Per_Method_ISTD_Corrected %>% filter(Type != 'EI')
CLEAN_MERGED_ALL_WITH_DUPS1_LC[,'QC_Pooled_Avg'] = (CLEAN_MERGED_ALL_WITH_DUPS1_LC$QCP_002+CLEAN_MERGED_ALL_WITH_DUPS1_LC$QCP_001)/2
CLEAN_MERGED_ALL_WITH_DUPS1_LC[,'QC_Blank_Avg'] = (CLEAN_MERGED_ALL_WITH_DUPS1_LC$QCB_002+CLEAN_MERGED_ALL_WITH_DUPS1_LC$QCB_001)/2
CLEAN_MERGED_ALL_WITH_DUPS1_LC[,'Solvent_Blank_Avg'] = (CLEAN_MERGED_ALL_WITH_DUPS1_LC$Spk1_001+CLEAN_MERGED_ALL_WITH_DUPS1_LC$Spk2_002)/2 #these are spiked method blanks#

CLEAN_MERGED_ALL_WITH_DUPS1_GC <- Dups_Per_Method_ISTD_Corrected %>% filter(Type == 'EI')
CLEAN_MERGED_ALL_WITH_DUPS1_GC[,'QC_Pooled_Avg'] = CLEAN_MERGED_ALL_WITH_DUPS1_GC$QCP_002
CLEAN_MERGED_ALL_WITH_DUPS1_GC[,'QC_Blank_Avg'] = CLEAN_MERGED_ALL_WITH_DUPS1_GC$QCB_002
CLEAN_MERGED_ALL_WITH_DUPS1_GC[,'Solvent_Blank_Avg'] = CLEAN_MERGED_ALL_WITH_DUPS1_GC$Spk2_002 #only spk2_002 - spk1 doesn't have istd#

CLEAN_MERGED_ALL_WITH_DUPS1_Corrected <- rbind(CLEAN_MERGED_ALL_WITH_DUPS1_GC, CLEAN_MERGED_ALL_WITH_DUPS1_LC) #merge LC and GC back together
#NAs are 0/0

# Convert columns with the string in their names into numeric
columns_to_convert <- grep("P1", names(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected), value = TRUE)

# Convert those columns to numeric
CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[columns_to_convert] <- lapply(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[columns_to_convert], as.numeric)
######get general use columns including QC pooled subtracting QC unworn for later duplicate removal#####
#change 0s to NA for all QC and participant columsn#
var_name_P <- colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[,grep("P1", colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected))]) #create variable names to use later for any column with these strings in the name
var_name_qcp <- colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[,grep("QCP", colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected))])
var_name_qcb <- colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[,grep("QCB", colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected))])

CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[var_name_P][CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[var_name_P]==0] <- NA #set any avalues in the previously defined columns that are equal 0 as NA. this is to get the LOD wihtout issue#
CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[var_name_qcp][CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[var_name_qcp]==0] <- NA
CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[var_name_qcb][CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[var_name_qcb]==0] <- NA
CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[, "LOD_pre_solv_sub"] <- apply(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[, which(colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected)=="P12_B_001"):which(colnames(CLEAN_MERGED_ALL_WITH_DUPS1_Corrected)=="QCP_002")], 1, min, na.rm = TRUE)/5 #get LOD for solvent subtraction
CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[, 'QC_Pool_Blank_Sub'] = CLEAN_MERGED_ALL_WITH_DUPS1_Corrected$QC_Pooled_Avg - CLEAN_MERGED_ALL_WITH_DUPS1_Corrected$QC_Blank_Avg
CLEAN_MERGED_ALL_WITH_DUPS1_Corrected[, 'QC_Pool_Blank_Ratio'] = CLEAN_MERGED_ALL_WITH_DUPS1_Corrected$QC_Blank_Avg/CLEAN_MERGED_ALL_WITH_DUPS1_Corrected$QC_Pooled_Avg

#replace blank subtracted QCP with LOD if negative & filter out features only detected in solvent#
CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected <- CLEAN_MERGED_ALL_WITH_DUPS1_Corrected %>%
  filter(LOD_pre_solv_sub != Inf) %>% #these were only detected in the solvent blank and not the method blank or any samples/blank#
  mutate(QC_Pool_Blank_Sub = ifelse(QC_Pool_Blank_Sub < 0, 0, QC_Pool_Blank_Sub)) %>%
  mutate(QC_Pool_Blank_Sub = ifelse(QC_Pool_Blank_Sub == 0, 0, QC_Pool_Blank_Sub)) %>%
  mutate(QC_Pool_Blank_Sub = ifelse(is.na(QC_Pool_Blank_Sub), 0, QC_Pool_Blank_Sub)) # these were non-detects before solvent subtraction

#######number of features before duplicate removal
nrow(CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected)

CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected$m.z_similarity = as.numeric(as.character(CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected$m.z_similarity)) #turn the m.z similarity column to numeric#
#######get LOD after solvent subtraction#######
Solv_Sub_LOD <- CLEAN_MERGED_ALL_WITH_DUPS1_Corrected %>%  #NAs are 0s
  subset(select = c(Type:Metabolite_name, m.z_similarity, Total_score, SMILES, INCHIKEY, P12_B_001:QCP_002, QC_Pooled_Avg:QC_Pool_Blank_Ratio)) %>%
  pivot_longer(names_to = 'Sample', values_to = 'ISTD_Corrected_Value', cols = c(P12_B_001:QCP_002)) %>%
  mutate(ISTD_Corrected_Value = ifelse(is.na(ISTD_Corrected_Value), 0, ISTD_Corrected_Value))

Solv_Sub_LOD[,'Solv_Sub_Value'] = Solv_Sub_LOD$ISTD_Corrected_Value- Solv_Sub_LOD$Solvent_Blank_Avg
Solv_Sub_LOD[,'Solv_Sub_Ratio'] = Solv_Sub_LOD$Solvent_Blank_Avg/Solv_Sub_LOD$ISTD_Corrected_Value

Solv_Sub_LOD_filter <- Solv_Sub_LOD %>% filter(Solv_Sub_Ratio < 0.35) %>% #filter for features where solvent is < 0.35 peak area of sample#
  subset(select = -c(Solv_Sub_Ratio, ISTD_Corrected_Value)) %>%
  pivot_wider(names_from = Sample, values_from = Solv_Sub_Value)

Solv_Sub_LOD_filter[,'Solv_sub_LOD'] <- apply(Solv_Sub_LOD_filter[,which(colnames(Solv_Sub_LOD_filter) == 'P12_B_001'):which(colnames(Solv_Sub_LOD_filter) == 'P16_D2_001')],1,min,na.rm = TRUE)/5
Solv_Sub_LOD_subset <- Solv_Sub_LOD_filter %>% subset(select = c(Type, Alignment_ID, Average_Mz, Solv_sub_LOD))

#######remove duplicates based on QC_Pool_Sub######
########GC Data - do separately because SMILES will filter out all of the GC data#######
mz1_gc1 <- CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected %>% #filter for EI data
  filter(Type == 'EI')%>%
  filter(Total_score > 0.90) %>%
  subset(select = -c(SMILES, INCHIKEY)) %>%
  group_by(Metabolite_name) %>%
  slice_max(order_by = QC_Pool_Blank_Sub , n =1)


mz1_gc <- read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/mz1_gc.csv")

########LC Data - do separately because SMILES will filter out all of the GC data#######
LC_mz1 <- CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected %>%
  filter(m.z_similarity == 1 & Type != 'EI')
LC_mz1$Alignment_ID <- as.numeric(LC_mz1$Alignment_ID)
LC_mz1$Reference_m.z<- as.numeric(LC_mz1$Reference_m.z)
LC_mz1$Total_score<- as.numeric(LC_mz1$Total_score)
LC_mz1$S.N_average<- as.numeric(LC_mz1$S.N_average)
LC_mz1$Spectrum_reference_file_name<- as.numeric(LC_mz1$Spectrum_reference_file_name)
LC_mz1$HRF_Score<- as.numeric(LC_mz1$HRF_Score)
LC_mz1$SI<- as.numeric(LC_mz1$SI)

######filter by metabolomic name######
# Remove duplicates, keeping the row with the greatest mz silimaritywith the greatest QC pooled column
LC_mz1$`Average_Rt(min)` <- as.numeric(LC_mz1$`Average_Rt(min)`)
mz1_all <- bind_rows(LC_mz1, mz1_gc)

####LC - filter by SMILES######
mz1_all1 <- mz1_all %>%
  filter(!is.na(SMILES) & SMILES != 'null') %>% #these would all get filtered into 1 point but that one point can mess things up later for venn diagram number consistencty
  group_by(SMILES) %>%
  arrange(SMILES, desc(QC_Pool_Blank_Sub)) %>%  # First order by m.z_similarity, then QC_Pooled within each m.z_similarity
  slice_max(order_by = QC_Pool_Blank_Sub, n = 1) %>%  # Keep the row with the largest QC_Pooled value for each metabolite group
  ungroup()  # Ungroup to return to a standard data frame

####LC - filter by INCHIKEY######
mz1_all1 <- mz1_all1 %>%
  filter(!is.na(INCHIKEY) & INCHIKEY != 'null') %>%
  group_by(INCHIKEY) %>%
  arrange(INCHIKEY,desc(QC_Pool_Blank_Sub)) %>%  # First order by m.z_similarity, then QC_Pooled within each m.z_similarity
  slice_max(order_by = QC_Pool_Blank_Sub, n = 1) %>%  # Keep the row with the largest QC_Pooled value for each metabolite group
  ungroup()  # Ungroup to return to a standard data frame

#remove duplicates across lc an gc
mz1_all_gclc1 <- mz1_all1 %>% #get total number of initial features from here with mz similarity = 1 and without duplicates across methods
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2' because it's not in the GC names
  subset(select = -c(Discard)) %>%
  group_by(Metabolite_name) %>%
  filter(!is.na(Metabolite_name) & Metabolite_name != 'Unknown' & Metabolite_name != 'null') %>%
  slice_max(order_by = QC_Pool_Blank_Sub, n = 1) %>%  # Keep the row with the largest QC_Pooled value for each metabolite group
  ungroup()

duplicate_removal_by_sample_avg <- function(df){
  participants <- df %>%
    pivot_longer(cols = starts_with("P1"), names_to = 'Sample', values_to = 'Area') %>% #area is just istd corrected at this point#
    subset(select = -c(Adduct_type, Post_curation_result, Fill_percent, Reference_RT, `Annotation_tag_(VS1.0)`, RT_matched, Comment, Manually_modified_for_quantification, Manually_modified_for_annotation, Isotope_tracking_parent_ID,
                       Isotope_tracking_weight_number, Isotope_tracking_weight_number2, Spectrum_reference_file_name, MS1_isotopic_spectrum, MS.MS_spectrum))

  participants[, 'Participant_Number'] = NA
  participants[, 'Sample_Type'] = NA

  participants <- participants %>%   mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>% #label samples as blank vs sample
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
    mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #label samples with participant nu,ber
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
    mutate(Area = ifelse(is.na(Area), 0, Area))

  #isolate blank samples
  Participant_Blanks <- participants %>% filter(Sample_Type == 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))
  #sanity check - should produce 0
  check_blanks <- nrow(Participant_Blanks) - (nrow(participants)/4) #blanks are a quarter of samples
  print(check_blanks)
  #prepare non-blanks#
  not_blanks <- participants %>%filter(Sample_Type != 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))

  #merge back together
  Participants_Blank_Correct <- merge(Participant_Blanks, not_blanks, by = c('Alignment_ID', 'Average_Mz','Metabolite_name', 'INCHIKEY', 'SMILES', 'Participant_Number', 'Type', 'QC_Pool_Blank_Sub'), all = T) %>%
    dplyr::rename(Blank_Area = Area.x) %>%
    dplyr::rename(Sample_Area = Area.y)

  #correct for blank
  Participants_Blank_Correct[,'Blank_Correction'] = Participants_Blank_Correct$Sample_Area - Participants_Blank_Correct$Blank_Area

  #check for remaining duplicates - msdial sometimes assigns two of the same peak different alignment ids but they'll have different samples despite the same QCP. basiclaly redoing the next bit of analysis #
  duplicate_met_names<- Participants_Blank_Correct %>%
    subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Type, Sample.y, Blank_Correction, QC_Pool_Blank_Sub, Average_Mz)) %>%
    pivot_wider(values_from = Blank_Correction, names_from = Sample.y) %>%
    group_by(Metabolite_name) %>%
    filter(n()>1) %>%
    ungroup()

  lod_correction <- function(df){
    var_name <- colnames(df[,grep("P1", colnames(df))])

    df[var_name][df[var_name]<0] <- NA
    df[var_name][df[var_name]==0] <- NA

    Start <- which(colnames(df)=="Average_Mz") + 1
    End <- ncol(df) #adjust this wuth appropriate last column

    df[, "LOD_Worn_Sub"] <- apply(df[,Start:End], 1, min, na.rm = TRUE)/5

    for (var in var_name){
      df[[var]][is.na(df[[var]])] <- df$LOD_Worn_Sub[is.na(df[[var]])]
    }
    End2 <- which(colnames(df)=="LOD_Worn_Sub")-1
    df[,'Sample_Avg'] = apply(df[,Start:End2], 1, mean, na.rm = TRUE)
    df <- df %>% mutate(Sample_Avg = ifelse(Sample_Avg == Inf, 0, Sample_Avg)) #Inf was all NAs
    df
  }

  duplicate_met_names <- lod_correction(duplicate_met_names)

  #filter out duplicated value#

  dups_filtered <-duplicate_met_names %>% #keep duplicate with highest average in sapmple#
    group_by(Metabolite_name) %>%
    arrange(Metabolite_name, desc(Sample_Avg)) %>%  # First order by m.z_similarity, then QC_Pooled within each m.z_similarity
    slice_max(order_by = Sample_Avg, n = 1) %>%  # Keep the row with the largest QC_Pooled value for each metabolite group
    ungroup()

  #if there are STILL duplicates
  dups_round2 <- dups_filtered %>% group_by(Metabolite_name) %>% filter(n()>1)
  dups_round2

  dups_round2_filtered <- dups_round2 %>%
    group_by(Metabolite_name) %>%
    slice(1)

  not_round2_dups <- dups_filtered %>% group_by(Metabolite_name) %>% filter(n()==1)

  fixed_secondary_dups <- rbind(dups_round2_filtered, not_round2_dups) %>%
    subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY))

  df_dups <- df %>% group_by(Metabolite_name, SMILES, INCHIKEY) %>%
    filter(n()>1)

  df_notdups <- df %>% group_by(Metabolite_name) %>%
    filter(n() == 1)
  df_dups_fixed <- merge(df_dups, fixed_secondary_dups, by = c('Alignment_ID', 'Metabolite_name', 'SMILES', 'INCHIKEY'))

  df <- rbind(df_dups_fixed, df_notdups)
  df
}

mz1_all_gclc <- duplicate_removal_by_sample_avg(mz1_all_gclc1) %>%#inf warnings are ok the code deals with them
  group_by(Metabolite_name) %>%
  slice(1) %>%
  ungroup()
#write.csv(mz1_all_gclc,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/SI_Other_Data_Sets/Removed_Duplicates.csv", row.names=FALSE) #export final data frame#


#######here we split into 2 dfs for further analysis: one will be for comparing blanks and samples which will contain solvent subtracted values. the other will be for focusing on particiapnt worn samples, which will have unworn blank subtracted values#####
#########participant worn samples#######
participants <- mz1_all_gclc %>%
  pivot_longer(cols = starts_with("P1"), names_to = 'Sample', values_to = 'Area') %>% #area is just istd corrected at this point#
  subset(select = -c(Adduct_type, Post_curation_result, Fill_percent, Reference_RT, `Annotation_tag_(VS1.0)`, RT_matched, Comment, Manually_modified_for_quantification, Manually_modified_for_annotation, Isotope_tracking_parent_ID,
                     Isotope_tracking_weight_number, Isotope_tracking_weight_number2, Spectrum_reference_file_name, MS1_isotopic_spectrum, MS.MS_spectrum))

participants[, 'Participant_Number'] = NA
participants[, 'Sample_Type'] = NA

participants <- participants %>%   mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>% #label samples as blank vs sample
  mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
  mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #label samples with participant nu,ber
  mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
  mutate(Area = ifelse(is.na(Area), 0, Area))

#isolate blank samples
Participant_Blanks <- participants %>% filter(Sample_Type == 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))
#sanity check - should produce 0
check_blanks <- nrow(Participant_Blanks) - (nrow(participants)/4) #blanks are a quarter of samples

#prepare non-blanks#
not_blanks <- participants %>%filter(Sample_Type != 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))
colnames(participants)

#merge back together
Participants_Blank_Correct1 <- merge(Participant_Blanks, not_blanks, by = c('Alignment_ID', 'Average_Mz','Metabolite_name', 'INCHIKEY', 'SMILES', 'Participant_Number', 'Type', 'QC_Pool_Blank_Sub')) %>%
  dplyr::rename(Blank_Area = Area.x) %>%
  dplyr::rename(Sample_Area = Area.y)

#sanity check on merge - should produce 0 value
nrow(not_blanks) - nrow(Participants_Blank_Correct1)

#correct for blank
Participants_Blank_Correct1[,'Blank_Correction'] = Participants_Blank_Correct1$Sample_Area - Participants_Blank_Correct1$Blank_Area
Participants_Blank_Correct1[,'Blank_Ratio'] = Participants_Blank_Correct1$Blank_Area/Participants_Blank_Correct1$Sample_Area #NAs: not detected in blank or sample#

#filter out high blanks
Participants_Blank_Correct <- Participants_Blank_Correct1 %>%
  filter(Blank_Ratio < 0.35 )

#pivot wider to get lods#
#AG Note: the filtered out samples will still be in the df but as NAs#
Participants_Blank_Correct_Wide <- Participants_Blank_Correct %>%
  subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Type, Sample.y, Blank_Correction, QC_Pool_Blank_Sub, Average_Mz)) %>%
  pivot_wider(values_from = Blank_Correction, names_from = Sample.y)

#sanity check - should give 0 row tibble#
pivot_check <- Participants_Blank_Correct_Wide %>% group_by(Metabolite_name) %>% filter(n()>1)

#get LOD above blanks#

var_name <- colnames(Participants_Blank_Correct_Wide[,grep("P1", colnames(Participants_Blank_Correct_Wide))])

Participants_Blank_Correct_Wide[var_name][Participants_Blank_Correct_Wide[var_name]<0] <- NA
Participants_Blank_Correct_Wide[var_name][Participants_Blank_Correct_Wide[var_name]==0] <- NA

#@AG: edit this line so it's not manual - the columns used keeps changing so double check#
Start <- which(colnames(Participants_Blank_Correct_Wide)=="P12_D2_001")
End <- which(colnames(Participants_Blank_Correct_Wide)=="P16_D3_001") #adjust this wuth appropriate last column

Participants_Blank_Correct_Wide[, "LOD_Worn_Sub"] <- apply(Participants_Blank_Correct_Wide[,Start:End], 1, min, na.rm = TRUE)/5

for (var in var_name){
  Participants_Blank_Correct_Wide[[var]][is.na(Participants_Blank_Correct_Wide[[var]])] <- Participants_Blank_Correct_Wide$LOD_Worn_Sub[is.na(Participants_Blank_Correct_Wide[[var]])]
}

#get percent detection#
Participants_Blank_Correct_Wide[,'Detection_Count'] = NA #empty column for detection count#

Participants_Blank_Correct_Wide <- Participants_Blank_Correct_Wide %>% mutate(Detection_Count = rowSums(.[grepl('_D', colnames(.))] != LOD_Worn_Sub)) #change row for detection count to the number of columns that are worn samples not equal the LOD

number_worn_samples = sum(grepl(paste0("_D"),names(Participants_Blank_Correct_Wide))) #count number of samples#

Participants_Blank_Correct_Wide[,'Percent_Detection'] = Participants_Blank_Correct_Wide$Detection_Count*100/number_worn_samples #find percent detection

#pivot back longer for analysis#
Participants_Cleaned1 <- Participants_Blank_Correct_Wide %>%
  pivot_longer(cols = starts_with('P1'), values_to = 'Blank_Corrected_Anlayte_Area', names_to = 'Sample') %>%
  mutate(Blank_Corrected_Anlayte_Area = ifelse(Blank_Corrected_Anlayte_Area == 0, LOD_Worn_Sub, Blank_Corrected_Anlayte_Area)) %>%
  mutate(Blank_Corrected_Anlayte_Area = ifelse(Blank_Corrected_Anlayte_Area < 0, LOD_Worn_Sub, Blank_Corrected_Anlayte_Area)) %>%
  mutate(Blank_Corrected_Anlayte_Area = ifelse(is.na(Blank_Corrected_Anlayte_Area), LOD_Worn_Sub, Blank_Corrected_Anlayte_Area))

Participants_Cleaned1[,'Participant_Number'] = NA
Participants_Cleaned1[,'Sample_Type'] = NA

Participants_Cleaned2 <- Participants_Cleaned1 %>%   mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #assign participant numbers
  mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
  mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>%
  mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type))

#merge with creatinine#
creatinine_for_norm <- Manual_Int_Long %>% filter(Analyte == 'Creatinine')
Participants_Cleaned <- merge(creatinine_for_norm, Participants_Cleaned2, by = c('Participant_Number', 'Sample'), all = T) %>%
  subset(select = -c(Detection_Count.y, Detection_Count.x, LOD, Percent_Detection.x)) %>%
  dplyr::rename(Creatinine_Area = Corrected_Area) %>%
  dplyr::rename(Percent_Detection = Percent_Detection.y)

#sanity check - hsould be 0
nrow(Participants_Cleaned)-nrow(Participants_Cleaned2)

#correct to creatinine
Participants_Cleaned[,'Creatinine_corrected_value'] = Participants_Cleaned$Blank_Corrected_Anlayte_Area/Participants_Cleaned$Creatinine_Area
Participants_Cleaned[,'lognormalized_area_to_creatine'] = log10(Participants_Cleaned$Blank_Corrected_Anlayte_Area/Participants_Cleaned$Creatinine_Area)
Participants_Cleaned[,'log_not_norm_area'] = log10(Participants_Cleaned$Blank_Corrected_Anlayte_Area)

log10_density <- Participants_Cleaned %>%
  ggplot(aes(x = log10(Creatinine_corrected_value)))+
  geom_density()+
  theme_classic()
log2_density <- Participants_Cleaned %>%
  ggplot(aes(x = log2(Creatinine_corrected_value)))+
  geom_density()+
  theme_classic()

min_abund <- min(Participants_Cleaned$log_not_norm_area)
max_abund <- max(Participants_Cleaned$log_not_norm_area)
max_abund-min_abund

heatmap_features <- Participants_Cleaned %>%
  subset(select = c('Sample', 'Metabolite_name', 'log_not_norm_area', 'Percent_Detection', 'INCHIKEY', 'SMILES', 'Type', 'Average_Mz', 'QC_Pool_Blank_Sub', 'LOD_Worn_Sub')) %>% #subset relevant columns. Sample_Area is the blank subtracted sample log transformed#
  pivot_wider(names_from = Sample, values_from = log_not_norm_area) %>%
  filter(Percent_Detection > 50 | Percent_Detection == 50)
#get LOD from creatinine normalization

#write.csv(heatmap_features,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/heatmap_features.csv") ##export final data frame#
heatmap_data <- Participants_Cleaned %>% #convert to matrix
  filter(Percent_Detection > 50 | Percent_Detection == 50) %>%
  #filter(lognormalized_area_to_creatine == Inf) %>%#
  mutate(Sample = ifelse(Sample == 'P12_D2_001', '1, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D1_001', '1, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D3_001', '1, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D1_001', '2, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D2_001', '2, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D3_001', '2, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D1_001', '3, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D2_001', '3, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D3_001', '3, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D1_001', '4, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D2_001', '4, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D3_001', '4, Day 3', Sample)) %>%
  subset(select = c('Sample', 'Metabolite_name', 'lognormalized_area_to_creatine')) %>% #subset relevant columns. Sample_Area is the blank subtracted sample log transformed#
  pivot_wider(names_from = Sample, values_from = lognormalized_area_to_creatine)

#write.csv(heatmap_data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Heatmap_data.csv", row.names=FALSE) #export final data frame#

#generate heatmap
Participants_heatmap_matrix_creatine_norm_blank_sub <- Participants_Cleaned %>% #convert to matrix
  filter(Percent_Detection > 50 | Percent_Detection == 50) %>%
  #filter(lognormalized_area_to_creatine == Inf) %>%#
  mutate(Sample = ifelse(Sample == 'P12_D2_001', '1, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D1_001', '1, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D3_001', '1, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D1_001', '2, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D2_001', '2, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D3_001', '2, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D1_001', '3, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D2_001', '3, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D3_001', '3, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D1_001', '4, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D2_001', '4, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D3_001', '4, Day 3', Sample)) %>%
  subset(select = c('Sample', 'Metabolite_name', 'lognormalized_area_to_creatine')) %>% #subset relevant columns. Sample_Area is the blank subtracted sample log transformed#
  pivot_wider(names_from = Sample, values_from = lognormalized_area_to_creatine) %>%
  select(-1) %>%
  data.matrix()
Participants_heatmap_matrix_creatine_norm_blank_sub
colnames(Participants_Cleaned)

Participants_heatmap_matrix_creatine_norm <- pheatmap(Participants_heatmap_matrix_creatine_norm_blank_sub, show_rownames = FALSE, scale = 'row', fontsize_col = 17, color = hcl.colors(50, "BluYl"))
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Participants_heatmap_creatine_norm.svg", plot = Participants_heatmap_matrix_creatine_norm, base_width=10.95, base_height=10)

#heatmaps for firt 100 features with levels above blanks

Participants_heatmap_matrix_100_nonorm <- Participants_Cleaned %>% #convert to matrix
  filter(Percent_Detection > 50 | Percent_Detection == 50) %>%
  arrange(desc(QC_Pool_Blank_Sub)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D2_001', '1, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D1_001', '1, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D3_001', '1, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D1_001', '2, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D2_001', '2, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D3_001', '2, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D1_001', '3, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D2_001', '3, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D3_001', '3, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D1_001', '4, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D2_001', '4, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D3_001', '4, Day 3', Sample)) %>%
  subset(select = c('Sample', 'Metabolite_name', 'log_not_norm_area', 'QC_Pool_Blank_Sub')) %>% #subset relevant columns. Sample_Area is the blank subtracted sample log transformed#
  pivot_wider(names_from = Sample, values_from = log_not_norm_area) %>%
  slice_max(order_by = QC_Pool_Blank_Sub, n = 100) %>%
  subset(select = -c(QC_Pool_Blank_Sub)) %>%
  select(-1) %>%
  data.matrix()

Participants_heatmap_matrix_100_normdf1 <- Participants_Cleaned %>%  filter(Percent_Detection > 50 | Percent_Detection == 50) %>%
  subset(select = c('Sample', 'Metabolite_name', 'lognormalized_area_to_creatine', 'QC_Pool_Blank_Sub')) %>% #subset relevant columns. Sample_Area is the blank subtracted sample log transformed#
  pivot_wider(names_from = Sample, values_from = lognormalized_area_to_creatine) %>%
  slice_max(order_by = QC_Pool_Blank_Sub, n = 100) %>%
  subset(select = -c(QC_Pool_Blank_Sub))  %>%
  as.data.frame()
rownames(Participants_heatmap_matrix_100_normdf1) <- Participants_heatmap_matrix_100_normdf1$Metabolite_name
Participants_heatmap_matrix_100_normdf <-  t(scale(t(Participants_heatmap_matrix_100_normdf1[, -1])))

Participants_heatmap_matrix_100_normdf3 <-Participants_heatmap_matrix_100_normdf %>% as.data.frame()

Participants_heatmap_matrix_100_normdf2 <- cbind(Metabolite_name = rownames(Participants_heatmap_matrix_100_normdf3), Participants_heatmap_matrix_100_normdf3)
rownames(Participants_heatmap_matrix_100_normdf2) <- NULL  # Remove the row names

Participants_heatmap_matrix_100_normdf <- Participants_heatmap_matrix_100_normdf2 %>%
  pivot_longer(names_to = 'Sample', values_to = 'lognormalized_area_to_creatine', cols = starts_with('P1')) %>%
  mutate(Sample = ifelse(Sample == 'P12_D2_001', '1, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D1_001', '1, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P12_D3_001', '1, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D1_001', '2, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D2_001', '2, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P14_D3_001', '2, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D1_001', '3, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D2_001', '3, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P15_D3_001', '3, Day 3', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D1_001', '4, Day 1', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D2_001', '4, Day 2', Sample)) %>%
  mutate(Sample = ifelse(Sample == 'P16_D3_001', '4, Day 3', Sample))


palette <- colorRampPalette(c("#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"))

Participants_heatmap_nonorm100 <- pheatmap(Participants_heatmap_matrix_100_nonorm,
                                         show_rownames = FALSE, fontsize_col = 30,
                                         fontsize = 28, scale = 'row', border_color = NA)
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/heatmap_Feb27.svg", plot = Participants_heatmap_norm100, base_width=25, base_height=14)
save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/heatmap_Apr28_nonorm.svg", plot = Participants_heatmap_nonorm100, base_width=11, base_height=10)

#interactive_heatmap <- plot_ly(z = Participants_heatmap_matrix_100_normdf$lognormalized_area_to_creatine,
   #                            x = Participants_heatmap_matrix_100_normdf$Sample,
    #                             y = Participants_heatmap_matrix_100_normdf$Metabolite_name,
    #                           colors = palette(50), type = "heatmap") %>%
 # layout(yaxis = list(title = 'Metabolite', showticklabels = FALSE, ticks = FALSE), xaxis = list(title = 'Sample'))

######solvent sub analysis######
######solvent sub analysis######
solvent_subs_lod_correct1 <- mz1_all_gclc %>%
  pivot_longer(cols = starts_with('P1'), values_to = 'Area', names_to = 'Sample') %>%
  mutate(Area = ifelse(is.na(Area), 0, Area))

solvent_subs_lod_correct1[,'Solvent_Sub_Area'] = solvent_subs_lod_correct1$Area - solvent_subs_lod_correct1$Solvent_Blank_Avg
solvent_subs_lod_correct1[,'Solvent_Area_Ratio'] = solvent_subs_lod_correct1$Solvent_Blank_Avg/solvent_subs_lod_correct1$Area

solvent_subs_lod_correct <- solvent_subs_lod_correct1 %>%
  filter(Solvent_Area_Ratio < 0.35) %>%
  subset(select = c(Sample, INCHIKEY, SMILES, Solvent_Sub_Area, Type, Metabolite_name, Average_Mz)) %>%
  pivot_wider(names_from = 'Sample', values_from = 'Solvent_Sub_Area')

var_name_P <- colnames(solvent_subs_lod_correct[,grep("P1", colnames(solvent_subs_lod_correct))]) #create variable names to use later for any column with these strings in the name

solvent_subs_lod_correct[var_name_P][solvent_subs_lod_correct[var_name_P]==0] <- NA #set any avalues in the previously defined columns that are equal 0 as NA#
solvent_subs_lod_correct[var_name_P][solvent_subs_lod_correct[var_name_P]<0] <- NA #set any avalues in the previously defined columns that are equal 0 as NA#
solvent_subs_lod_correct[,'Spk_Sub_LOD'] <- apply(solvent_subs_lod_correct[,which(colnames(solvent_subs_lod_correct) == 'P14_B_001'):which(colnames(solvent_subs_lod_correct) == 'P12_D3_001')],1,min,na.rm = TRUE)/5

for (var in var_name_P){
  solvent_subs_lod_correct[[var]][is.na(solvent_subs_lod_correct[[var]])] <- solvent_subs_lod_correct$Spk_Sub_LOD[is.na(solvent_subs_lod_correct[[var]])] #change to lod
}

#get percent detection in participant samples#
solvent_subs_lod_correct[,'Detection_Count'] = NA #empty column for detection count#

solvent_subs_lod_correct <- solvent_subs_lod_correct %>% mutate(Detection_Count = rowSums(.[grepl('P1', colnames(.))] != Spk_Sub_LOD)) #change row for detection count to the number of columns that are worn samples not equal the LOD
number_worn_samples2 = sum(grepl(paste0("P1"),names(solvent_subs_lod_correct))) #count number of samples#

solvent_subs_lod_correct[,'Percent_Detection'] = solvent_subs_lod_correct$Detection_Count*100/number_worn_samples2 #find percent detection

#####volcano#############
participant_solvent_sub <- solvent_subs_lod_correct %>%
  pivot_longer(col = starts_with('P1'), names_to = 'Sample', values_to = 'Solvent_Sub_Area')
participant_solvent_sub[,'Participant_Number'] = NA
participant_solvent_sub[,'Sample_Type'] = NA
participant_solvent_sub <- participant_solvent_sub %>%   mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>% #label samples as blank vs sample
  mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
  mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #label samples with participant nu,ber
  mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
  mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number))%>%
  ungroup()

#log transform#
participant_solvent_sub[,'log_transformed_area'] = log10(participant_solvent_sub$Solvent_Sub_Area)

#get number only in blanks or only in samples
only_samples <- participant_solvent_sub %>% ungroup() %>%
  group_by(Metabolite_name, SMILES, INCHIKEY, Sample_Type, Spk_Sub_LOD) %>%
  dplyr::summarise(Mean = mean(Solvent_Sub_Area)) %>%
  ungroup()%>%
  filter(Sample_Type == 'Blank' & Mean == Spk_Sub_LOD)%>%
  subset(select = -c(Mean, Spk_Sub_LOD, Sample_Type))

only_blanks<- participant_solvent_sub %>% ungroup() %>%
  group_by(Metabolite_name, INCHIKEY, Sample_Type, Spk_Sub_LOD) %>%
  dplyr::summarise(Mean = mean(Solvent_Sub_Area)) %>%
  ungroup()%>%
  filter(Sample_Type == 'Sample' & Mean == Spk_Sub_LOD) %>%
  nrow()

#view(merged_with_dups_per_method_long_no_dups_mz_1)#
#get p values#
t_tests_volc1 <- participant_solvent_sub %>% #t tests
  ungroup() %>%
  filter(Percent_Detection > 50 | Percent_Detection == 50) %>%
  filter(Sample_Type == 'Blank' | Sample_Type == 'Sample') %>%
  group_by(Metabolite_name, SMILES, Average_Mz) %>%
  filter(sd(log_transformed_area) > 0) %>% #filter out with no variation between blanks and samples#
  group_by(Sample_Type, Metabolite_name, Type, Average_Mz) %>%
  nest() %>%
  spread(key = Sample_Type, value = data) %>%
  mutate(t_test = map2(Sample, Blank, ~{t.test(.x$log_transformed_area, .y$log_transformed_area) %>%
      tidy()}), Sample = map(Sample, nrow),Blank = map(Blank, nrow)) %>%
  unnest() %>%
  as.data.frame()

participant_solvent_sub[,'mean'] = NA

participant_solvent_sub_stats <- participant_solvent_sub %>% #get the average of smaples and blanks for each metabolite for fold change#
  #can't jsut use estimate value because the log messes things up
  group_by(Sample_Type, Metabolite_name, SMILES) %>%
  dplyr::summarise(mean = mean(Solvent_Sub_Area)) %>%#note:must add dplyr::#
  ungroup()

participant_solvent_sub_stats_blank <- participant_solvent_sub_stats %>% filter(Sample_Type == 'Blank') %>%
  subset(select = c(Metabolite_name, SMILES, mean)) %>%
  dplyr::rename(Blank_mean = mean)

participant_solvent_sub_stats_worn <- participant_solvent_sub_stats %>% filter(Sample_Type == 'Sample') %>%
  subset(select = c(Metabolite_name, SMILES, mean)) %>%
  dplyr::rename(Sample_mean = mean)

participant_fold_diff_means <- merge(participant_solvent_sub_stats_worn, participant_solvent_sub_stats_blank, by = c('Metabolite_name', 'SMILES'))
((nrow(participant_solvent_sub_stats))/2)-nrow(participant_fold_diff_means) # sanity check - expect a 0#

participant_fold_diff_means[,'Fold_Difference'] = participant_fold_diff_means$Sample_mean/participant_fold_diff_means$Blank_mean

#merge fold diff values back with t test results#
t_tests_volc <- merge(t_tests_volc1, participant_fold_diff_means, by = c('Metabolite_name'))

nrow(t_tests_volc1) - nrow(t_tests_volc) #sanity check - expect 0 value

t_tests_volc[,'logP'] = -log2(t_tests_volc$p.value)
t_tests_volc[,'log_FC'] = log2(t_tests_volc$Fold_Difference)
volcano_p_vlaues_filitered <- t_tests_volc %>% filter(p.value < 0.05) %>% filter(Fold_Difference > 1) #filter for p < 0.05 and higher in worns
volcano_p_vlaues_filitered2 <- t_tests_volc %>% filter(p.value < 0.001) %>% filter(Fold_Difference > 1) #filter for p < 0.05 and higher in worns
nrow(volcano_p_vlaues_filitered)
min(volcano_p_vlaues_filitered$p.value)
max(volcano_p_vlaues_filitered$p.value)

volcano_p_vlaues_filitered_subset <- volcano_p_vlaues_filitered %>% subset(select = c(Type, Metabolite_name, alternative, logP, Fold_Difference, log_FC))
volcano_p_vlaues_filitered_subset2 <- volcano_p_vlaues_filitered %>% subset(select = c(Type, Metabolite_name, SMILES, alternative, logP, Fold_Difference, log_FC))

volcano_p_vlaues_filitered_subset_all <- volcano_p_vlaues_filitered %>% subset(select = c(Type, Metabolite_name, alternative, logP))

merge_mass_load <- merge(participant_solvent_sub, volcano_p_vlaues_filitered_subset, by = c('Type', 'Metabolite_name'), all = T) %>%
  filter(!is.na(alternative))

t_test_subset <- t_tests_volc %>% subset(select = c(Type, Metabolite_name, alternative, logP, Fold_Difference, log_FC))

nrow(volcano_p_vlaues_filitered) #features significantly differing#
volcano_p_vlaues_filitered2 <- t_tests_volc %>% filter(p.value < 0.001)
nrow(volcano_p_vlaues_filitered2)

#view(t_tests_volc)#
p_cutoff <- 0.05
log_p_cuotff = -log2(p_cutoff)
p_cutoff2 <- 0.01
log_p_cuotff2 = -log10(p_cutoff2)
p_cutoff3 <- 0.001
log_p_cuotff3= -log10(p_cutoff3)

t_tests_volc$Type <- as.character(t_tests_volc$Type)

#fold change
#merge first with a file that has mass
# Define the shapes explicitly, excluding the default plus symbol
t_tests_volc[,'Fold_Category'] = NA
t_tests_volc <- t_tests_volc %>%
  mutate(Fold_Category = ifelse(log_FC > 0, 'Higher in Worn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC < 0, 'Higher in Unworn', Fold_Category))

t_tests_volc[,'p_Category'] = 'Not significantly different'
t_tests_volc <- t_tests_volc %>%
  mutate(p_Category = case_when((logP > log_p_cuotff & log_FC > 0) ~ 'Higher in Worn',
         (logP > log_p_cuotff & log_FC < 0) ~ 'Higher in Unworn', TRUE ~ 'Not significantly different'))

volc_stats <- t_tests_volc %>%
  group_by(Fold_Category) %>%
  get_summary_stats(log_FC, type = 'common') %>%
  subset(select = c(Fold_Category, n)) %>%
  pivot_wider(names_from = Fold_Category, values_from = n)
percent_higher_in_worn = volc_stats$`Higher in Worn`*100/(volc_stats$`Higher in Unworn`+volc_stats$`Higher in Worn`)
max(t_tests_volc$log_FC)

volc_n <- t_tests_volc %>% mutate(Type = ifelse(Type == 'C18_Negative', 'C18 Negative', Type)) %>%
  mutate(Type = ifelse(Type == 'C18_Positive', 'C18 Positive', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Negative', 'HILIC Negative', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Positive', 'HILIC Positive', Type)) %>%
  mutate(Type = factor(Type, levels=c("C18 Negative", "C18 Positive", "HILIC Negative", "HILIC Positive", "EI"))) %>%
  nrow()

volc_data <- t_tests_volc %>% dplyr::rename(log.p.value = logP)
#write.csv(volc_data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Volcano_Data.csv", row.names=FALSE) #export final data frame#
min(volc_data$p.value)
max(volc_data$p.value)

volc <- t_tests_volc %>%
  mutate(Type = ifelse(Type == 'C18_Negative', 'C18 Negative', Type)) %>%
  mutate(Type = ifelse(Type == 'C18_Positive', 'C18 Positive', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Negative', 'HILIC Negative', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Positive', 'HILIC Positive', Type)) %>%
  mutate(Type = factor(Type, levels=c("C18 Negative", "C18 Positive", "HILIC Negative", "HILIC Positive", "EI"))) %>%
  ggplot(aes(x = log_FC, y = logP, colour = p_Category)) +
  geom_point(size = 4, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(limits = c(-12,14))+
  geom_hline(yintercept = log_p_cuotff, linetype = 'dashed', colour = 'black', size = 1) +
#  geom_hline(yintercept = log_p_cuotff2, linetype = 'dotted', colour = 'black', size = 1) +
#  geom_hline(yintercept = log_p_cuotff3, linetype = 'solid', colour = 'black', size = 1) +
  labs(x = expression(atop('Log'[2]*' Fold Difference (Peak Area)')), y = expression('-Log'[2]* ' p value'), title = paste0('n =', volc_n)) +
  scale_colour_manual(values = c('#FBB4AE', '#B3CDE3', 'grey')) +
  #scale_colour_manual(values = c('#377EB8', '#4DAF4A'))+
  theme(legend.position = 'none')+
  #scale_shape_manual(values = c(16, 17, 13, 18, 19)) +  # Use solid circle, triangle, square, and diamond
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    legend.text = element_text(size = 25),
    title = element_text(size = 25)) +# Adjust font size
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))
volc

#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/Volcano_Feb27.svg", plot = volc, base_width=12.82, base_height=8.34)

FC_Filtered_n <- t_tests_volc %>%
  nrow()
FC_filtered_Data <- t_tests_volc %>%
  mutate(Fold_Category = ifelse(log_FC > 0, 'Higher in Worn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC < 0, 'Higher in Unworn', Fold_Category)) %>%
  group_by(Fold_Category) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Fold_Category, values_from = n)

higher_in_worn_volc <- FC_filtered_Data$`Higher in Worn`/(FC_filtered_Data$`Higher in Unworn`+FC_filtered_Data$`Higher in Worn`)
max_fc_volc <- 2^max(t_tests_volc$log_FC)

#write.csv(FC_filtered_Data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Fold_Change_Filtered.csv", row.names=FALSE) #export final data frame#

plot_FC_all_filtered <- t_tests_volc %>%
  mutate(Fold_Category = ifelse(log_FC > 0, 'Higher in Worn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC < 0, 'Higher in Unworn', Fold_Category)) %>%
  ggplot(aes(x = Average_Mz, y = log_FC, colour = Fold_Category))+
  geom_point(size = 4, alpha = 0.6) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 'solid')+
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    legend.text = element_text(size = 25),
    title = element_text(size = 25))+
  labs(y = expression('Log'[2]*' Fold Difference (Peak Area Ratio)'), x = 'Average m/z', title =
         paste0('n = ',FC_Filtered_n)) +
  scale_colour_manual(values = c('#FBB4AE', '#B3CDE3', 'grey')) +
  theme(legend.position = 'bottom')+
  #scale_shape_manual(values = c(16, 17, 13, 18, 19)) +  # Use solid circle, triangle, square, and diamond
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    legend.text = element_text(size = 25),
    title = element_text(size = 25)) +# Adjust font size
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(-12,14))+
  # guides(colour = 'none')+
  #theme(legend.position = 'right')+
  #theme(legend.justification = "top")
  theme(legend.position = c(0.8, 0.97), legend.box.background = element_rect(colour = "black")) #move legend in plot
plot_FC_all_filtered
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/Fold_Plot_Filtered_Feb27.svg", plot = plot_FC_all_filtered, base_width=12, base_height=8.34)


######try running all GC data through pubchem - for unfiltered #######
GC_all_unique_names <- CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected %>%
  filter(Type == 'EI') %>%
  group_by(Metabolite_name) %>%
  slice(1)

GC_all <- GC_all_unique_names$Metabolite_name

gc_inchikey_list_all<- list()

# Loop through each metabolite name

pb <- progress_bar$new(total = length(GC_all)) #progress bar for loop processing

for (i in seq_along(GC_all)) {
  pb$tick()
  tryCatch({ #can do without the tryCatch but it's just better at handling errors#
    props <- get_properties(
      properties = c("InChI", 'SMILES'),
      identifier = c(GC_all[i]),
      namespace = "name")
    if (!is.null(props)) {
      gc_inchikey <- retrieve(props, .combine.all = TRUE, .to.data.frame = TRUE)
      gc_inchikey_list[[i]] <- gc_inchikey
    } else {
      message(paste("No result found for:", GC_all[i]))
    }
  }, error = function(e) {
    message(paste("Error processing:", GC_all[i], "Error:", e$message))
  })
}
GC_IDs_Combined_all  <- bind_rows(gc_inchikey_list) %>%
  mutate(CanonicalSMILES = ifelse(CanonicalSMILES == '[NA]', NA, CanonicalSMILES)) %>%
  mutate(IsomericSMILES = ifelse(IsomericSMILES == '[NA]', NA, IsomericSMILES)) %>%
  mutate(InChI = ifelse(InChI == '[NA]', NA, InChI)) %>%
  mutate(InChIKey = ifelse(InChIKey == '[NA]', NA, InChIKey))%>%
  dplyr::rename(SMILES = CanonicalSMILES) %>% #MS Dial uses canonical smiles
  dplyr::rename(INCHIKEY = InChIKey) %>%
  group_by(INCHIKEY, CID)%>%
  slice(1)%>% #remove duplicates generated
  ungroup() %>%
  dplyr::rename(Metabolite_name =Identifier)

#write.csv(GC_IDs_Combined_all,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/GC_IDs_Combined_all.csv", row.names=FALSE) #export final data frame#
GC_IDs_Combined_all <- read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/GC_IDs_Combined_all.csv", col_types = cols(.default = "c"))

GC_all_unique_names_subset <- CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected %>%
  filter(Type == 'EI') %>%
  subset(select = -c(INCHIKEY, SMILES))

GC_all_merged <- merge(GC_IDs_Combined_all,GC_all_unique_names_subset, by = c('Metabolite_name'), all = T) %>%
  subset(select = -c(CID, IsomericSMILES, InChI)) %>%
  filter(!is.na(Average_Mz))
GC_all_missed <- GC_all_merged %>% filter(is.na(INCHIKEY)) %>%
  group_by(Metabolite_name) %>%
  slice(1)

LC_all <- CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected %>%
  filter(Type !='EI')

all_with_dups_and_inchikey1 <- rbind(LC_all,GC_all_merged) %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2' because it's not in the GC names
  subset(select = -c(Discard))

No_Inchikey <- all_with_dups_and_inchikey1 %>% filter(is.na(INCHIKEY))

With_Incihkey <- all_with_dups_and_inchikey1 %>% filter(!is.na(INCHIKEY)) %>%
  group_by(INCHIKEY) %>%
  slice_max(order_by = QC_Pool_Blank_Sub, n = 1) %>%
  ungroup() %>%
  group_by(SMILES) %>%
  filter(!is.na(SMILES) & SMILES != 'null') %>%
  slice_max(order_by = QC_Pool_Blank_Sub, n = 1) %>%
  ungroup()

All_no_dups_initial <- rbind(No_Inchikey, With_Incihkey) %>%
  filter(!is.na(Metabolite_name) & Metabolite_name != 'null' & Metabolite_name != 'Unknown') %>%
  group_by(Metabolite_name) %>%
  slice_max(order_by = QC_Pool_Blank_Sub, n = 1) %>%
  ungroup()

duplicate_removal_by_sample_avg <- function(df){
  participants <- df %>%
    pivot_longer(cols = starts_with("P1"), names_to = 'Sample', values_to = 'Area') %>% #area is just istd corrected at this point#
    subset(select = -c(Adduct_type, Post_curation_result, Fill_percent, Reference_RT, `Annotation_tag_(VS1.0)`, RT_matched, Comment, Manually_modified_for_quantification, Manually_modified_for_annotation, Isotope_tracking_parent_ID,
                       Isotope_tracking_weight_number, Isotope_tracking_weight_number2, Spectrum_reference_file_name, MS1_isotopic_spectrum, MS.MS_spectrum))

  participants[, 'Participant_Number'] = NA
  participants[, 'Sample_Type'] = NA

  participants <- participants %>%   mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>% #label samples as blank vs sample
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
    mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #label samples with participant nu,ber
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number)) %>%
    mutate(Area = ifelse(is.na(Area), 0, Area))

  #isolate blank samples
  Participant_Blanks <- participants %>% filter(Sample_Type == 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))
  #sanity check - should produce 0
  check_blanks <- nrow(Participant_Blanks) - (nrow(participants)/4) #blanks are a quarter of samples
  print(check_blanks)
  #prepare non-blanks#
  not_blanks <- participants %>%filter(Sample_Type != 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))

  #merge back together
  Participants_Blank_Correct <- merge(Participant_Blanks, not_blanks, by = c('Alignment_ID', 'Average_Mz','Metabolite_name', 'INCHIKEY', 'SMILES', 'Participant_Number', 'Type', 'QC_Pool_Blank_Sub'), all = T) %>%
    dplyr::rename(Blank_Area = Area.x) %>%
    dplyr::rename(Sample_Area = Area.y)

  #correct for blank
  Participants_Blank_Correct[,'Blank_Correction'] = Participants_Blank_Correct$Sample_Area - Participants_Blank_Correct$Blank_Area

  #check for remaining duplicates - msdial sometimes assigns two of the same peak different alignment ids but they'll have different samples despite the same QCP. basiclaly redoing the next bit of analysis #
  duplicate_met_names<- Participants_Blank_Correct %>%
    subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Type, Sample.y, Blank_Correction, QC_Pool_Blank_Sub, Average_Mz)) %>%
    pivot_wider(values_from = Blank_Correction, names_from = Sample.y) %>%
    group_by(Metabolite_name) %>%
    filter(n()>1) %>%
    ungroup()

  lod_correction <- function(df){
    var_name <- colnames(df[,grep("P1", colnames(df))])

    df[var_name][df[var_name]<0] <- NA
    df[var_name][df[var_name]==0] <- NA

    Start <- which(colnames(df)=="Average_Mz") + 1
    End <- ncol(df) #adjust this wuth appropriate last column

    df[, "LOD_Worn_Sub"] <- apply(df[,Start:End], 1, min, na.rm = TRUE)/5

    for (var in var_name){
      df[[var]][is.na(df[[var]])] <- df$LOD_Worn_Sub[is.na(df[[var]])]
    }
    End2 <- which(colnames(df)=="LOD_Worn_Sub")-1
    df[,'Sample_Avg'] = apply(df[,Start:End2], 1, mean, na.rm = TRUE)
    df <- df %>% mutate(Sample_Avg = ifelse(Sample_Avg == Inf, 0, Sample_Avg)) #Inf was all NAs
    df
  }

  duplicate_met_names <- lod_correction(duplicate_met_names)

  #filter out duplicated value#

  dups_filtered <-duplicate_met_names %>% #keep duplicate with highest average in sapmple#
    group_by(Metabolite_name) %>%
    arrange(Metabolite_name, desc(Sample_Avg)) %>%  # First order by m.z_similarity, then QC_Pooled within each m.z_similarity
    slice_max(order_by = Sample_Avg, n = 1) %>%  # Keep the row with the largest QC_Pooled value for each metabolite group
    ungroup()

  #if there are STILL duplicates
  dups_round2 <- dups_filtered %>% group_by(Metabolite_name) %>% filter(n()>1)
  dups_round2

  dups_round2_filtered <- dups_round2 %>%
    group_by(Metabolite_name) %>%
    slice(1)

  not_round2_dups <- dups_filtered %>% group_by(Metabolite_name) %>% filter(n()==1)

  fixed_secondary_dups <- rbind(dups_round2_filtered, not_round2_dups) %>%
    subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY))

  df_dups <- df %>% group_by(Metabolite_name, SMILES, INCHIKEY) %>%
    filter(n()>1)

  df_notdups <- df %>% group_by(Metabolite_name) %>%
    filter(n() == 1)
  df_dups_fixed <- merge(df_dups, fixed_secondary_dups, by = c('Alignment_ID', 'Metabolite_name', 'SMILES', 'INCHIKEY'))

  df <- rbind(df_dups_fixed, df_notdups)
  df
}

All_no_dups <- duplicate_removal_by_sample_avg(All_no_dups_initial)

solv_sub_analysis <- function(df){
  solvent_subs_lod_correct1 <- df %>%
    pivot_longer(cols = starts_with('P1'), values_to = 'Area', names_to = 'Sample') %>%
    mutate(Area = ifelse(is.na(Area), 0, Area))

  solvent_subs_lod_correct1[,'Solvent_Sub_Area'] = solvent_subs_lod_correct1$Area - solvent_subs_lod_correct1$Solvent_Blank_Avg
  solvent_subs_lod_correct1[,'Solvent_Area_Ratio'] = solvent_subs_lod_correct1$Solvent_Blank_Avg/solvent_subs_lod_correct1$Area

  solvent_subs_lod_correct <- solvent_subs_lod_correct1 %>%
    filter(Solvent_Area_Ratio < 0.35) %>%
    subset(select = c(Sample, INCHIKEY, SMILES, Solvent_Sub_Area, Type, Metabolite_name, Average_Mz)) %>%
    pivot_wider(names_from = 'Sample', values_from = 'Solvent_Sub_Area')

  var_name_P <- colnames(solvent_subs_lod_correct[,grep("P1", colnames(solvent_subs_lod_correct))]) #create variable names to use later for any column with these strings in the name

  solvent_subs_lod_correct[var_name_P][solvent_subs_lod_correct[var_name_P]==0] <- NA #set any avalues in the previously defined columns that are equal 0 as NA#
  solvent_subs_lod_correct[var_name_P][solvent_subs_lod_correct[var_name_P]<0] <- NA
  solvent_subs_lod_correct[,'Spk_Sub_LOD'] <- apply(solvent_subs_lod_correct[,which(colnames(solvent_subs_lod_correct) == 'P12_B_001'):which(colnames(solvent_subs_lod_correct) == 'P16_D3_001')],1,min,na.rm = TRUE)/5

  for (var in var_name_P){
    solvent_subs_lod_correct[[var]][is.na(solvent_subs_lod_correct[[var]])] <- solvent_subs_lod_correct$Spk_Sub_LOD[is.na(solvent_subs_lod_correct[[var]])] #change to lod
  }

  #get percent detection in participant samples#
  solvent_subs_lod_correct[,'Detection_Count'] = NA #empty column for detection count#

  solvent_subs_lod_correct <- solvent_subs_lod_correct %>% mutate(Detection_Count = rowSums(.[grepl('P1', colnames(.))] != Spk_Sub_LOD)) #change row for detection count to the number of columns that are worn samples not equal the LOD
  number_worn_samples2 = sum(grepl(paste0("P1"),names(solvent_subs_lod_correct))) #count number of samples#

  solvent_subs_lod_correct[,'Percent_detection'] = solvent_subs_lod_correct$Detection_Count*100/number_worn_samples2 #find percent detection
  df <- solvent_subs_lod_correct
  df
}

All_no_dups_solv_sub <- solv_sub_analysis(All_no_dups)

var_name_solv_blank <- colnames(All_no_dups_solv_sub[,grep("_B_", colnames(All_no_dups_solv_sub))])
All_no_dups_solv_sub[,'Blank_Avg'] = apply(All_no_dups_solv_sub[var_name_solv_blank], 1, mean, na.rm = TRUE)
var_name_solv_sample <- colnames(All_no_dups_solv_sub[,grep("_D", colnames(All_no_dups_solv_sub))])
All_no_dups_solv_sub[,'Sample_Avg'] = apply(All_no_dups_solv_sub[var_name_solv_sample], 1, mean, na.rm = TRUE)

All_no_dups_solv_sub[,'log_FC'] = log2(All_no_dups_solv_sub$Sample_Avg/All_no_dups_solv_sub$Blank_Avg)
All_no_dups_solv_sub[,'Fold_Category'] = NA
All_no_dups_solv_sub$Average_Mz<- as.numeric(All_no_dups_solv_sub$Average_Mz)
FC_all_n <- All_no_dups_solv_sub %>%
  mutate(Fold_Category = ifelse(log_FC > 0, 'Higher in Worn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC < 0, 'Higher in Unworn', Fold_Category)) %>%
  nrow()

All_no_dups_solv_sub_stats <- All_no_dups_solv_sub%>%
  mutate(Fold_Category = ifelse(log_FC > 0, 'Higher in Worn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC < 0, 'Higher in Unworn', Fold_Category)) %>%
  group_by(Fold_Category) %>%
  get_summary_stats(log_FC, type = 'common')

plot_FC_all <- All_no_dups_solv_sub %>%
  mutate(Fold_Category = ifelse(log_FC > 0, 'Higher in Worn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC < 0, 'Higher in Unworn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC == 0, 'none', Fold_Category)) %>%
  ggplot(aes(x = Average_Mz, y = log_FC, colour = Fold_Category))+
  geom_point(size = 4, alpha = 0.6) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 'solid')+
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    legend.text = element_text(size = 25),
    title = element_text(size = 25))+
  labs(y = expression('Log'[2]*' Fold Difference (Peak Area)'), x = 'Average m/z', title =
         paste0('n = ',FC_all_n)) +
  scale_colour_manual(values = c('#FBB4AE', '#B3CDE3', 'grey')) +
 scale_y_continuous(limits = c(-12,14))+
  theme(legend.title=element_blank(), legend.position = 'bottom')+
  theme(legend.position = 'right')+
  #scale_shape_manual(values = c(16, 17, 13, 18, 19)) +  # Use solid circle, triangle, square, and diamond
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    legend.text = element_text(size = 25),
    title = element_text(size = 25)) +# Adjust font size
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))+
  guides(colour = 'none')
plot_FC_all
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/Fold_Plot_All_Feb27.svg", plot = plot_FC_all, base_width=5, base_height=8.34)
plot_FC_all_data <- All_no_dups_solv_sub %>%
  mutate(Fold_Category = ifelse(log_FC > 0, 'Higher in Worn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC < 0, 'Higher in Unworn', Fold_Category)) %>%
  mutate(Fold_Category = ifelse(log_FC == 0, 'none', Fold_Category)) %>% 
  group_by(Fold_Category) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Fold_Category, values_from = n)
#write.csv(plot_FC_all_data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Fold_Change_All_Data.csv", row.names=FALSE) #export final data frame#
higher_in_worn <- plot_FC_all_data$`Higher in Worn`/(plot_FC_all_data$`Higher in Unworn`+plot_FC_all_data$`Higher in Worn`)
max_fold_diff <- 2^(max(All_no_dups_solv_sub$log_FC))

##Venn Diagram######################
#start from the df that still has duplicates across methods. Our goal now is to filter so that each method has only only 1 replicate#
#use data prior to any filtering at all#
########LC Data - do separately because SMILES will filter out all of the GC data#######
######filter by metabolomic name######
# Remove duplicates, keeping the row with the greatest mz silimarity with the greatest QC pooled column

remove_duplicates <- function(df){
  MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected <- df %>%
    filter(m.z_similarity == 1) %>% #filter for mz similarity of 1 - also filters out GC data but all data other than that should already by mz = 1
    group_by(SMILES) %>%
    filter(!is.na(SMILES) & SMILES != 'null')%>%
    slice_max(order_by = QC_Pool_Blank_Sub, n =1) %>%#take highest of each unique SMILE and type combination for QC Pool blank sub
    ungroup()

  MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>%
    group_by(INCHIKEY) %>%
    filter(!is.na(INCHIKEY) & INCHIKEY != 'null')%>%
    slice_max(order_by = QC_Pool_Blank_Sub, n =1) %>%#take highest of each unique SMILE and type combination for QC Pool blank sub
    ungroup()

  df <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>%
    group_by(Metabolite_name) %>%
    filter(!is.na(Metabolite_name) & Metabolite_name != 'null' & Metabolite_name != 'Unknown')%>%
    slice_max(order_by = QC_Pool_Blank_Sub, n =1) %>%#take highest of each unique SMILE and type combination for QC Pool blank sub
    ungroup()
}

hilic_pos_nd <- mz1_all %>%
  filter(QC_Pool_Blank_Ratio < 0.35) %>%
  filter(Type == 'Hilic_Positive') %>%
  remove_duplicates() %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2' because it's not in the GC names
  subset(select = -c(Discard))
hilic_neg_nd <- mz1_all %>%
  filter(QC_Pool_Blank_Ratio < 0.35) %>%
  filter(Type == 'Hilic_Negative') %>%
  remove_duplicates() %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2' because it's not in the GC names
  subset(select = -c(Discard))
c18_pos_nd <- mz1_all %>%
  filter(QC_Pool_Blank_Ratio < 0.35) %>%
  filter(Type == 'C18_Positive') %>%
  remove_duplicates() %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2' because it's not in the GC names
  subset(select = -c(Discard))

c18_neg_nd <- mz1_all %>%
  filter(QC_Pool_Blank_Ratio < 0.35) %>%
  filter(Type == 'C18_Negative') %>%
  remove_duplicates() %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2' because it's not in the GC names
  subset(select = -c(Discard))

remove_duplicates_gc <- function(df){
  MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected <- df %>%
    filter(Total_score > 0.9) %>% #filter for mz similarity of 1 - also filters out GC data but all data other than that should already by mz = 1
    group_by(SMILES) %>%
    filter(!is.na(SMILES) & SMILES != 'null')%>%
    slice_max(order_by = QC_Pool_Blank_Sub, n =1) %>%#take highest of each unique SMILE and type combination for QC Pool blank sub
    ungroup()

  MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>%
    group_by(INCHIKEY) %>%
    filter(!is.na(INCHIKEY) & INCHIKEY != 'null')%>%
    slice_max(order_by = QC_Pool_Blank_Sub, n =1) %>%#take highest of each unique SMILE and type combination for QC Pool blank sub
    ungroup()

  df <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>%
    group_by(Metabolite_name) %>%
    filter(!is.na(Metabolite_name) & Metabolite_name != 'null' & Metabolite_name != 'Unknown')%>%
    slice_max(order_by = QC_Pool_Blank_Sub, n =1) %>%#take highest of each unique SMILE and type combination for QC Pool blank sub
    ungroup()
}

GC_INCHIKEY_List <- mz1_gc %>%
  subset(select = c(Type, Metabolite_name, INCHIKEY, SMILES))

EI_nd1 <- mz1_all %>%
  filter(QC_Pool_Blank_Ratio < 0.35) %>%
  filter(Type == 'EI') %>%
  subset(select = -c(INCHIKEY, SMILES))

EI_nd <- merge(EI_nd1, GC_INCHIKEY_List, by = c('Metabolite_name', 'Type'), all = T) %>%
  remove_duplicates_gc() %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2' because it's not in the GC names
  subset(select = -c(Discard))

redo_worn_analysis <- function(df){
  participants <- df %>%
    pivot_longer(cols = starts_with("P1"), names_to = 'Sample', values_to = 'Area') %>% #area is just istd corrected at this point#
    subset(select = -c(Adduct_type, Post_curation_result, Fill_percent, Reference_RT, `Annotation_tag_(VS1.0)`, RT_matched, Comment, Manually_modified_for_quantification, Manually_modified_for_annotation, Isotope_tracking_parent_ID,
                       Isotope_tracking_weight_number, Isotope_tracking_weight_number2, Spectrum_reference_file_name, MS1_isotopic_spectrum, MS.MS_spectrum))

  participants[, 'Participant_Number'] = NA
  participants[, 'Sample_Type'] = NA

  participants <- participants %>%   mutate(Sample_Type = ifelse(grepl('_B_', Sample), 'Blank', Sample_Type)) %>% #label samples as blank vs sample
    mutate(Sample_Type = ifelse(grepl('_D', Sample), 'Sample', Sample_Type)) %>%
    mutate(Participant_Number = ifelse(grepl('P12', Sample), '12', Participant_Number)) %>% #label samples with participant nu,ber
    mutate(Participant_Number = ifelse(grepl('P14', Sample), '14', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P15', Sample), '15', Participant_Number)) %>%
    mutate(Participant_Number = ifelse(grepl('P16', Sample), '16', Participant_Number))

  #isolate blank samples
  Participant_Blanks <- participants %>% filter(Sample_Type == 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))
  #sanity check - should produce 0
  check_blanks <- nrow(Participant_Blanks) - (nrow(participants)/4) #blanks are a quarter of samples

  #prepare non-blanks#
  not_blanks <- participants %>%filter(Sample_Type != 'Blank') %>% subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Participant_Number, Sample, Area, Type, QC_Pool_Blank_Sub, Average_Mz))
  colnames(participants)
  #merge back together
  Participants_Blank_Correct <- merge(Participant_Blanks, not_blanks, by = c('Alignment_ID', 'Average_Mz','Metabolite_name', 'INCHIKEY', 'SMILES', 'Participant_Number', 'Type', 'QC_Pool_Blank_Sub'), all = T) %>%
    dplyr::rename(Blank_Area = Area.x) %>%
    dplyr::rename(Sample_Area = Area.y)

  #sanity check on merge - should produce 0 value
  nrow(not_blanks) - nrow(Participants_Blank_Correct)

  #correct for blank
  Participants_Blank_Correct[,'Blank_Correction'] = Participants_Blank_Correct$Sample_Area - Participants_Blank_Correct$Blank_Area
  Participants_Blank_Correct[,'Blank_Ratio'] = Participants_Blank_Correct$Blank_Area/Participants_Blank_Correct$Sample_Area

  #filter out high blanks
  Participants_Blank_Filtered <- Participants_Blank_Correct %>% filter(Blank_Ratio < 0.35)

  #pivot wider to get lods#
  #AG Note: the filtered out samples will still be in the df but as NAs#
  Participants_Blank_Correct_Wide <- Participants_Blank_Filtered %>%
    subset(select = c(Alignment_ID, Metabolite_name, SMILES, INCHIKEY, Type, Sample.y, Blank_Correction, QC_Pool_Blank_Sub, Average_Mz)) %>%
    pivot_wider(values_from = Blank_Correction, names_from = Sample.y)

  #sanity check - should give 0 row tibble#
  pivot_check <- Participants_Blank_Correct_Wide %>% group_by(Metabolite_name) %>% filter(n()>1)

  #get LOD above blanks#

  var_name <- colnames(Participants_Blank_Correct_Wide[,grep("P1", colnames(Participants_Blank_Correct_Wide))])

  Participants_Blank_Correct_Wide[var_name][Participants_Blank_Correct_Wide[var_name]<0] <- NA
  Participants_Blank_Correct_Wide[var_name][Participants_Blank_Correct_Wide[var_name]==0] <- NA

  #@AG: edit this line so it's not manual - the columns used keeps changing so double check#
  Start <- which(colnames(Participants_Blank_Correct_Wide)=="Type") + 1
  End <- which(colnames(Participants_Blank_Correct_Wide)=="P12_D2_001") #adjust this wuth appropriate last column

  Participants_Blank_Correct_Wide[, "LOD_Worn_Sub"] <- apply(Participants_Blank_Correct_Wide[,Start:End], 1, min, na.rm = TRUE)/5

  for (var in var_name){
    Participants_Blank_Correct_Wide[[var]][is.na(Participants_Blank_Correct_Wide[[var]])] <- Participants_Blank_Correct_Wide$LOD_Worn_Sub[is.na(Participants_Blank_Correct_Wide[[var]])]
  }

  head(Participants_Blank_Correct_Wide)

  #get percent detection#
  Participants_Blank_Correct_Wide[,'Detection_Count'] = NA #empty column for detection count#

  Participants_Blank_Correct_Wide <- Participants_Blank_Correct_Wide %>% mutate(Detection_Count = rowSums(.[grepl('_D', colnames(.))] != LOD_Worn_Sub)) #change row for detection count to the number of columns that are worn samples not equal the LOD

  number_worn_samples = sum(grepl(paste0("_D"),names(Participants_Blank_Correct_Wide))) #count number of samples#

  Participants_Blank_Correct_Wide[,'Percent_Detection'] = Participants_Blank_Correct_Wide$Detection_Count*100/number_worn_samples #find percent detection
  df <-Participants_Blank_Correct_Wide
  df
}

#check for remaining duplicates - msdial sometimes assigns two of the same peak different alignment ids but they'll have different samples despite the same QCP#

MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected1 <- rbind(EI_nd, hilic_pos_nd, hilic_neg_nd, c18_pos_nd, c18_neg_nd)
 # redo_worn_analysis()
# filter(Percent_Detection == 50 | Percent_Detection > 50)

Venn_Remaining_Dups <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected1 %>% group_by(Type, Metabolite_name) %>% filter(n()>1)

Venn_Remaining_Dups$Sample.Avg = rowMeans(Venn_Remaining_Dups[ , c(45:47, 49:52, 54:56, 58:60)], na.rm=TRUE)

Venn_Remaining_Dups_filtered <-Venn_Remaining_Dups%>%
  group_by(Metabolite_name, Type) %>%
  slice_max(order_by = Sample.Avg, n = 1) %>%  # Keep the row with the largest QC_Pooled value for each metabolite group
  ungroup() %>%  # if there are still reamining duplicates at this point, they should be identical#
  group_by(Metabolite_name, Type, Sample.Avg) %>%
  slice(1)

Venn_not_Remaining_Dups <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected1 %>% group_by(Type, Metabolite_name) %>% filter(n()==1)

MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected <- rbind(Venn_not_Remaining_Dups, Venn_Remaining_Dups_filtered) #put split data frame back together

test <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>% group_by(Metabolite_name, Type) %>% filter(n() > 1)

#turn into list for it to work with ven diagram#
venn_data <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>% mutate(Type = ifelse(Type == 'C18_Negative', 'C18-ESI\u207B', Type)) %>%
  mutate(Type = ifelse(Type == 'C18_Positive', 'C18-ESI\u207A', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Negative', 'HILIC-ESI\u207B', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Positive', 'HILIC-ESI\u207A', Type))

#write.csv(venn_data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Venn_data.csv", row.names=FALSE) #export final data frame#

overlap <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>%
  filter(Type != 'EI') %>%
  filter(SMILES != 'null')  %>%
  group_by(INCHIKEY) %>%
  filter(n()==4)%>%
  group_by(INCHIKEY) %>%
  slice(1) %>%
  subset(select = c(Metabolite_name, INCHIKEY, SMILES))

#write.csv(only_samples, "/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/SI_Other_Data_Sets/Features_only_in_samples.csv")


ven_list <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>%
  filter(Type != 'EI') %>%
  filter(SMILES != 'null')  %>%
  mutate(Type = ifelse(Type == 'C18_Negative', 'C18-ESI\u207B', Type)) %>%
  mutate(Type = ifelse(Type == 'C18_Positive', 'C18-ESI\u207A', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Negative', 'HILIC-ESI\u207B', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Positive', 'HILIC-ESI\u207A', Type)) %>%
  unstack(INCHIKEY ~ Type)

ven <- ggVennDiagram(ven_list, stroke_size = 0.5, set_size = 8,
                     label_size = 6, label_percent_digit = 1, label = 'both')+
  #scale_fill_manual(values = c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3'))+
  #scale_fill_gradient(low = '#Fab000', high = '#619CFF', name = 'Count')+#
  scale_x_continuous(expand = expansion(mult = .2))+ #expand plot so titles aren't cut off#
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = 'Count')
#scale_fill_gradient(low = '#FFFFF0', high = '#619CFF', name = 'Count')+#
#scale_fill_brewer(palette = 'Set1')#
theme(
  legend.title = element_text(size = 20), legend.text = element_text(size = 18),
  title = element_text(size = 25), strip.text = element_text(size = 18)) #strip.text for plot labels#
ven

ven3 <- ggVennDiagram(ven_list, stroke_size = 0.5, set_size = 8,
                      label_size = 6, label_percent_digit = 2, label = 'both')+
  #scale_fill_manual(values = c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3'))+
  #scale_fill_gradient(low = '#Fab000', high = '#619CFF', name = 'Count')+#
  scale_x_continuous(expand = expansion(mult = .2))+ #expand plot so titles aren't cut off#
  scale_fill_distiller(palette = "Reds", direction = 1, name = "Count")+
theme(
  legend.title = element_text(size = 20), legend.text = element_text(size = 18),
  title = element_text(size = 25), strip.text = element_text(size = 18)) +# Customize theme
#scale_fill_gradient(low = '#FFFFF0', high = '#619CFF', name = 'Count')+#
#scale_fill_brewer(palette = 'Set1')#
  ggplot2::theme(legend.position = "right")
ven3
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/Venn_Diagram_Feb27.svg", plot = ven3, base_width=8.34, base_height=7)
#

heatmap_palette <- colorRampPalette(c("#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"))(100)

ven_colours2 <- ggVennDiagram(ven_list, stroke_size = 0.5, set_size = 8,
                              label_size = 6, label_percent_digit = 2, alpha = 0.5, label = 'percent') +
  scale_x_continuous(expand = expansion(mult = .2)) + # Expand plot so titles aren't cut off
  scale_fill_gradientn(colors = heatmap_palette, name = 'Count') + # Use scale_fill_gradientn for continuous fill
  theme(
    legend.title = element_text(size = 20), legend.text = element_text(size = 18),
    title = element_text(size = 25), strip.text = element_text(size = 18)) # Customize theme

ven_colours2


#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/venn_Feb2.svg", plot = ven, base_width=13, base_height=10)
brewer.pal(6, "Pastel1")

pastel1 <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4",'#FED9A6')
set1 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",'#FED9A6')

# Create the Venn diagram with manual colors
venn_plot_manual <- ggvenn(ven_list, stroke_size = 0.7,fill_alpha = 0.2,
                           fill_color = set1, set_name_size = 7, text_size = 6, show_percentage = FALSE)+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 18),
        title = element_text(size = 25), strip.text = element_text(size = 18))+
  scale_x_continuous(expand = expansion(mult = 0.3))+ #expand plot so titles aren't cut off#
  scale_y_continuous(expand = expansion(mult = 0.2)) #expand plot so titles aren't cut off#

venn_plot_manual
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/venn_Feb2.pdf", plot = venn_plot_manual, base_width=13, base_height=10)


#get the 54 compounds overlapping#
MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected$Sample.Avg <- as.numeric(MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected$Sample.Avg)
MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected$Alignment_ID <- as.numeric(MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected$Alignment_ID)

overlap <- MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected %>% #doesn't work if we use metabolite name
  filter(Type != 'EI') %>%
  group_by(SMILES) %>%
  filter(n() == 4) %>%
  subset(select = c(SMILES))
unique(overlap$SMILES)

#merge smiles back with metabolite name
overlap_merged <- merge(MERGED_ALL_NO_DUPS_PER_METHOD_SUBSET_LOD_Corrected, overlap, by = c('SMILES'), all = T) %>%
  filter(!is.na(n))

#overlap_clean <- overlap_merged %>% group_by(Metabolite_name, SMILES) %>% get_summary_stats(Alignment_ID, type = 'common') %>%
#  group_by(SMILES) %>%
##  arrange(SMILES, desc(mean)) %>%
#  slice_max(order_by = mean, n = 1) %>%
#  ungroup()%>%
#  subset(select = c(Metabolite_name, SMILES)) %>%
#  separate(Metabolite_name, into = c("Discard", "Metabolite_name"), sep = ": ") %>%
#  subset(select = -c(Discard))

#write_xlsx(overlap_clean,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Method_Overlap.xlsx") #export final data frame


########venn diagram for positive vs negative vs EI#######

gc_lc_venn <- mz1_all

gc_lc_venn[,'Ionization'] = NA

gc_lc_venn<- gc_lc_venn %>%
  mutate(Ionization = ifelse(Type == 'C18_Positive', 'ESI Positive', Ionization)) %>%
  mutate(Ionization = ifelse(Type == 'Hilic_Positive', 'ESI Positive', Ionization)) %>%
  mutate(Ionization = ifelse(Type == 'Hilic_Negative', 'ESI Negative', Ionization)) %>%
  mutate(Ionization = ifelse(Type == 'Hilic_Negative', 'ESI Negative', Ionization)) %>%
  mutate(Ionization = ifelse(Type == 'EI', 'EI', Ionization))  %>%
  filter(QC_Pool_Blank_Ratio < 0.35)

gc_lc_venn_it1 <- gc_lc_venn %>%
  filter(Ionization != 'EI') %>%
  filter(!is.na(SMILES) & SMILES != 'null') %>%
  filter(m.z_similarity == 1) %>% #filter for mz similarity of 1 - also filters out GC data but all data other than that should already by mz = 1
  arrange(SMILES, Ionization, desc(QC_Pool_Blank_Sub)) %>% ##take highest sample average (doesn't really matter what we use here)
  group_by(SMILES, Ionization) %>%
  slice(1) %>%#take highest of each unique SMILE and type combination for QC Pool blank sub
  arrange(Alignment_ID)

gc_lc_venn_it2 <- gc_lc_venn_it1 %>%
  filter(!is.na(INCHIKEY) & INCHIKEY != 'null') %>%
  arrange(INCHIKEY, Ionization, desc(QC_Pool_Blank_Sub)) %>%
  group_by(INCHIKEY, Ionization) %>%
  slice(1) %>%
  arrange(Alignment_ID)

gc_lc_venn_it3 <- gc_lc_venn_it2 %>%
  filter(m.z_similarity == 1) %>%
  filter(!is.na(Metabolite_name) & Metabolite_name != 'null' & Metabolite_name != 'Unknown') %>%
  arrange(Metabolite_name, Ionization, desc(QC_Pool_Blank_Sub)) %>%
  group_by(Metabolite_name, Ionization) %>%
  slice(1) %>%
  arrange(Alignment_ID)

gc_lc_venn_gc1 <- gc_lc_venn %>%
  filter(Total_score > 0.9) %>%
  filter(Ionization == 'EI')
gc_lc_venn_gc <- rbind(gc_lc_venn_gc1, gc_lc_venn_it3) %>%
  filter(!is.na(Alignment_ID)) %>% #filter out inchikeys that didn't match anything
  arrange(Metabolite_name, Ionization, desc(QC_Pool_Blank_Sub)) %>%
  group_by(Metabolite_name, Ionization) %>%
  filter(!is.na(Metabolite_name) & Metabolite_name != 'null' & Metabolite_name != 'Unknown') %>%
  slice(1) %>%
  ungroup() %>%
  arrange(INCHIKEY, Ionization, desc(QC_Pool_Blank_Sub)) %>%
  filter(!is.na(INCHIKEY) & INCHIKEY != 'null') %>%
  group_by(INCHIKEY, Ionization) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(SMILES, Ionization, desc(QC_Pool_Blank_Sub)) %>%
  filter(!is.na(SMILES) & SMILES != 'null') %>%
  group_by(SMILES, Ionization) %>%
  slice(1) %>%
  ungroup()

gc_lc_venn_final <- rbind(gc_lc_venn_it3, gc_lc_venn_gc)

#turn into list for it to work with ven diagram#
gclcven_list <- gc_lc_venn_final %>%
  #filter(SMILES != 'null') %>%#
  unstack(INCHIKEY ~ Ionization)
#write.csv(gc_lc_venn_final,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/GC_LC_Venn_Data.csv", row.names=FALSE) #export final data frame#

gclcven <- ggVennDiagram(gclcven_list, stroke_size = 0.5, set_size = 8,
                         label_size = 6, label_percent_digit = 2, alpha = 0.5, label = 'percent') +
  scale_x_continuous(expand = expansion(mult = .2)) + # Expand plot so titles aren't cut off
  scale_fill_distiller(palette = "Reds", direction = 1, name = "Count")+
  theme(
    legend.title = element_text(size = 20), legend.text = element_text(size = 18),
    title = element_text(size = 25), strip.text = element_text(size = 18)) # Customize theme

gclcven
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/GC_LC_Ven_Feb27.svg", plot = gclcven, base_width=8, base_height=6)

######import classyfire######

CL_Classification_df_combinedwide1 <- read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/CL_Classification_df_combinedwide1.csv", col_types = cols(.default = "c"))
CL_Classification_df_combinedwide <- CL_Classification_df_combinedwide1[!duplicated(CL_Classification_df_combinedwide1), ]

CL_Classification_df_combined <- CL_Classification_df_combinedwide %>%
  subset(select = c(InChIKey, Superclass, Class, Subclass, Kingdom)) %>%
  dplyr::rename(Compound = InChIKey) %>%
  dplyr::rename(superclass = Superclass) %>%
  dplyr::rename(class = Class) %>%
  dplyr::rename(subclass = Subclass) %>%
  dplyr::rename(kingdom = Kingdom) %>%
  pivot_longer(cols = c(superclass, class, subclass, kingdom), names_to = 'Level', values_to = 'Classification')

all_levels_present<-CL_Classification_df_combinedwide %>%
  subset(select = c(InChIKey, Superclass, Class, Subclass, Kingdom)) %>%
  na.omit() %>%
  dplyr::rename(INCHIKEY = InChIKey)
all_levels_present_mass <- merge(heatmap_features,all_levels_present, by = c('INCHIKEY'))
all_levels_present_mass$Average_Mz<-as.numeric(all_levels_present_mass$Average_Mz)
min(all_levels_present_mass$Average_Mz) # min mass used in dendrograms
max(all_levels_present_mass$Average_Mz) # max mass used in dendrograms

######Class vs mass######
CL_Classification_df_combined_nonas <- CL_Classification_df_combinedwide %>%
  subset(select = c(InChIKey, Superclass, Class, Subclass, Kingdom)) %>%
  na.omit()%>%
  dplyr::rename(Compound = InChIKey) %>%
  dplyr::rename(superclass = Superclass) %>%
  dplyr::rename(class = Class) %>%
  dplyr::rename(subclass = Subclass) %>%
  dplyr::rename(kingdom = Kingdom) %>%
  pivot_longer(cols = c(superclass, class, subclass, kingdom), names_to = 'Level', values_to = 'Classification')

Superclasses <- CL_Classification_df_combined_nonas %>% filter(Level == 'superclass') %>%
  dplyr::rename(INCHIKEY = Compound)

CL_Classifications_heatmap <- CL_Classification_df_combinedwide %>%
  dplyr::rename(INCHIKEY = InChIKey)

mass_vs_class <-merge(heatmap_features, Superclasses, by = c('INCHIKEY')) %>%
  #dplyr::mutate_all(as.character) %>%
  #subset(select = c(SMILES, Type, Classification, mean)) %>%
  as.data.frame() %>%
  group_by(INCHIKEY)

mass_vs_class <- mass_vs_class[!duplicated(mass_vs_class), ] #filter out duplicates that may have risen from merging with the classyfire output

mass_vs_class$Average_Mz <- as.numeric(mass_vs_class$Average_Mz)
#############import dendrogram packages########
if (!require("ape")) install.packages("ape", dependencies = TRUE)
library(ape)
library(gridGraphics)

######get colours for dendrogram#####
fills1 <- c(sapply(c("Set1"),
                   function(x) brewer.pal(9, x)[c(1:5, 7, 9)])) #combine set 1 and set 2 into a palette so there's enough colours
fills3 <- c(sapply(c("Set2"),
                   function(x) brewer.pal(9, x)[c(1,6,3,4,2,5,7)])) #combine set 1 and set 2 into a palette so there's enough colours

fills_combined <- c(fills1, fills3)


fills <- c(sapply(c("Set1", "Set2"),
                  function(x) brewer.pal(7, x))) #combine set 1 and set 2 into a palette so there's enough colours
fills_combined2 <- fills_combined[1:length(unique(CL_Classification_df_combinedwide$Superclass))] #get elgnth of colours equal to legnth of superclasses
superclass_colors <- setNames(fills_combined2, unique(CL_Classification_df_combinedwide$Superclass)) #assign colour per superclass
names(superclass_colors) <- unique(CL_Classification_df_combinedwide$Superclass)

CL_Classification_All_Filtered1 <- CL_Classification_df_combinedwide %>%
  subset(select = c(InChIKey, Superclass, Class, Subclass, Kingdom)) %>%
  na.omit() %>% #remove NAs so the legend doesn't get messed up
  group_by(Superclass) %>%
  filter(n()>10) %>%
  ungroup()%>%
  dplyr::rename(Compound = InChIKey) %>%
  mutate(Superclass = factor(Superclass,
                             levels = c('Lipids and lipid-like molecules', 'Organic acids and derivatives',
                                        'Organoheterocyclic compounds', 'Phenylpropanoids and polyketides',
                                        'Benzenoids', 'Organic oxygen compounds', 'Organic nitrogen compounds',
                                        'Organohalogen compounds', 'Nucleosides, nucleotides, and analogues')))

heatmap_features_renamed <- heatmap_features %>% subset(select = c(Metabolite_name, SMILES, INCHIKEY)) %>%
  dplyr::rename(Compound = INCHIKEY)

CL_Classification_All_Filtered <- merge(heatmap_features_renamed, CL_Classification_All_Filtered1, by = c('Compound'))

#write.csv(CL_Classification_All_Filtered,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Dendrogram_data_Mar3.csv", row.names=FALSE) #export final data frame#


fills_combined_filtered <- fills_combined[1:length(unique(CL_Classification_All_Filtered$Superclass))] #get elgnth of colours equal to legnth of superclasses
superclass_colors_filtered <- setNames(fills_combined_filtered, levels(CL_Classification_All_Filtered$Superclass)) # Ensure the color assignment matches the new order


CL_Classification_df_combined_filtered1 <- CL_Classification_df_combinedwide %>%
  group_by(Superclass) %>%
  filter(n() > 10) %>%
  ungroup()%>%
  subset(select = c(InChIKey, Superclass, Class, Subclass, Kingdom)) %>%
  dplyr::rename(Compound = InChIKey) %>%
  filter(Superclass == 'Benzenoids' | Superclass == 'Phenylpropanoids and polyketides')

CL_Classification_df_combined_filtered2 <- CL_Classification_df_combinedwide %>%
  group_by(Superclass) %>%
  filter(n() > 10) %>%
  ungroup()%>%
  subset(select = c(InChIKey, Superclass, Class, Subclass, Kingdom)) %>% #select columns needed
  dplyr::rename(Compound = InChIKey) %>% #rename to match function
  filter(Superclass == 'Lipids and lipid-like molecules'| Superclass == 'Organic acids and derivatives') #select for superclasses

CL_Classification_df_combined_filtered3 <- CL_Classification_df_combinedwide %>%
  group_by(Superclass) %>%
  filter(n() > 10) %>%
  ungroup()%>%
  subset(select = c(InChIKey, Superclass, Class, Subclass, Kingdom)) %>% #select columns needed
  dplyr::rename(Compound = InChIKey) %>% #rename to match function
  filter(Superclass != 'Benzenoids' & Superclass != 'Organic acids and derivatives' & Superclass != 'Lipids and lipid-like molecules' & Superclass != 'Phenylpropanoids and polyketides') #select for superclasses

phylogenic_tree <- function(df){
  Annotated_data_reorganizeddf <- as.data.frame(df)

  Annotated_data_reorganizeddf2 <- Annotated_data_reorganizeddf %>%
    mutate_at(.vars = c("Kingdom","Superclass", "Class", "Subclass"),
              .funs = list(~ifelse(.=="", NA, as.character(.))))

  data2 <- Annotated_data_reorganizeddf2[complete.cases(Annotated_data_reorganizeddf2[, c("Kingdom", "Superclass", "Class", "Subclass")]), ]

  clean_data <- data2 %>%
    filter(
      Subclass != 'NULL' &
        !is.na(Subclass) &
        !is.nan(Subclass) &
        Subclass != 'NA' &
        Subclass != '<NA>' &
        Subclass != "" &
        Subclass != " "
    )
  clean_data$Subclass <- gsub(",", "&", clean_data$Subclass)
  clean_data$Subclass <- gsub(" ", "_", clean_data$Subclass)
  clean_data$Subclass <- gsub("'", "qpt", clean_data$Subclass)

  #get number of each class

  n_distinct(clean_data$Subclass)
  n_distinct(clean_data$Superclass)
  n_distinct(clean_data$Class)

  # Construct a hierarchical Newick tree without numerical leaf nodes
  build_newick <- function(data) {
    kingdoms <- unique(data$Kingdom)
    branches <- sapply(kingdoms, function(kingdom) {
      kingdom_data <- data[data$Kingdom == kingdom, ]
      superclasses <- unique(kingdom_data$Superclass)
      superclass_branches <- sapply(superclasses, function(superclass) {
        superclass_data <- kingdom_data[kingdom_data$Superclass == superclass, ]
        classes <- unique(superclass_data$Class)
        class_branches <- sapply(classes, function(class) {
          class_data <- superclass_data[superclass_data$Class == class, ]
          subclasses <- unique(class_data$Subclass)
          # Remove numerical labels (like "1") when constructing the Newick string
          valid_subclasses <- subclasses[!subclasses %in% c("1")]
          paste0("(", paste(valid_subclasses, collapse = ","), "):0.5")
        })
        paste0("(", paste(class_branches, collapse = ","), "):0.5")
      })
      paste0("(", paste(superclass_branches, collapse = ","), "):0.5")
    })
    paste0("(", paste(branches, collapse = ","), ");")
  }
  # Generate the Newick string
  newick_string <- build_newick(clean_data)

  # Read the tree from the Newick string
  tree <- read.tree(text = newick_string)
  # Check for NA branch lengths and handle them
  if (is.null(tree$edge.length)) {
    # If edge.length is NULL, assign default lengths (e.g., 1) to all edges
    tree$edge.length <- rep(0.3, nrow(tree$edge))
  } else if (any(is.na(tree$edge.length))) {
    # If edge.length contains NAs, replace NAs with a default value (e.g., 1)
    tree$edge.length[is.na(tree$edge.length) | tree$edge.length == 'NaN'] <- 0.5}

  empty_tips <- tree$tip.label[tree$tip.label == "" | tree$tip.label == " "]

  # Map superclasses to unique colors
  # fills <- c(sapply(c("Set1", "Set2"),
  #    function(x) brewer.pal(7, x)))
  #fills <- fills[1:length(unique(clean_data$Superclass))]
  ## superclass_colors <- setNames(fills, unique(clean_data$Superclass))
  # names(superclass_colors) <- unique(clean_data$Superclass)

  # Initialize all edge colors as black
  edge_colors <- rep("black", nrow(tree$edge))
  # Assign colors to edges based on subclasses
  # Loop over each tip in the tree
  for (i in seq_along(tree$tip.label)) {
    tip_label <- tree$tip.label[i]  # Get current tip label

    # Find the superclass corresponding to the current tip
    superclass <- unique(clean_data$Superclass[clean_data$Subclass == tip_label])
    unique(clean_data$Subclass)
    # If the superclass is found and it has a corresponding color
    if (length(superclass) > 0 && superclass %in% names(superclass_colors_filtered)) {
      # Find edges leading to this tip (tip 'i')
      tip_edge <- which(tree$edge[, 2] == i)

      # Assign the color to the edges leading to this tip
      edge_colors[tip_edge] <- superclass_colors_filtered[superclass]
    }
  }

  # Check if there are any empty tip labels
  if (length(empty_tips) > 0) {
    warning("Empty or blank tip labels found: ", paste(empty_tips, collapse = ", "))

    # Remove empty tip labels
    tree$tip.label <- tree$tip.label[tree$tip.label != "" & tree$tip.label != " " & tree$tip.label != 'NA']}

  tree$tip.label <- gsub('&',',', tree$tip.label)
  tree$tip.label <- gsub('qpt',"'", tree$tip.label)

  # Plot the tree with edge colors
  treeplot <- plot(
    tree,  x.lim = c(-3.5,3.5),y.lim = c(-3.75,3.75),
    type = "fan",                     # Circular fan tree
    show.tip.label = TRUE,            # Show labels at the tips
    tip.color = 'black',              # Tip label color
    edge.color = edge_colors,  # Colored branches
    edge.width = 2,                   # Make branches thicker
    cex = 0.5                        # Adjust label size
  )
  treeplot
  # Add a legend for the superclasses
  #legend <- legend("topright", legend = names(superclass_colors), fill = superclass_colors, cex = 1, xpd = TRUE, inset = c(-0.0001, 0))
  # legend

}

par(#mfrow = c(3,2),#
  mar = c(0.5,0.5,0.5,0.5), bg = NA)

par(bg=NA)


phylogenic_tree(CL_Classification_All_Filtered)

# Filter the named vector
plot.new()#makenew plot

par(mar = c(1,1,1,1))
legend("center", legend = names(superclass_colors_filtered), fill = superclass_colors_filtered, cex = 1, ncol = 1, xpd = FALSE)

dev.off() #erase plot history
#####number of classifications#####
nfeatures <- merge(all_levels_present, heatmap_features, by = c('INCHIKEY'))
nrow(all_levels_present)-nrow(nfeatures)

total_features.phylo <- nfeatures %>%
  group_by(INCHIKEY) %>%
  slice(1) %>%
  nrow()

n.superclasses <- nfeatures %>%
  group_by(Superclass) %>%
  slice(1) %>%
  nrow()

n.subclass <- nfeatures %>%
  group_by(Subclass) %>%
  slice(1) %>%
  nrow()

n.class <- nfeatures %>%
  filter(!is.na(Class)) %>%
  group_by(Class) %>%
  slice(1) %>%
  nrow()

min(nfeatures$Average_Mz)
max(nfeatures$Average_Mz)

mass_vs_class_stats <- mass_vs_class %>%
  group_by(Classification) %>%
  get_summary_stats(Average_Mz, type = 'common')

mass_vs_class_means <- mass_vs_class %>%
  group_by(Classification) %>%
  filter(Classification == 'Lipids and lipid-like molecules')
min(mass_vs_class_means$Average_Mz)
max(mass_vs_class_means$Average_Mz)

mass_vs_class_means2 <- mass_vs_class %>%
  group_by(Classification) %>%
  filter(Classification == 'Organic acids and derivatives')
min(mass_vs_class_means2$Average_Mz)
max(mass_vs_class_means2$Average_Mz)

mass_vs_class_means2 <- mass_vs_class %>%
  group_by(Classification) %>%
  filter(Classification == 'Organic acids and derivatives') %>% ungroup()
min(mass_vs_class_means2$Average_Mz)
max(mass_vs_class_means2$Average_Mz)

mass_vs_class_means3 <- mass_vs_class %>%
  group_by(Classification) %>%
  filter(Classification == 'Organometallic compounds') %>% ungroup()
min(mass_vs_class_means3$Average_Mz)
max(mass_vs_class_means3$Average_Mz)

class_vs_mass <- mass_vs_class %>%
  group_by(Classification) %>%
  filter(!is.na(Classification)) %>%
  dplyr::summarise(J = n()) %>%
  mutate(Label = paste0(Classification,'\n(n = ',J,')'))
sum(class_vs_mass$J)-total_features.phylo
mass_vs_class_plot_data <- merge(class_vs_mass, mass_vs_class, by = c('Classification'), all = TRUE) %>%
  filter(!is.na(Label)) %>%
  group_by(Classification) %>%
  filter(n() > 10)
#write.csv(mass_vs_class_plot_data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Density_Data.csv", row.names=FALSE) #export final data frame#

mass_vs_class_plot
fills <- c(sapply(c("Set1", "Set2"),
                  function(x) brewer.pal(7, x))) #combine set 1 and 2 into one palette so there's enough fills

mass_vs_class_plot2 <- merge(class_vs_mass, mass_vs_class, by = c('Classification'), all = TRUE) %>%
  filter(!is.na(Label)) %>%
  mutate(Label = factor(Label, levels=c('Lipids and lipid-like molecules\n(n = 702)',
                                        'Phenylpropanoids and polyketides\n(n = 207)',
                                        'Organic nitrogen compounds\n(n = 28)',
                                        'Organic acids and derivatives\n(n = 317)',
                                        'Benzenoids\n(n = 175)',
                                        'Organohalogen compounds\n(n = 21)',
                                        'Organoheterocyclic compounds\n(n = 214)',
                                        'Organic oxygen compounds\n(n = 124)', 'Nucleosides, nucleotides, and analogues\n(n = 18)'))) %>%
  group_by(Classification) %>%
  filter(n() > 10) %>%
  ungroup()%>%
  ggplot(aes(x = Average_Mz)) +
  geom_density(alpha = 0.2, fill = '#E41A1C') +
  theme_classic()+
  facet_wrap(~Label, scales='free_y', dir = "v", strip.position = "top",
             axes = "all", nrow = 3)+
  #scale_fill_manual(values = fills)+
  #scale_colour_manual(values = fills)+
  labs(y = 'Density', x = 'Mass (m/z)')+
  theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20), legend.title = element_text(size = 20), legend.text = element_text(size = 20),
        title = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(colour="black", fill="white"))+ #adjust text size#
  theme(legend.position = 'none')

mass_vs_class_plot2

#######boxplots by class - right now uses all classyfires regardless of NAs in other rows######
CL_Classification_df_combined_filtered_rename<- CL_Classification_df_combinedwide %>%
  dplyr::rename(INCHIKEY = InChIKey)

CL_Classification_df_combined_filtered_Classmerged1 <- merge(CL_Classification_df_combined_filtered_rename, heatmap_features, by = c('INCHIKEY'), all = T) %>%
  filter(!is.na(Average_Mz))
nrow(CL_Classification_df_combined_filtered_Classmerged1)-nrow(heatmap_features)

CL_Classification_df_combined_filtered_Classmerged <- CL_Classification_df_combined_filtered_Classmerged1[!duplicated(CL_Classification_df_combined_filtered_Classmerged1), ] #filter out duplicates that may have risen from merging with the classyfire output


CL_Classification_df_combined_filtered_Class <- CL_Classification_df_combined_filtered_Classmerged %>%
  filter(Subclass == 'Phosphate esters' | Subclass == 'Alkyl fluorides') %>%
  filter(Metabolite_name != 'Ethanesulfonamide, 1,1,2,2,2-pentafluoro-') %>%#duplicate
  pivot_longer(cols = starts_with('P1'), names_to = 'Sample', values_to = 'log_notnorm_area')
CL_Classification_df_combined_filtered_Class[,'area'] = 10^(CL_Classification_df_combined_filtered_Class$log_notnorm_area)
alkyl_fluorides_stats <- CL_Classification_df_combined_filtered_Class %>% filter(Subclass == 'Alkyl fluorides') %>%
  group_by(Metabolite_name) %>%
  dplyr::summarise(mean = mean(area), sd = sd(area)) %>%
  ungroup()
alkyl_fluorides_stats[,'CV'] = alkyl_fluorides_stats$sd*100/alkyl_fluorides_stats$mean

min(alkyl_fluorides_stats$mean)
max(alkyl_fluorides_stats$mean)

OPE_stats <- CL_Classification_df_combined_filtered_Class %>% filter(Subclass == 'Phosphate esters') %>%
  group_by(Metabolite_name) %>%
  dplyr::summarise(mean = mean(area), sd = sd(area)) %>%
  ungroup()
OPE_stats[,'CV'] = OPE_stats$sd*100/OPE_stats$mean
min(OPE_stats$mean)
max(OPE_stats$mean)

boxplot_exo_data <- CL_Classification_df_combined_filtered_Class %>%  #data frame#
  mutate(Metabolite_name = ifelse(Metabolite_name == 'PFSM-perfluoroalkyl_sulfonamide; C5H2F11NO2S', 'Perfluoroalkyl sulfonamide', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'PFSM-carboxylic_acid; C5H6F5NO4S', 'PFSM-carboxylic acid 1', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'PFSM-carboxylic_acid; C6H8F5NO4S', 'PFSM-carboxylic acid 2', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == '3,3,4,4,5,5,6,6,7,7,8,8,9,9,9-Pentadecafluorononan-1-ol', 'PFNOL', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'N-Ethylperfluoroctansulfonamid (N-EtFOSA); LC-ESI-ITFT; MS2; CE', 'N-EtFOSA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Ethanesulfonamide, 1,1,2,2,2-pentafluoro-', 'PFEtSA', Metabolite_name)) %>%
  filter(Metabolite_name != 'Pesticide1_Monocrotophos_C7H14NO5P_Dimethyl (2E)-4-(methylamino)-4-oxo-2-buten-2-yl phosphate') %>%
  filter(Metabolite_name != '1-Butanesulfonamide, N-ethyl-1,1,2,2,3,3,4,4,4-nonafluoro-N-[2-(phosphonooxy)ethyl]-' &
           Metabolite_name != 'PFSM-carboxylic acid 1' & Metabolite_name != 'PFSM-carboxylic acid 2') %>%
  separate_wider_delim(Metabolite_name, "MS2:", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2'
  separate_wider_delim(Metabolite_name, "; P", names = c("Metabolite_name", "Discard2"), too_few = c("align_start")) %>% #use align start to address things without deliminator#
  separate_wider_delim(Metabolite_name, "!", names = c("Discard3", "Metabolite_name"), too_few = c("align_end")) %>%
  separate_wider_delim(Metabolite_name, "; C", names = c("Metabolite_name", "Discard4"), too_few = c("align_start")) %>%
  separate_wider_delim(Metabolite_name, "; L", names = c("Metabolite_name", "Discard5"), too_few = c("align_start")) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluoroundecanoic acid', 'PFUnDA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluorooctanesulfonic acid', 'PFOS', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluoroheptanoic acid', 'PFHpA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluorodecanoic acid', 'PFDA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluorobutanesulfonic acid', 'PFBS', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == '8:2 Fluorotelomer alcohol', '8:2 FTOH', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluoroalkyl sulfonamide', 'PFASA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == '1,1,2,2,2-Pentafluoroethanesulfonamide', 'PFEtSA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Tri(butoxyethyl) phosphate', 'TBOEP', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluorodecyl phosphate', '8:2 PAPS', Metabolite_name)) %>%
mutate(Metabolite_name = ifelse(Metabolite_name == 'Diphenyl phosphate', 'DPP', Metabolite_name)) %>%
mutate(Metabolite_name = ifelse(Metabolite_name == '1,1,2,2,2-pentafluoroethanesulfonamide', 'PFEtSA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Bis(2-ethylhexyl)phosphate', 'HDEHP', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'PFSM-carboxylic_acid', 'PFSM-carboxylic acid', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Diethyl phosphate', 'DPF', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Bis(2-ethylhexyl)phosphate', 'HDEHP', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == '1,1,2,2-Tetrahydroperfluoro-1-octadecanol', '16:2 FTOH', Metabolite_name)) %>%
mutate(Metabolite_name = ifelse(Metabolite_name == 'Perfluoroethanesulfonic acid', 'PFESA', Metabolite_name))

#write.csv(boxplot_exo_data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Boxplot_exo_data.csv", row.names=FALSE) #export final data frame#

nOPEs <- boxplot_exo_data %>% filter(Subclass == 'Phosphate esters') %>% group_by(Metabolite_name) %>% slice(1) %>% nrow()
nFluroides <- boxplot_exo_data %>% filter(Subclass == 'Alkyl fluorides') %>% group_by(Metabolite_name) %>% slice(1) %>% nrow()


boxplot_exo_classes <- boxplot_exo_data %>%
  ggplot(aes(y = reorder(Metabolite_name, log_notnorm_area, FUN = median), x = log_notnorm_area))+
  theme_classic()+
  geom_boxplot(fill = '#FBB4AE', alpha = 1, outliers = TRUE, width=0.6, outlier.size=2)+
  # stat_summary(fun=base::mean, colour="black", geom="point",shape='square', size=2, show.legend=FALSE,
  #            position = position_dodge2(width = 0.75, preserve = "single"))+ #show means#
  facet_nested_wrap(~Subclass, dir = "v", strip.position = "top",
                    axes = "all", remove_labels = "x", scales = 'free', nrow = 3)+  #split plots#
  theme(strip.background = element_rect(colour = "black", fill = "white"))+
  labs(x = expression('log'[10]* ' Area'), y = ' ', fill = 'Compound')+
  theme(axis.title.x = element_text(size = 37), axis.title.y = element_text(size = 30), axis.text.x = element_text(size = 30), axis.text.y = element_text(size = 30), legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),title = element_text(size = 15), strip.text = element_text(size = 37))  +
  theme(legend.position = "right")+
  #scale_fill_brewer(palette = 'Set2')+
  force_panelsizes(rows = c(nFluroides,nOPEs))+
  scale_x_continuous(limits = c(-6,4))+
  guides(fill="none", colour = 'none') #remove legend
boxplot_exo_classes

#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/Boxplot_exogenous.svg", plot = boxplot_exo_classes, base_width=12, base_height=14)

CL_Classification_df_combined_filtered_endo <- CL_Classification_df_combined_filtered_Classmerged %>%
  filter(Subclass == 'Amino acids, peptides, and analogues' | Subclass == 'Bile acids, alcohols and derivatives') %>%
  pivot_longer(cols = starts_with('P1'), names_to = 'Sample', values_to = 'log_notnorm_area') %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2'
  separate_wider_delim(Metabolite_name, "; P", names = c("Metabolite_name", "Discard2"), too_few = c("align_start")) %>% #use align start to address things without deliminator#
  separate_wider_delim(Metabolite_name, "!", names = c("Discard3", "Metabolite_name"), too_few = c("align_end")) %>%
  separate_wider_delim(Metabolite_name, "; C", names = c("Metabolite_name", "Discard4"), too_few = c("align_start")) %>%
  separate_wider_delim(Metabolite_name, "; L", names = c("Metabolite_name", "Discard5"), too_few = c("align_start")) %>%
  filter(Subclass == 'Bile acids, alcohols and derivatives' | Metabolite_name == 'Aspartic acid'|
           Metabolite_name == 'GLYCINE' | Metabolite_name == 'Isoleucine (Not validated)' | Metabolite_name == 'Phenylalanine (Not validated); PlaSMA ID-143' |
           Metabolite_name == 'Serine' | Metabolite_name == 'Proline' | Metabolite_name == 'Asp-Glu' | Metabolite_name == 'Phenylalanylvaline'|
           Metabolite_name == 'Glycyl-L-proline' | Metabolite_name == 'Prolylproline' | Metabolite_name == 'Gly-Val' | Metabolite_name == 'Leucylproline'|
           Metabolite_name == 'Glutamyltyrosine' | Metabolite_name == 'Glutamylphenylalanine' | Metabolite_name == 'Gly-Gly-Gly'|Metabolite_name == 'Aspartic acid' |
           Metabolite_name == 'Glu-Val-Phe'|Metabolite_name == 'Glycyl-L-proline; LC-ESI-QTOF; MS2; CE' | Metabolite_name =='Glutamylleucine; PlaSMA ID-1148'|
           Metabolite_name == 'Prolylproline' | Metabolite_name == 'Leucylproline; PlaSMA ID-901' | Metabolite_name == 'Glutamyltyrosine; PlaSMA ID-626'|
           Metabolite_name == 'Glutamic acid' | Metabolite_name == 'Glutamine' | Metabolite_name == 'Phenylglycine; PlaSMA ID-237' |
           Metabolite_name == 'Glutathione (reduced)')

boxplot_endo_data <- CL_Classification_df_combined_filtered_endo %>%  #data frame#
  filter(Metabolite_name != '2,3,14-trihydroxy-10,13-dimethyl-17-(2,3,6,7-tetrahydroxy-5,6-dimethylheptan-2-yl)-2,3,4,5,9,11,12,15,16,17-decahydro-1H-cyclopenta[a]phenanthren-6-one'&
           Metabolite_name != '(R)-4-((3R,5R,8R,9S,10S,12S,13R,14S,17R)-3,12-dihydroxy-10,13-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-(pyridin-2-ylmethyl)pentanamide' &
           Metabolite_name != '2-((4R)-4-((3R5R9S10S13R14S17R)-3-hydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-methylpentanamido)ethane-1-sulfonic acid\"\"' &
           Metabolite_name != '(3R7R8R9S10S12S13R14S17R)-17-((2R5R)-57-dihydroxyheptan-2-yl)-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthrene-3712-triol\"\"' &
           Metabolite_name != 'ethyl (R)-4-((5R7R8R9S10S13R14S17R)-7-hydroxy-1013-dimethyl-312-dioxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoate\"\"' &
           Metabolite_name != '2-((4R)-4-((3R5S7S9S10S13R14S17R)-37-dihydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-22-dimethylpentanamido)ethane-1-sulfonic acid""' &
           Metabolite_name != '2-((4R)-4-((5R7R8R9S10S12S13R17R)-712-dihydroxy-1013-dimethyl-3-oxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanamido)ethane-1-sulfonic acid\"\"' &
           Metabolite_name != '(R)-4-((8S9S10R13R14S17R)-1013-dimethyl-3-oxo-2367891011121314151617-tetradecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"' &
           Metabolite_name != 'NCGC00385132-01_C23H28O7_1-Phenanthrenecarboxylic acid, 10-(acetyloxy)-1,2,4a,9,10,10a-hexahydro-5,9-dihydroxy-1,4a-dimethyl-7-(1-methylethyl)-2-oxo-, methyl ester, (1R,4aS,9S,10S)-' &
           Metabolite_name != 'methyl 2,4,10-triacetyloxy-5,9-dihydroxy-1,4a-dimethyl-7-propan-2-yl-2,3,4,9,10,10a-hexahydrophenanthrene-1-carboxylate') %>%
  filter(Metabolite_name != '2,3,14-trihydroxy-10,13-dimethyl-17-(2,3,6,7-tetrahydroxy-5,6-dimethylheptan-2-yl)-2,3,4,5,9,11,12,15,16,17-decahydro-1H-cyclopenta[a]phenanthren-6-one'&
           Metabolite_name != '(R)-4-((3R,5R,8R,9S,10S,12S,13R,14S,17R)-3,12-dihydroxy-10,13-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-(pyridin-2-ylmethyl)pentanamide' &
           Metabolite_name != '2-((4R)-4-((3R5R9S10S13R14S17R)-3-hydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-methylpentanamido)ethane-1-sulfonic acid\"\"' &
           Metabolite_name != '(3R7R8R9S10S12S13R14S17R)-17-((2R5R)-57-dihydroxyheptan-2-yl)-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthrene-3712-triol\"\"' &
           Metabolite_name != 'ethyl (R)-4-((5R7R8R9S10S13R14S17R)-7-hydroxy-1013-dimethyl-312-dioxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoate\"\"' &
           Metabolite_name != '2-((4R)-4-((3R5S7S9S10S13R14S17R)-37-dihydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-22-dimethylpentanamido)ethane-1-sulfonic acid""' &
           Metabolite_name != '2-((4R)-4-((5R7R8R9S10S12S13R17R)-712-dihydroxy-1013-dimethyl-3-oxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanamido)ethane-1-sulfonic acid\"\"' &
           Metabolite_name != '(R)-4-((8S9S10R13R14S17R)-1013-dimethyl-3-oxo-2367891011121314151617-tetradecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"'&
           Metabolite_name != '(R)-4-((8S9S10R13R14S17R)-1013-dimethyl-3-oxo-23891011121314151617-dodecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"' &
           Metabolite_name != 'methyl (4R)-4-((5R8S9S10R13R17R)-1013-dimethyl-36-dioxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoate\"\"' &
           Metabolite_name != '3-(dimethyl(3-((4R)-4-((3R,7R,12S,17R)-3,7,12-trihydroxy-10,13-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanamido)propyl)ammonio)-2-hydroxypropane-1-sulfonate' &
           Metabolite_name != '(4R)-N-methyl-N-((2S3R4R5R)-23456-pentahydroxyhexyl)-4-((3R5S7R9S10S12S13R14S17R)-3712-trihydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanamide\"\"' &
           Metabolite_name != '(2S,3R,5R,10R,13R,14S,17S)-2,3,14-trihydroxy-10,13-dimethyl-17-[(2R,3R,5R)-2,3,6-trihydroxy-5,6-dimethylheptan-2-yl]-2,3,4,5,9,11,12,15,16,17-decahydro-1H-cyclopenta[a]phenanthren-6-one' &
           Metabolite_name != '(R)-4-((5R8R9S10S13R14S17R)-1013-dimethyl-3-oxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"' &
           Metabolite_name != '(3R6R)-6-((3R5R7R8R9S10S12S13R14S17R)-37-diacetoxy-12-hydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)heptane-13-diyl diacetate\"\"' &
           Metabolite_name != '(2S,3R,5R,10R,13R,14S,17S)-2,3,14-trihydroxy-10,13-dimethyl-17-[(2R,3R)-2,3,6-trihydroxy-6-methylheptan-2-yl]-2,3,4,5,9,11,12,15,16,17-decahydro-1H-cyclopenta[a]phenanthren-6-one' &
           Metabolite_name != '(3R)-3-((3R5S7R9S10S12S13R14S17R)-3712-trihydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)butanoic acid\"\"' &
           Metabolite_name != '(R)-4-((3S5S8R9S10S13R14S17R)-3-hydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"' &
           Metabolite_name != '(R)-4-((1R3S5S7R8S9S10S12S13R14S17R)-13712-tetrahydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"') %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'LITHOCHOL-11-ENIC ACID_major',
    'Lithochol-11-enic acid', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'GLYCOCHENODEOXYCHOLATE',
    'Glycohenodeoxycholate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Glutathione (reduced)',
    'Glutathione', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Isoleucine (Not validated)',
    'Isoleucine', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'GLYCINE',
    'Glycine', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'glycolithocholic acid',
    'Glycolithocholic acid', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == '11BETA,17,21-TRIHYDROXYPREGN-4-ENE-3,20-DIONE 17-BUTYRATE',
    'Hydrocortisone 17-butyrate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'CORTICOSTERONE',
    'Corticosterone', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'L-GLUTAMINE',
    'L-Glutamine', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == '11BETA,17,21-TRIHYDROXYPREGN-4-ENE-3,20-DIONE 17-BUTYRATE',
    'Hydrocortisone 17-butyrate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'GLYCOCHENODEOXYCHOLATE',
    'Glycohenodeoxycholate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'glycolithocholic acid',
    'Glycolithocholic acid', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Glutamylphenylalanine',
    'Gln-Phe', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Phenylalanylvaline',
    'Phe-Val', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Glutamyltyrosine',
    'Gln-Tyr', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Leucylproline',
    'Leu-Pro', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Prolylproline',
    'Pro-Pro', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Glycyl-L-proline',
    'Gly-Pro', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Glutamine', 'Gln', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Phenylalanine', 'Phe', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Proline', 'Pro', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Serine', 'Ser', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Aspartic acid', 'Asp', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Isoleucine', 'Ile', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Glutamic acid', 'Glu', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Glycine', 'Gly', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Tryptophan conjugated cholic acid', 'Trp-CA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Taurodeoxycholic acid', 'TDCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Lysine conjugated deoxycholic acid putative', 'Lys-DCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Histidine conjugated cholic acid', 'His-CA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Glycolithocholic acid', 'GLCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Glycohenodeoxycholate', 'GCDCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Glycocholic Acid', 'GCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Dehydrotumulosic acid', 'DTA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Cysteine conjugated chenodeoxycholic acid', 'Cys-CDCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Chenodeoxycholic acid', 'CDCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Arginine conjugated chenodeoxycholic acid', 'Arg-CDCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == '3Beta-Hydroxy-23,24-Bisnorchol-5-Enic Acid', '3\u03B2-OH-23,24-Bisnorchol-5-Enic Acid', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Deoxycholic acid', 'DCA', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == 'Polyporenic acid C', 'PAC', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(Metabolite_name == '2-O-Acetyl-20-hydroxyecdysone', '2-O-Ac-20HE', Metabolite_name))

#write.csv(boxplot_endo_data,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figure_Data_Sets/Boxplot_endo_data.csv", row.names=FALSE) #export final data frame#

nAA <- boxplot_endo_data %>% filter(Subclass == 'Amino acids, peptides, and analogues') %>% group_by(Metabolite_name) %>% slice(1) %>% nrow()
nbile <- boxplot_endo_data %>% filter(Subclass == 'Bile acids, alcohols and derivatives') %>% group_by(Metabolite_name) %>% slice(1) %>% nrow()

boxplot_endo_classes <- boxplot_endo_data%>%
  ggplot(aes(y = reorder(Metabolite_name, log_notnorm_area, FUN = median), x = log_notnorm_area, fill = Superclass))+
  theme_classic()+
  geom_boxplot(fill = '#FBB4AE', alpha = 1, outliers = TRUE, width=0.6, outlier.size=2)+
  facet_wrap(~Subclass, dir = "v", strip.position = "top", scales = 'free_y',
                    axes = "all", nrow = 2)+  #split plots#
  theme(strip.background = element_rect(colour = "black", fill = "white"))+
  labs(x = expression('log'[10]* ' Area'), y = ' ', fill = 'Compound')+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size = 37), axis.title.y = element_text(size = 30), axis.text.x = element_text(size = 30), axis.text.y = element_text(size = 30), legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),title = element_text(size = 15), strip.text = element_text(size = 37))  +
  #scale_fill_brewer(palette = 'Set2')+
  force_panelsizes(rows = c(nAA,nbile))+
  scale_x_continuous(limits = c(-6,4))+
  guides(fill="none", colour = 'none') #remove legend
boxplot_endo_classes
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/Boxplot_endogenous.svg", plot = boxplot_endo_classes, base_width=15, base_height=14)
box1n = nFluroides + nOPEs
box2n = nAA + nbile

plot_grid(boxplot_exo_classes, boxplot_endo_classes, nrow = 2, align = "v", axis = "l",
          rel_heights = c(box1n, box2n))

CL_Classification_df_combined_filtered_endo2 <- CL_Classification_df_combined_filtered_Classmerged %>%
  filter(Subclass == 'Hydroxysteroids' | Subclass == 'Bile acids, alcohols and derivatives'| Subclass == 'Ceramides') %>%
  pivot_longer(cols = starts_with('P1'), names_to = 'Sample', values_to = 'log_norm_area') %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2'
  separate_wider_delim(Metabolite_name, "; P", names = c("Metabolite_name", "Discard2"), too_few = c("align_start")) %>% #use align start to address things without deliminator#
  separate_wider_delim(Metabolite_name, "!", names = c("Discard3", "Metabolite_name"), too_few = c("align_end")) %>%
  separate_wider_delim(Metabolite_name, "; C", names = c("Metabolite_name", "Discard4"), too_few = c("align_start")) %>%
  separate_wider_delim(Metabolite_name, "; L", names = c("Metabolite_name", "Discard5"), too_few = c("align_start"))
unique(CL_Classification_df_combined_filtered_endo2$SMILES)

unique(CL_Classification_df_combined_filtered_endo2$Metabolite_name)

boxplot_endo_classes2 <- CL_Classification_df_combined_filtered_endo2 %>%  #data frame#
  filter(Metabolite_name != '2,3,14-trihydroxy-10,13-dimethyl-17-(2,3,6,7-tetrahydroxy-5,6-dimethylheptan-2-yl)-2,3,4,5,9,11,12,15,16,17-decahydro-1H-cyclopenta[a]phenanthren-6-one'&
           Metabolite_name != '(R)-4-((3R,5R,8R,9S,10S,12S,13R,14S,17R)-3,12-dihydroxy-10,13-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-(pyridin-2-ylmethyl)pentanamide' &
           Metabolite_name != '2-((4R)-4-((3R5R9S10S13R14S17R)-3-hydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-methylpentanamido)ethane-1-sulfonic acid\"\"' &
           Metabolite_name != '(3R7R8R9S10S12S13R14S17R)-17-((2R5R)-57-dihydroxyheptan-2-yl)-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthrene-3712-triol\"\"' &
           Metabolite_name != 'ethyl (R)-4-((5R7R8R9S10S13R14S17R)-7-hydroxy-1013-dimethyl-312-dioxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoate\"\"' &
           Metabolite_name != '2-((4R)-4-((3R5S7S9S10S13R14S17R)-37-dihydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-22-dimethylpentanamido)ethane-1-sulfonic acid""' &
           Metabolite_name != '2-((4R)-4-((5R7R8R9S10S12S13R17R)-712-dihydroxy-1013-dimethyl-3-oxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanamido)ethane-1-sulfonic acid\"\"' &
           Metabolite_name != '(R)-4-((8S9S10R13R14S17R)-1013-dimethyl-3-oxo-2367891011121314151617-tetradecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"' &
           Metabolite_name != 'NCGC00385132-01_C23H28O7_1-Phenanthrenecarboxylic acid, 10-(acetyloxy)-1,2,4a,9,10,10a-hexahydro-5,9-dihydroxy-1,4a-dimethyl-7-(1-methylethyl)-2-oxo-, methyl ester, (1R,4aS,9S,10S)-' &
           Metabolite_name != 'methyl 2,4,10-triacetyloxy-5,9-dihydroxy-1,4a-dimethyl-7-propan-2-yl-2,3,4,9,10,10a-hexahydrophenanthrene-1-carboxylate') %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == '11BETA,17,21-TRIHYDROXYPREGN-4-ENE-3,20-DIONE 17-BUTYRATE',
    'Hydrocortisone 17-butyrate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'CORTICOSTERONE',
    'Corticosterone', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == '11BETA,17,21-TRIHYDROXYPREGN-4-ENE-3,20-DIONE 17-BUTYRATE',
    'Hydrocortisone 17-butyrate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'GLYCOCHENODEOXYCHOLATE',
    'Glycohenodeoxycholate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Cysteine conjugated chenodeoxycholic acid',
    'Cysteine conjugated\nchenodeoxycholic acid', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'glycolithocholic acid',
    'Glycolithocholic acid', Metabolite_name)) %>%
  ggplot(aes(y = Metabolite_name, x = log_norm_area))+
  theme_classic()+
  geom_boxplot(fill = '#FBB4AE', alpha = 1, outliers = TRUE, width=0.5, outlier.size=2)+
  # stat_summary(fun=base::mean, colour="black", geom="point",shape='square', size=2, show.legend=FALSE,
  #            position = position_dodge2(width = 0.75, preserve = "single"))+ #show means#
  facet_nested_wrap(~Subclass, dir = "v", strip.position = "top",
                    axes = "all", remove_labels = "x", scales = 'free', nrow = 3)+  #split plots#
  theme(strip.background = element_rect(colour = "black", fill = "white"))+
  labs(x = 'log Area', y = 'Compound', fill = 'Compound')+
  theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),title = element_text(size = 15), strip.text = element_text(size = 30))  +
  theme(legend.position = "right")+
  force_panelsizes(rows = c(2,0.2,1.1))+
  #scale_fill_brewer(palette = 'Set2')+
  guides(fill="none", colour = 'none') #remove legend
boxplot_endo_classes2
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/boxplot_endo_classes2.pdf", plot = boxplot_endo_classes2, base_width=22, base_height=27)

CL_Classification_df_combined_filtered_endo3 <- CL_Classification_df_combined_filtered_Classmerged %>%
  filter(Subclass == 'Glycerophosphocholines' | Subclass == 'Bile acids, alcohols and derivatives'| Subclass == 'Ceramides') %>%
  pivot_longer(cols = starts_with('P1'), names_to = 'Sample', values_to = 'log_norm_area') %>%
  separate_wider_delim(Metabolite_name, "MS2: ", names = c("Discard", "Metabolite_name"), too_few = c('align_end')) %>% #trying to get rid of 'No MS2'
  separate_wider_delim(Metabolite_name, "; P", names = c("Metabolite_name", "Discard2"), too_few = c("align_start")) %>% #use align start to address things without deliminator#
  separate_wider_delim(Metabolite_name, "!", names = c("Discard3", "Metabolite_name"), too_few = c("align_end")) %>%
  separate_wider_delim(Metabolite_name, "; C", names = c("Metabolite_name", "Discard4"), too_few = c("align_start")) %>%
  separate_wider_delim(Metabolite_name, "; L", names = c("Metabolite_name", "Discard5"), too_few = c("align_start"))


boxplot_endo_classes3 <- CL_Classification_df_combined_filtered_endo3 %>%  #data frame#
  filter(Metabolite_name != '2,3,14-trihydroxy-10,13-dimethyl-17-(2,3,6,7-tetrahydroxy-5,6-dimethylheptan-2-yl)-2,3,4,5,9,11,12,15,16,17-decahydro-1H-cyclopenta[a]phenanthren-6-one'&
           Metabolite_name != '(R)-4-((3R,5R,8R,9S,10S,12S,13R,14S,17R)-3,12-dihydroxy-10,13-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-(pyridin-2-ylmethyl)pentanamide' &
           Metabolite_name != '2-((4R)-4-((3R5R9S10S13R14S17R)-3-hydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-N-methylpentanamido)ethane-1-sulfonic acid\"\"' &
          Metabolite_name != '(3R7R8R9S10S12S13R14S17R)-17-((2R5R)-57-dihydroxyheptan-2-yl)-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthrene-3712-triol\"\"' &
           Metabolite_name != 'ethyl (R)-4-((5R7R8R9S10S13R14S17R)-7-hydroxy-1013-dimethyl-312-dioxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoate\"\"' &
          Metabolite_name != '2-((4R)-4-((3R5S7S9S10S13R14S17R)-37-dihydroxy-1013-dimethylhexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)-22-dimethylpentanamido)ethane-1-sulfonic acid""' &
           Metabolite_name != '2-((4R)-4-((5R7R8R9S10S12S13R17R)-712-dihydroxy-1013-dimethyl-3-oxohexadecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanamido)ethane-1-sulfonic acid\"\"' &
           Metabolite_name != '(R)-4-((8S9S10R13R14S17R)-1013-dimethyl-3-oxo-2367891011121314151617-tetradecahydro-1H-cyclopenta[a]phenanthren-17-yl)pentanoic acid\"\"') %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'GLYCOCHENODEOXYCHOLATE',
    'Glycohenodeoxycholate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'GLYCOCHENODEOXYCHOLATE',
    'Glycohenodeoxycholate', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'Cysteine conjugated chenodeoxycholic acid',
    'Cysteine conjugated\nchenodeoxycholic acid', Metabolite_name)) %>%
  mutate(Metabolite_name = ifelse(
    Metabolite_name == 'glycolithocholic acid',
    'Glycolithocholic acid', Metabolite_name)) %>%
  ggplot(aes(y = Metabolite_name, x = log_norm_area))+
  theme_classic()+
  geom_boxplot(fill = '#FBB4AE', alpha = 1, outliers = TRUE, width=0.6, outlier.size=2)+
  # stat_summary(fun=base::mean, colour="black", geom="point",shape='square', size=2, show.legend=FALSE,
  #            position = position_dodge2(width = 0.75, preserve = "single"))+ #show means#
  facet_nested_wrap(~Subclass, dir = "v", strip.position = "top",
                    axes = "all", remove_labels = "x", scales = 'free', nrow = 3)+  #split plots#
  theme(strip.background = element_rect(colour = "black", fill = "white"))+
  labs(x = 'log Area', y = 'Compound', fill = 'Compound')+
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15), legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),title = element_text(size = 15), strip.text = element_text(size = 25))+
  #scale_fill_brewer(palette = 'Set2')+
  force_panelsizes(rows = c(2,0.2,2))+
  guides(fill="none", colour = 'none') #remove legend
boxplot_endo_classes3
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/boxplot_endo_classes3.pdf", plot = boxplot_endo_classes3, base_width=18, base_height=27)

boxplot_grid <- ggarrange(boxplot_exo_classes, boxplot_endo_classes, nrow = 2, labels = c('A', 'B'), font.label=list(color="black",size=35), align = c('v'))
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/boxplot_grid.svg", plot = boxplot_grid, base_width=20, base_height=30)

#####number of features#####
total_features <- CLEAN_MERGED_ALL_WITH_DUPS1_LOD_Corrected %>% nrow()
mz1 <- mz1_all %>% nrow()

test <- mz1_all %>% filter(is.na(INCHIKEY) | is.na(Metabolite_name) | is.na(SMILES) | Metabolite_name == 'null' | 
                             Metabolite_name == 'Unknown'| INCHIKEY == 'null' | SMILES == 'null') %>%
  nrow()

difference1 = total_features-mz1
dups_removed <- nrow(mz1_all_gclc)
mz1_msdial <- mz1_all_gclc%>% filter(Type != 'EI') %>% nrow()
mz1_CD <- mz1_all_gclc%>% filter(Type == 'EI') %>% nrow()

difference2 = mz1-dups_removed
blanks <- Participants_Cleaned %>%
  subset(select = c('Sample', 'Metabolite_name', 'lognormalized_area_to_creatine')) %>% #subset relevant columns. Sample_Area is the blank subtracted sample log transformed#
  pivot_wider(names_from = Sample, values_from = lognormalized_area_to_creatine) %>%
  nrow()

blanks2_and_detection <- heatmap_features %>% nrow()

detection <- heatmap_features %>% nrow()

#####new scatterplot#####

CL_Classification_df_combinedwide2 <- CL_Classification_df_combinedwide %>%
  dplyr::rename(INCHIKEY = InChIKey)

Classyfire_venn <- read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Physical-Chemical Properties/Classyfire_venn.csv", col_types = cols(.default = "c")) 

CL_Classification_df_combinedwide2 <- CL_Classification_df_combinedwide %>% dplyr::rename(INCHIKEY = InChIKey)

Classyfire_merge_venn_missing <- merge(heatmap_features, CL_Classification_df_combinedwide2, by = c('INCHIKEY'), all = T)%>%
  filter(!is.na(Metabolite_name)) %>%
  filter(is.na(ClassyFy_Status))

Classyfire_merge_venn <- merge(heatmap_features, CL_Classification_df_combinedwide2, by = c('INCHIKEY'))%>%
  filter(!is.na(ClassyFy_Status)) %>%
  filter(!is.na(Superclass)) %>%
  mutate(Type = ifelse(Type == 'C18_Positive', 'C18-ESI\u207A', Type))%>%
  mutate(Type = ifelse(Type == 'C18_Negative', 'C18-ESI\u207B', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Positive', 'HILIC-ESI\u207A', Type)) %>%
  mutate(Type = ifelse(Type == 'Hilic_Negative', 'HILIC-ESI\u207B', Type))

fills2 <- c(sapply(c("Set1"),
                   function(x) brewer.pal(9, x)[c(1:5, 7, 9, 8)])) #combine set 1 and set 2 into a palette so there's enough colours
fills4 <- c(sapply(c("Set2"),
                   function(x) brewer.pal(9, x)[c(1,6,3,4,2,5,7)])) #combine set 1 and set 2 into a palette so there's enough colours
fills5 <- c(sapply(c("Dark2"),
                   function(x) brewer.pal(5, x)[c(1:5)])) #combine set 1 and set 2 into a palette so there's enough colours

fills_combined2 <- c(fills2, fills4, fills5)

Start <- which(colnames(Participants_Blank_Correct_Wide)=="P12_D2_001")
End <- which(colnames(Participants_Blank_Correct_Wide)=="P16_D3_001") #adjust this wuth appropriate last column

Classyfire_merge_venn[, "Participant_Avg"] <- apply(Classyfire_merge_venn[,Start:End], 1, mean, na.rm = TRUE)

fold_change_data2_labels <- Classyfire_merge_venn %>%
  group_by(Superclass) %>%
  filter(n()>10) %>%
  dplyr::summarise(Ty = n()) %>%
  mutate(Label = paste0(Superclass,' (n = ',Ty,')')) %>%
  arrange(Ty)
#change to decreasing order - same for boxplot#
Fold_change_new <-merge(Classyfire_merge_venn, fold_change_data2_labels, by = c('Superclass')) %>%
  group_by(Superclass) %>%
  filter(n()>10) %>%
  ungroup() %>%
  mutate(Label = factor(Label, levels=c("Lipids and lipid-like molecules (n = 712)", "Organoheterocyclic compounds (n = 288)",
                                        "Benzenoids (n = 197)", "Alkaloids and derivatives (n = 55)", 'Organic nitrogen compounds (n = 28)',
                                        'Lignans, neolignans and related compounds (n = 21)',
                                        "Organic acids and derivatives (n = 318)",
                                        "Phenylpropanoids and polyketides (n = 270)",
                                        "Organic oxygen compounds (n = 124)",
                                        'Nucleosides, nucleotides, and analogues (n = 31)',
                                        "Organohalogen compounds (n = 21)"
                                         ))) %>%
  ggplot(aes(x = Average_Mz, y = Participant_Avg, colour = Type, shape = Type))+
  geom_point(size = 8, alpha = 0.5, stroke=2)+
  theme_classic()+
  scale_colour_manual(values = fills_combined2)+
  facet_nested_wrap(~Label, dir = "v", strip.position = "top",
                    axes = "all", remove_labels = "x", ncol = 2)+
  labs(x = 'Average m/z', y = expression('Log'[10]*'Area'))+
  scale_shape_manual(values = c(6,16,15,17,18,6,7,8,9,10,11,12,13,14,2,21))+
  # scale_y_continuous(limits = c(0,0.001))+
  theme(plot.title = element_text(hjust = 0.5))+ #make plot title in center of plot#
  theme(axis.title.x = element_text(size = 63), axis.title.y = element_text(size = 63), axis.text.x = element_text(size = 50), #adjust font size#
        axis.text.y = element_text(size = 50), legend.title = element_text(size = 50), legend.text = element_text(size = 40),
        title = element_text(size = 25), strip.text = element_text(size = 37))+ #strip.text for plot labels#
  #theme(legend.position = c(0.77, 0.12))+ #move legend in plot
  theme(legend.position = 'bottom')+ #move legend in plot
  scale_x_continuous(n.breaks = 6)+
  guides(colour=guide_legend(ncol=2), shape = guide_legend(ncol =2)) +#change number of columns in legend
  theme(plot.margin=unit(c(0.5,2,0.5,0.5), 'cm'))

Fold_change_new

#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/Abundance.svg", plot = Fold_change_new, base_width=32, base_height=32)

Scatterplot_Data <- merge(Classyfire_merge_venn, fold_change_data2_labels, by = c('Superclass')) %>%
  group_by(Superclass) %>%
  filter(n()>10) %>%
  ungroup() %>%
  mutate(Label = factor(Label, levels=c("Lipids and lipid-like molecules (n = 712)", "Organoheterocyclic compounds (n = 287)",
                                        "Benzenoids (n = 197)", "Alkaloids and derivatives (n = 55)", 'Organic nitrogen compounds (n = 28)',
                                        'Lignans, neolignans and related compounds (n = 21)',
                                        "Organic acids and derivatives (n = 318)",
                                        "Phenylpropanoids and polyketides (n = 270)",
                                        "Organic oxygen compounds (n = 124)",
                                        'Nucleosides, nucleotides, and analogues (n = 31)',
                                        "Organohalogen compounds (n = 21)"
  )))


####heatmap volc grid#####

volc.heatmap.grid <- ggarrange(ggarrange(plot_FC_all, plot_FC_all_filtered,
                               labels = c('A', 'B'), font.label = list(color = 'black', size = 35), nrow = 1),
                               ggarrange(volc, Participants_heatmap_norm100$gtable,
                               labels = c('C', 'D'), font.label=list(color="black",size=35)),
                               ncol = 1, nrow = 2)
volc.heatmap.grid
#save_plot("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Figures_Feb27/volc.heatmap.grid2.svg", plot = volc.heatmap.grid, base_width=22, base_height=20)

###venn, fold diff grid####
ven3_noleg <- ggVennDiagram(ven_list, stroke_size = 0.5, set_size = 8,
                      label_size = 6, label_percent_digit = 2, label = 'percent')+
  #scale_fill_manual(values = c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3'))+
  #scale_fill_gradient(low = '#Fab000', high = '#619CFF', name = 'Count')+#
  scale_x_continuous(expand = expansion(mult = .2))+ #expand plot so titles aren't cut off#
  scale_fill_distiller(palette = "Reds", direction = 1, name = "Count")+
  theme(
    legend.title = element_text(size = 20), legend.text = element_text(size = 18),
    title = element_text(size = 25), strip.text = element_text(size = 18), legend.position = 'non')# Customize theme
  #scale_fill_gradient(low = '#FFFFF0', high = '#619CFF', name = 'Count')+#
  #scale_fill_brewer(palette = 'Set1')
ven3_noleg


######pubchem exposomics######
pubchem_database <- read_csv("/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Anya/Contact Lenses/PubChemLite_exposomics_20241227.csv", col_types = cols(.default = "c"))

database_merge<- merge(heatmap_features, pubchem_database, by = c('SMILES'))
#write_xlsx(database_merge,"/Users/anyaguo/Library/CloudStorage/OneDrive-McMasterUniversity/Results/Pubchem_Database_Merge.xlsx") #export final data frame#
view(database_merge)
