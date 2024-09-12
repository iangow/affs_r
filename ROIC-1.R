


install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

install.packages("openxlsx")
library("openxlsx")

install.packages("ggplot2")
library("ggplot2")


ROICData <- read.csv("C:/Users/mpinnuck/OneDrive - The University of Melbourne/New folder/My Documents/AusData/BenchData/RData/Tutorials/WeekFive_Ratios/Roicdata.csv")


# Getting subsample. Removing observation that do not meet criteria. 

Benchdata <- ROICData %>%
  filter(fyear>1999) %>% 
  filter(roic>-3 & roic <3 )

##Getting percentile rank by size (based on total assets = at) and forming size groups
Benchdata <- Benchdata%>% group_by(fyear) %>%
  mutate(size_group = ntile(at,4),
         size_decile = ntile(at,10),
         large = ifelse (size_decile > 8, 1,0))%>% ungroup()

##Producing descriptive stats
ROICSummary <-Benchdata  %>% filter(size_group ==4)%>% group_by(gsector) %>%
  summarise(count = n(),mean = mean(roic ,na.rm = TRUE ), median =median(roic , na.rm = TRUE),
            min = min(roic ,na.rm = TRUE ),max = max(roic , na.rm = TRUE),
            quant25 = quantile(roic , probs = 0.25,na.rm = TRUE ), quant75 = quantile(roic , probs = 0.75, na.rm = TRUE), sd = sd(roic, na.rm = TRUE), p10 = quantile(roic, probs = 0.10,na.rm = TRUE  ), p90 = quantile(roic, probs = 0.90,na.rm = TRUE  )  )



#to export output to excel
setwd("C:\\Users\\mpinnuck\\OneDrive - The University of Melbourne\\New folder\\My Documents\\AusData\\BenchData")
write.xlsx(ROICSummary, "ROICSummary")







