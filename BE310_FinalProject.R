# BE 310 Final Project #

# import libraries
library("tidyverse")
library("ggplot2")
library("dplyr")
library("tibble")

# import data from csv file
mydata <- read.table("c:/Users/sydmo/OneDrive/Desktop/FALL 21/BE 310/BE_310_CleanData_Field.csv", header=TRUE,
                     sep=',', row.names=NULL)

# select desired columns
num_AMF_data <- select(mydata, c(Ppumila_Population, TotalFluorescence:FaithPhylogeneticDiversity))

# print out cleaned up table
head(num_AMF_data)

# graph 1: Fluorescence vs. # AMF species
num_AMF_data %>%
  ggplot(mapping = aes(x = num_AMFspecies, y = TotalFluorescence)) +
  geom_point(color = 'black')

# graph 2: Initial Seedling Size vs. # AMF species
num_AMF_data %>%
  ggplot(mapping = aes(x = num_AMFspecies, y = InitialSeedlingSize_leaves)) +
  geom_point(color = 'black')

# checking distribution
hist(num_AMF_data$FaithPhylogeneticDiversity) # y-value
hist(num_AMF_data$InitialSeedlingSize_leaves) # y-value
hist(num_AMF_data$TotalFluorescence) # y-value
hist(num_AMF_data$num_AMFspecies) # x-value


# graph 3: Faith's Diversity vs. # AMF species
num_AMF_data %>%
  ggplot(mapping = aes(x = num_AMFspecies, y = FaithPhylogeneticDiversity)) +
  geom_point(color = 'black') +
  stat_smooth(method = "lm", col = "red")

#linear regression
numAMF.FPD.lm <- lm(FaithPhylogeneticDiversity ~ num_AMFspecies, data = num_AMF_data)
lm_summary <- summary(numAMF.FPD.lm)
lm_summary

# graph 4: Total Fluorescence vs Faith's Diversity
num_AMF_data %>%
  ggplot(mapping = aes(x = FaithPhylogeneticDiversity, y = Total Fluorescence)) +
  Geom_point(color = 'black) 

# linear regression for Total Fluorescence vs Faith's Diversity
  tf.FPD.lm <- lm(Total Fluorescence ~ FaithPhylogeneticDiversity, data = num_AMF_data)
  lm_summary2 <- summary(tf.FPD.lm)
  lm_summary2
  

