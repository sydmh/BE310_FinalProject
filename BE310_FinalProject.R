# BE 310 Final Project #

# import libraries
library("tidyverse")
library("ggplot2")
library("dplyr")
library("tibble")

# import data from csv file
mydata <- read.csv(file.choose(), header=TRUE, sep=',', row.names = NULL)

# select desired columns
num_AMF_data <- select(mydata, c(Ppumila_Population, TotalFluorescence:FaithPhylogeneticDiversity))

# print out cleaned up table
head(num_AMF_data)

# checking distribution
hist(num_AMF_data$FaithPhylogeneticDiversity) # y-value
hist(num_AMF_data$InitialSeedlingSize_leaves) # y-value
hist(num_AMF_data$TotalFluorescence) # y-value
hist(num_AMF_data$num_AMFspecies) # x-value

# graph 1: Fluorescence vs. # AMF species
num_AMF_data %>%
  ggplot(mapping = aes(x = num_AMFspecies, y = TotalFluorescence)) +
  geom_point(color = 'black') +
  labs(x = "Number of AMF Species Present", y = "Total Fluorescence") +
  stat_smooth(method = "lm", col = "red")

# graph 2: Initial Seedling Size vs. # AMF species
num_AMF_data %>%
  ggplot(mapping = aes(x = num_AMFspecies, y = InitialSeedlingSize_leaves)) +
  geom_point(color = 'black') +
  labs(x = "Number of AMF Species Present", y = "Initial Seedling Size (# of leaves)") +
  stat_smooth(method = "lm", col = "red")

# graph 3: Faith's Diversity vs. # AMF species
num_AMF_data %>%
  ggplot(mapping = aes(x = num_AMFspecies, y = FaithPhylogeneticDiversity)) +
  geom_point(color = 'black') +
  labs(x = "Number of AMF Species Present", y = "Faith's Phylogenetic Diversity Value") +
  stat_smooth(method = "lm", col = "red")

#linear regression
numAMF.FPD.lm <- lm(FaithPhylogeneticDiversity ~ num_AMFspecies, data = num_AMF_data)
lm_summary1 <- summary(numAMF.FPD.lm)
lm_summary1

# graph 4: Total Fluorescence vs Faith's Diversity
num_AMF_data %>%
  ggplot(mapping = aes(x = FaithPhylogeneticDiversity, y = TotalFluorescence)) +
  geom_point(color = 'black') +
  labs(x = "Faith's Phylogenetic Diversity Value", y = "Total Fluorescence") +
  stat_smooth(method = "lm", col = "red")

# linear regression for Total Fluorescence vs Faith's Diversity
tf.FPD.lm <- lm(TotalFluorescence ~ FaithPhylogeneticDiversity, data = num_AMF_data)
lm_summary2 <- summary(tf.FPD.lm)
lm_summary2
  
