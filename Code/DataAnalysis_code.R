#import library
library(tidyverse)  #load tidyverse package to memory
library(BSDA)       #load BDSA package to memory
library("readxl")


#import data
heat_data <- read_excel("E:/Documents/Probability&Statistics/HK222/Assignment/Data/heat_data.xlsx")
view(heat_data)
head(heat_data)

############################# Cleaning data ##############################

#Remote row with NA values
clean_heat_data <- heat_data[complete.cases(heat_data), ]
#Remove duplicated row
clean_heat_data <- clean_heat_data %>% distinct()
#Save clean_heat_data to a DF
clean_heat_data <- data.frame(clean_heat_data)

############################# Visualize data ##############################

#Descriptive stats for each variables
summary(clean_heat_data)

############################# Plot data ##############################

#X1: Relative Compactness
hist(clean_heat_data$X1, main = "Relative Compactness", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X1, main = "Relative Compactness", xlab = "Values", col = "red")

#X2: Surface Area
hist(clean_heat_data$X2, main = "Sureface Area", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X2, main = "Sureface Area", xlab = "Values", col = "red")

#X3: Wall Area
hist(clean_heat_data$X3, main = "Wall Area", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X3, main = "WallS Area", xlab = "Values", col = "red")

#X4: Roof Area
hist(clean_heat_data$X4, main = "Roof Area", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X4, main = "Roof Area", xlab = "Values", col = "red")

#X5: Overall height
hist(clean_heat_data$X5, main = "Overall Height", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X5, main = "Overall Height", xlab = "Values", col = "red")

#X6: Orientation
hist(clean_heat_data$X6, main = "Orientation", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X6, main = "Orientation", xlab = "Values", col = "red")

#X7: Glazing Area
hist(clean_heat_data$X7, main = "Glazing Area", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X7, main = "Glazing Area", xlab = "Values", col = "red")

#X8: Glazing Area Distribution
hist(clean_heat_data$X8, main = "Glazing Area Distribution", xlab = "Values", col = "blue")
boxplot(clean_heat_data$X8, main = "Glazing Area Distribution", xlab = "Values", col = "red")

#Y1: Heating Load
hist(clean_heat_data$Y1, main = "Heating Load", xlab = "Values", col = "blue")
boxplot(clean_heat_data$Y1, main = "Heating Load", xlab = "Values", col = "red")

#Y2: Cooling Load
hist(clean_heat_data$Y2, main = "Cooling Load", xlab = "Values", col = "blue")
boxplot(clean_heat_data$Y2, main = "Cooling Load", xlab = "Values", col = "red")