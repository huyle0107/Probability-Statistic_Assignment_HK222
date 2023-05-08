################## Import Library ###################
library(gplots)     #install gplots library
library(dplyr)      #install dplyr library
library(tidyr)      #load tidyr package to memory
library(readr)
library("ggpubr")
library("car")

################# Import data ##################
Intel_CPUs <- read_csv("C:/Users/hh.aof/Desktop/HCMUT/SOPHOMORE2/Statistics-Probability/Assignment/Data/GPU/Source/Intel_CPUs.csv")
View(Intel_CPUs)

################# Clean data ###################
# Delete all the columns that not relate to the project
# Choose 4 column that relate to this project
c4_Intel <- subset(Intel_CPUs, select = c(2, 5, 7, 10))

# clear the row that have NA data
clear_NA_c4 <- na.omit(c4_Intel)
clear_NA_c4 <- subset(clear_NA_c4, clear_NA_c4$Recommended_Customer_Price != "N/A")

# Beacuse there is less MHz value so I decide to delete 
rows_to_remove <- grep(" MHz", clear_NA_c4$Processor_Base_Frequency)
clear_NA_c4 <- clear_NA_c4[-rows_to_remove, ]

# Seperate col Launchdate to col Quarter and col year
sep_Intel <- separate(clear_NA_c4, col = Launch_Date, into = c('Quarter', 'Year'), sep = "'")

# Change the price to numeric
sep_Intel$Recommended_Customer_Price <- as.numeric(gsub("[^0-9.-]", "", trimws(gsub("-.*", "", sep_Intel$Recommended_Customer_Price))))

# Change the Processor Frequency to numeric
sep_Intel$Processor_Base_Frequency <- as.numeric(gsub("[^0-9.-]", "", sep_Intel$Processor_Base_Frequency))
View(sep_Intel)

# Summary the data
summary(sep_Intel)

########################### Comparision ############################
# one way ANOVA
#------------------------------------------------------#
#__________1. One way ANOVA for release price__________#
one_way_Price <- aov(Recommended_Customer_Price ~ sep_Intel$Vertical_Segment, data = sep_Intel)

# Levene's test to check the homogeneity of variance assumption
leveneTest(Recommended_Customer_Price ~ sep_Intel$Vertical_Segment, data = sep_Intel)

# Shapiro-Wilk test to check normality assumption
residual_price <- residuals(one_way_Price)
shapiro.test(residual_price)

# Boxplot
boxplot(sep_Intel$Recommended_Customer_Price ~ sep_Intel$Vertical_Segment, 
        main = "Release_Price by Manufacturer",
        xlab = "Manufacturer", ylab = "Release_Price", col = c("green", "red", "yellow"), las=1)
summary(one_way_Price)

# TukeyHSD test to check difference between pairs of groups
TukeyHSD(one_way_Price)

#__________2. One way ANOVA for processor__________#
one_way_Fre <- aov(sep_Intel$Processor_Base_Frequency ~ sep_Intel$Vertical_Segment, data = sep_Intel)

# Levene's test to check the homogeneity of variance assumption
leveneTest(sep_Intel$Processor_Base_Frequency ~ sep_Intel$Vertical_Segment, data = sep_Intel)

# Shapiro-Wilk test to check normality assumption
residual_Fre <- residuals(one_way_Fre)
shapiro.test(residual_Fre)

# Boxplot
boxplot(sep_Intel$Processor_Base_Frequency ~ sep_Intel$Vertical_Segment, main = "Performance by Manufacturer",
        xlab = "Manufacturer", ylab = "Performance", col = c("green", "red", "yellow"), las=1)
summary(one_way_Fre)

# TukeyHSD test to check difference between pairs of groups
TukeyHSD(one_way_Fre)

########################### Regression ############################
# Fiting the linear model
LinearModel <- lm(sep_Intel$Processor_Base_Frequency ~ sep_Intel$Year, data = sep_Intel)
summary(LinearModel)

# plot data
plot(LinearModel)
set.seed(123)
variable_Year <- data.frame(round(runif(1264, 18, 23),0))
Predict_Frequency = predict(LinearModel, newdata = variable_Year)
pred_freq_tab <- cbind(variable_Year, Predict_Frequency)
summary(pred_freq_tab)

########################### Prediction ############################
# Predict
# Creating a data frame
variable_Year <- data.frame(round(runif(1264, 23, 23),0))
variable_Per <- data.frame(round(runif(1264, 2.7, 2.7), 1))
# Fiting the linear model
LinearModel_1 <- lm(sep_Intel$Recommended_Customer_Price ~ sep_Intel$Processor_Base_Frequency, data = sep_Intel)

# Predicts the future values
Predict_Price = predict(LinearModel_1, newdata = variable_Per + variable_Year)

# add it to table
x_col <- variable_Year
y_col <- variable_Per
z_col <- Predict_Price
table_final <- cbind(x_col, y_col, z_col)
View(table_final)
summary(table_final)