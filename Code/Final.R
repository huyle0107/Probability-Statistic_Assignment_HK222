################## Import Library ###################
library(gplots)     #install gplots library
library(dplyr)      #install dplyr library
library(tidyr)      #load tidyr package to memory
library(readr)
library(ggpubr)
library(car)
library(quantmod)   #the following 3 are used for ARIMA
library(tseries)
library(forecast)
library(tidyverse)  #the following 2 are used for multiple linear regression
library(ggcorrplot)

################# Import data ##################
Intel_CPUs <- read_csv("E:/Documents/Probability&Statistics/HK222/Assignment/Data/Intel_CPUs.csv")
view(Intel_CPUs)
head(Intel_CPUs)

################# Clean data ###################
# Delete all the columns that not relate to the project
# Choose 4 column that relate to this project
clear_NA_c4 <- subset(Intel_CPUs, select = c(2, 5, 6, 7, 8, 9, 10, 14))

# clear the row that have NA data
clear_NA_c4 <- subset(clear_NA_c4, clear_NA_c4$Recommended_Customer_Price != "N/A")
clear_NA_c4 <- na.omit(clear_NA_c4)

# Convert MHz to GHz
clear_NA_c4 <- separate(clear_NA_c4,col=Processor_Base_Frequency,into=c('Processor_Base_Frequency','CRUnit'),sep=" ")
clear_NA_c4$Processor_Base_Frequency <- as.numeric(gsub("[^0-9.-]", "", clear_NA_c4$Processor_Base_Frequency))
clear_NA_c4[grep("MHz",clear_NA_c4$CRUnit),]$Processor_Base_Frequency<-clear_NA_c4[grep("MHz",clear_NA_c4$CRUnit),]$Processor_Base_Frequency*0.001
clear_NA_c4 <- subset(clear_NA_c4,select=c(1,2,3,4,5,6,7,9))

# Convert all columns to numeric
clear_NA_c4$nb_of_Cores<-as.numeric(gsub("[^0-9.-]", "", clear_NA_c4$nb_of_Cores))
clear_NA_c4$nb_of_Threads<-as.numeric(gsub("[^0-9.-]", "", clear_NA_c4$nb_of_Threads))
clear_NA_c4$Lithography<-gsub(" nm","",clear_NA_c4$Lithography)
clear_NA_c4$Lithography<-as.numeric(gsub("[^0-9.-]", "", clear_NA_c4$Lithography))
clear_NA_c4$TDP<-gsub(" W","",clear_NA_c4$TDP)
clear_NA_c4$TDP<-as.numeric(gsub("[^0-9.-]", "", clear_NA_c4$TDP))
clear_NA_c4$Recommended_Customer_Price <- as.numeric(gsub("[^0-9.-]", "", trimws(gsub("-.*", "", clear_NA_c4$Recommended_Customer_Price))))

# Convert wrong format "Q1 " and "04" to right format
clear_NA_c4[grep("Q1 ",clear_NA_c4$Launch_Date),]$Launch_Date<-gsub("Q1 ","Q1",clear_NA_c4[grep("Q1 ",clear_NA_c4$Launch_Date),]$Launch_Date)
clear_NA_c4[grep("04",clear_NA_c4$Launch_Date),]$Launch_Date<-gsub("04","Q4",clear_NA_c4[grep("04",clear_NA_c4$Launch_Date),]$Launch_Date)
clear_NA_c4$Vertical_Segment<-as.factor(clear_NA_c4$Vertical_Segment)
clear_NA_c4$Launch_Date<-as.factor(clear_NA_c4$Launch_Date)

# Seperate col Launchdate to col Quarter and col year and change the strannge data to appropriate fromat
sep_Intel <- separate(clear_NA_c4, col = Launch_Date, into = c('Quarter', 'Year'), sep = "'")

# as.factor for 3 type of character
sep_Intel$Vertical_Segment <- as.factor(sep_Intel$Vertical_Segment)
sep_Intel$Quarter <- as.factor(sep_Intel$Quarter)
sep_Intel$Year <- as.factor(sep_Intel$Year)

# Summary the data
summary(sep_Intel)
head(sep_Intel)

########################### Comparision ############################
# one way ANOVA
#------------------------------------------------------#
#__________1. One way ANOVA for release price__________#
one_way_Price <- aov(Recommended_Customer_Price ~ sep_Intel$Vertical_Segment, data = sep_Intel)

# Boxplot
boxplot(sep_Intel$Recommended_Customer_Price ~ sep_Intel$Vertical_Segment, 
        main = "Release_Price by Manufacturer",
        xlab = "Manufacturer", ylab = "Release_Price", col = c("green", "red", "yellow"), las=1)
summary(one_way_Price)

# TukeyHSD test to check difference between pairs of groups
TukeyHSD(one_way_Price)

#__________2. One way ANOVA for processor__________#
one_way_Fre <- aov(sep_Intel$Processor_Base_Frequency ~ sep_Intel$Vertical_Segment, data = sep_Intel)

# Boxplot
boxplot(sep_Intel$Processor_Base_Frequency ~ sep_Intel$Vertical_Segment, main = "Performance by Manufacturer",
        xlab = "Manufacturer", ylab = "Performance", col = c("green", "red", "yellow"), las=1)
summary(one_way_Fre)

# TukeyHSD test to check difference between pairs of groups
TukeyHSD(one_way_Fre)

######################### Multiple Linear Regression #########################
# Prepare data; We will predict price based on factors: Clock rate, TDP, # cores, # threads, lithography
# Fit model, assuming that year does/ does not affect price
price_mlr_yes_year_model=lm(Recommended_Customer_Price~Processor_Base_Frequency+Lithography+nb_of_Threads+nb_of_Cores+TDP+Quarter+Year+Vertical_Segment,sep_Intel)

# Check & plot models residuals
# Using histogram
yes_year_residuals=price_mlr_yes_year_model$residuals
hist(yes_year_residuals)

# Using QQ plot
qqnorm(yes_year_residuals)
qqline(yes_year_residuals)

# Using Shapiro-Wilk test
shapiro.test(yes_year_residuals)

# Multi-collinearity assumption check
yes_year_data<-subset(sep_Intel, select = c(4,5,6,7,8,9))
corr_matrix_yes_year=round(cor(yes_year_data),2)
ggcorrplot(corr_matrix_yes_year, hc.order = TRUE, type = "lower",lab = TRUE)
summary(price_mlr_yes_year_model)
confint(price_mlr_yes_year_model)

# Check accuracy
sigma(price_mlr_yes_year_model)/mean(sep_Intel$Recommended_Customer_Price)

# Make prediction
pred_data<-data.frame(Processor_Base_Frequency=c(3.2),Lithography=c(14),nb_of_Threads=c(4),nb_of_Cores=c(8),TDP=c(130),Quarter=c("Q4"),Year=c("17"),Vertical_Segment=c("Desktop"))
predict(price_mlr_yes_year_model,pred_data)
pred_data_simple<-data.frame(Processor_Base_Frequency=c(3.2))

########################### ARIMA on Price ##############################
# Calculate Mean Price for each time stamp
arima_data<-subset(sep_Intel,select=c(2,3,5))
arima_data <- aggregate(arima_data$Recommended_Customer_Price, by=list(arima_data$Quarter,arima_data$Year), FUN=mean)

# Prepare data for ARIMA
plot(arima_data$x,type="l",lty=1,main="Original data")

# Test ADF
print(adf.test(arima_data$x))

# Fit model
AutoArimaModel=auto.arima(arima_data[1:30,]$x)
AutoArimaModel
forecastArima<-forecast(AutoArimaModel,5)
plot(forecastArima,main="Forecasted data")