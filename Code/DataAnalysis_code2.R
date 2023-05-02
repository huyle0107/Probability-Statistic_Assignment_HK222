############# Import library ###############
library(gplots) #install gplots library
library(dplyr)  #install dplyr library
library(readr)  #install readr
def.par <- par(no.readonly = TRUE)

df.data <- read.csv("E:/Documents/Probability&Statistics/HK222/Assignment/Data/SkillCraft1_Dataset.csv", header = TRUE)
names(df.data)

############## Data cleanning ##############
# Select a sample in the population to carry out the survey.
# Take columns 2 and 8 to do the survey (LeagueIndex, AssignToHotKeys).
new_df <- df.data[1600:2000, c(2,8)]

# Convert variables to the appropriate data type
is.numeric(new_df$AssignToHotkeys)
new_df$AssignToHotkeys <- as.numeric(new_df$AssignToHotkeys)
is.numeric(new_df$AssignToHotkeys)

# Convert LeagueIndex to factor
is.factor(new_df$LeagueIndex)
new_df$LeagueIndex <- as.factor(new_df$LeagueIndex)
is.factor(new_df$LeagueIndex)

# The result after converting
new_df$LeagueIndex
new_df$AssignToHotkeys

############## Calculate group average and plot (using call dplyr) ##############
new_df %>%                                          # Specify data frame
  group_by(LeagueIndex) %>%                         # Specify group indicator
  summarise_at(vars(AssignToHotkeys),               # Specify column
               list(AssignToHotkeys_mean = mean))

############## Plot the boxplot and compare the mean values between the groups ##############
# Boxplot
boxplot(AssignToHotkeys ~ LeagueIndex, main = "Total AssignToHotKeys by LeagueIndex", data = new_df, col = (2:8))

#Gplot
plotmeans(AssignToHotkeys ~ LeagueIndex, data = new_df)
