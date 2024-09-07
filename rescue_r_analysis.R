#install.packages("ggplot2")

library(ggplot2)
library(stringr)
library(dplyr)


datasetALL <- data.frame()
treatments<-c( 
  "240716_150_PAPi_1",
  "240716_150_PAPi_2",
  "240716_SCD1i_1",
  "240716_SCD1i_2.5_APA_1",
  "240716_SCD1i_2.5_APA_2",
  "240716_SCD1i_2.5_At_1",
  "240716_SCD1i_2.5_At_2",
  "240716_SCD1i_2.5_Pp_1",
  "240716_SCD1i_2.5_Pp_2",
  "240716_SCD1i_2",
  "240716_SCD1i_5_APA_1",
  "240716_SCD1i_5_APA_2",
  "240716_SCD1i_5_At_1",
  "240716_SCD1i_5_At_2",
  "240716_SCD1i_5_Pp_1",
  "240716_SCD1i_5_Pp_2",
  "240716_WT_1",
  "240716_WT_2",
  "240719_150_PAPi_1",
  "240719_150_PAPi_2",
  "240719_SCD1i_2.5_APA_1",
  "240719_SCD1i_2.5_APA_2",
  "240719_SCD1i_2.5_At_1",
  "240719_SCD1i_2.5_At_2",
  "240719_SCD1i_2.5_Pp_1",
  "240719_SCD1i_2.5_Pp_2",
  "240719_SCD1i_5_APA_1",
  "240719_SCD1i_5_APA_2",
  "240719_SCD1i_5_At_1",
  "240719_SCD1i_5_At_2",
  "240719_SCD1i_5_Pp_1",
  "240719_SCD1i_5_Pp_2",
  "240719_SCD1i_1",
  "240719_SCD1i_2",
  "240719_WT_1",
  "240719_WT_2",
  "240723_150_PAPi_1",
  "240723_150_PAPi_2",
  "240723_SCD1i_1",
  "240723_SCD1i_2.5_APA_1",
  "240723_SCD1i_2.5_APA_2",
  "240723_SCD1i_2.5_At_1",
  "240723_SCD1i_2.5_At_2",
  "240723_SCD1i_2.5_Pp_1",
  "240723_SCD1i_2.5_Pp_2",
  "240723_SCD1i_2",
  "240723_SCD1i_5_APA_1",
  "240723_SCD1i_5_APA_2",
  "240723_SCD1i_5_At_1",
  "240723_SCD1i_5_At_2",
  "240723_SCD1i_5_Pp_1",
  "240723_SCD1i_5_Pp_2",
  "240723_WT_1",
  "240723_WT_2")

#list_of_files<-list.files(path = choose.dir(), recursive = TRUE, full.name = TRUE, pattern = "chloro2.txt$")
#use alternative fixed path instead of the choose.dir
#path = "\\\\bbt78\\VidaliLabBBT78\\Jocelyn\\chloro_results"
path= "\\Users\\User\\OneDrive - Worcester Polytechnic Institute (wpi.edu)\\SummerResearch2024\\Leah-Arl8 project\\SCD\\(No subject)"

list_of_files<-list.files(path, recursive = TRUE, full.name = TRUE, pattern = "chloro2.txt$")
num_files <- length(list_of_files) # This is the total number of txt files in the directory

# Loop through each file and process
for(i in 1:num_files){
  # Read the data
  temp_data <- read.table(list_of_files[i])
  
  # Remove columns we won't be using
  temp_data <- temp_data[,-c(3:10)]
  
  # Extract date
  pos <- regexpr('/', list_of_files[i])
  temp_data$Date <- substr(list_of_files[i], pos+1, pos+6)
  
  # Extract RNAi treatment
  RNAi_treat_vect <- str_extract(list_of_files[i], paste(treatments, collapse = "|"))
  temp_data$RNAi_treat <- RNAi_treat_vect
  
  # Extract experiment type and remove _1 and _2 suffixes
  temp_data$Experiment_Type <- sub("_[12]$", "", sub("^[^_]*_", "", RNAi_treat_vect))
  
  
  # Append to the main dataset
  datasetALL <- rbind(datasetALL, temp_data)
}

# Create solidity scatterplot with position dodge
ggplot(datasetALL, aes(x = Experiment_Type, y = Area, color = Date)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Scatterplot of Area by RNAi Treatment", x = "Experiment Type", y = "Area")

# Create solidity scatterplot with position dodge
ggplot(datasetALL, aes(x = Experiment_Type, y = Solidity, color = Date)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Scatterplot of Solidity by RNAi Treatment", x = "Experiment Type", y = "Solidity")



# Create solidity scatterplot with position dodge
ggplot(datasetALL, aes(x = Experiment_Type, y = Area, color = Date)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Scatterplot of Area by RNAi Treatment", x = "Experiment Type", y = "Area")

# Create solidity scatterplot with position dodge
ggplot(datasetALL, aes(x = Experiment_Type, y = Solidity, color = Date)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Scatterplot of Solidity by RNAi Treatment", x = "Experiment Type", y = "Solidity")


#create box plots
ggplot(datasetALL, aes(x = Experiment_Type, y = Area, color = Date)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +  # Adding boxplots
  geom_point(position = position_dodge(width = 0.5)) +ylim(0,100000) +  # Adding scatter points
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Scatterplot and Boxplot of Area by Experiment Type", x = "Experiment Type", y = "Area")


ggplot(datasetALL, aes(x = Experiment_Type, y = Solidity, color = Date)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +  # Adding boxplots
  geom_point(position = position_dodge(width = 0.5))  +  # Adding scatter points
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Scatterplot and Boxplot of Solidity by Experiment Type", x = "Experiment Type", y = "Solidity")


