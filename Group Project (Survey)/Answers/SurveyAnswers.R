library(readr)

data <- read_csv("Group Project (Survey)/data.csv")
view(data)

#Performance Expectancy 1
PE1 <- data$`using Canva for school-related work helps ME complete assignments more quickly.`
length(PE1)
PE1 <- likert_to_numeric(PE1)

# Calculate mean and standard deviation
PE1_Mean <- mean(PE1, na.rm = TRUE)
PE1_SD <- sd(PE1, na.rm = TRUE)
PE1_Mean
PE1_SD

#Performance Expectancy 2
PE2 <- data$`using Canva to organize My schoolwork makes Me more productive.`
length(PE2)

PE2 <- likert_to_numeric(PE2)

# Calculate mean and standard deviation
PE2_Mean <- mean(PE2, na.rm = TRUE)
PE2_SD <- sd(PE2, na.rm = TRUE)
PE2_Mean
PE2_SD

#Performance Expectancy 3
PE3 <- data$`If I use canva for My school projects, It would increase My chances of receiving praise or succeeding academically.`
length(PE3)

PE3 <- likert_to_numeric(PE3)

# Calculate mean and standard deviation
PE3_Mean <- mean(PE3, na.rm = TRUE)
PE3_SD <- sd(PE3, na.rm = TRUE)
PE3_Mean
PE3_SD

merged_mean <- mean(c(PE1_Mean,PE2_Mean, PE3_Mean), na.rm = TRUE)

merged_sd <- sqrt(mean(c(PE1_SD^2,PE2_SD^2, PE3_SD^2), na.rm = TRUE))

print(merged_mean)
print(merged_sd)

summary_data <- data.frame(
  Code = c("PE1", "PE2", "PE3"),
  Mean = c(PE1_Mean, PE2_Mean, PE3_Mean),
  SD = c(PE1_SD, PE2_SD, PE3_SD)
)

# Save the data frame as a CSV file
write.csv(summary_data, "PE.csv", row.names = FALSE)


#FC 
#Social Influence
FC1 <- data$`I have the necessary tools to use Canva efficiently.`
length(FC1)

FC1_numeric <- likert_to_numeric(FC1)

FC1_Mean <- mean(FC1_numeric, na.rm = TRUE)
FC1_SD <- sd(FC1_numeric, na.rm = TRUE)
FC1_Mean
FC1_SD

FC2 <- data$`I am equipped with the necessary knowledge to use Canva effectively.`
length(FC2)

FC2_numeric <- likert_to_numeric(FC2)

FC2_Mean <- mean(FC2_numeric, na.rm = TRUE)
FC2_SD <- sd(FC2_numeric, na.rm = TRUE)
FC2_Mean
FC2_SD

FC3 <- data$`Using canva works well with other platforms that I frequently use.`
length(FC3)

FC3_numeric <- likert_to_numeric(FC3)

FC3_Mean <- mean(FC3_numeric, na.rm = TRUE)
FC3_SD <- sd(FC3_numeric, na.rm = TRUE)
FC3_Mean
FC3_SD

FC4 <- data$`there is a designated individual or team that can help me with any issues encountered on Canva.`
length(FC4)

FC4_numeric <- likert_to_numeric(FC4)

FC4_Mean <- mean(FC4_numeric, na.rm = TRUE)
FC4_SD <- sd(FC4_numeric, na.rm = TRUE)
FC4_Mean
FC4_SD

merged_mean <- mean(c(FC1_Mean, FC2_Mean, FC3_Mean, FC4_Mean), na.rm = TRUE)

merged_sd <- sqrt(mean(c(FC1_SD^2, FC2_SD^2, FC3_SD^2, FC4_SD^2), na.rm = TRUE))

print(merged_mean)
print(merged_sd)
summary_data2 <- data.frame(
  Code = c("FC1", "FC2", "FC3", "FC4"),
  Mean = c(FC1_Mean, FC2_Mean, FC3_Mean, FC4_Mean),
  SD = c(FC1_SD, FC2_SD, FC3_SD, FC4_SD)
)

# Save the data frame as a CSV file
write.csv(summary_data2, "FC.csv", row.names = FALSE)


#Effort Expectancy
EE1 <- data$`
I have a clear grasp of using Canva, understanding its functionality and how to effectively manage projects within the platform.`
length(EE1)

EE1_numeric <- likert_to_numeric(EE1)

EE1_Mean <- mean(EE1_numeric, na.rm = TRUE)
EE1_SD <- sd(EE1_numeric, na.rm = TRUE)
EE1_Mean
length(EE1_numeric)
EE1_SD

EE2 <- data$`It would be so easy for me to be skillful in using canva`
length(EE2)

EE2_numeric <- likert_to_numeric(EE2)

EE2_Mean <- mean(EE2_numeric, na.rm = TRUE)
EE2_SD <- sd(EE2_numeric, na.rm = TRUE)
EE2_Mean
length(EE2_numeric)
EE2_SD

EE3 <- data$`I find canva easy to use.`
length(EE3)

EE3_numeric <- likert_to_numeric(EE3)

EE3_Mean <- mean(EE3_numeric, na.rm = TRUE)
EE3_SD <- sd(EE3_numeric, na.rm = TRUE)
EE3_Mean
length(EE3_numeric)
EE3_SD

merged_mean <- mean(c(EE1_Mean, EE2_Mean, EE3_Mean), na.rm = TRUE)

merged_sd <- sqrt(mean(c(EE1_SD^2, EE2_SD^2, EE3_SD^2), na.rm = TRUE))

print(merged_mean)
print(merged_sd)


summary_data <- data.frame(
  Code = c("EE1", "EE2", "EE3"),
  Mean = c(EE1_Mean, EE2_Mean, EE3_Mean),
  SD = c(EE1_SD, EE2_SD, EE3_SD)
)

# Save the data frame as a CSV file
write.csv(summary_data, "EE.csv", row.names = FALSE)



#Social Influence
SI1 <- data$`Given that people can have a significant impact on how you behave, do you think students should utilize Canva?`
length(SI1)

SI1_numeric <- likert_to_numeric(SI1)

SI1_Mean <- mean(SI1_numeric, na.rm = TRUE)
SI1_SD <- sd(SI1_numeric, na.rm = TRUE)
SI1_Mean
length(SI1_numeric)
SI1_SD

SI2 <- data$`Would you advise students to use Canva, particularly if they have heard positive feedback from significant others?`
length(SI2)

SI2_numeric <- likert_to_numeric(SI2)

SI2_Mean <- mean(SI2_numeric, na.rm = TRUE)
SI2_SD <- sd(SI2_numeric, na.rm = TRUE)
SI2_Mean
length(EE2_numeric)
SI2_SD

SI3 <- data$`Is Your school usually in favor of students using Canva?`
length(SI3)

SI3_numeric <- likert_to_numeric(SI3)

SI3_Mean <- mean(SI3_numeric, na.rm = TRUE)
SI3_SD <- sd(SI3_numeric, na.rm = TRUE)
SI3_Mean
length(SI3_numeric)
SI3_SD

merged_mean <- mean(c(SI1_Mean, SI2_Mean, SI3_Mean), na.rm = TRUE)

merged_sd <- sqrt(mean(c(SI1_SD^2, SI2_SD^2, SI3_SD^2), na.rm = TRUE))

print(merged_mean)
print(merged_sd)


summary_data <- data.frame(
  Code = c("SI1", "SI2", "SI3"),
  Mean = c(SI1_Mean, SI2_Mean, SI3_Mean),
  SD = c(SI1_SD, SI2_SD, SI3_SD)
)

# Save the data frame as a CSV file
write.csv(summary_data, "SI.csv", row.names = FALSE)

