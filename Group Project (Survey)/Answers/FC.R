library(readr)

data <- read_csv("Group Project (Survey)/data.csv")
View(data)

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
