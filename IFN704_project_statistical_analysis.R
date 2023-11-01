#IFN703 Vulnerability Statistic Analaysis
#t-test for significance
#Setup for analysis
#Load the library
library(readr)
library(GGally)
library(ggpubr)
library(tidyverse)
library(foreign)
library(nnet)
library(ggplot2)
library(DHARMa)
library(reshape2)
library(ROCR)
library(pROC)
library(readr)
#Read data
X10_data <- read_csv("~/Desktop/10%_data.csv")
balance_10900 <- read_csv("~/Desktop/10900_balance.csv")
imbalance <- read_csv("~/Desktop/smallimbalance.csv")

X10_data <- subset(X10_data, select = -1)
balance_10900 <- subset(balance_10900, select = -1)
imbalance <- subset(imbalance, select = -1)

#Set analyse group
#Group 1: precision
LineVul_Precision <- X10_data$`LineVul_10%_precision`
TFIDF_Precision <- X10_data$Token_TFIDF_precision
BoW_Precision <- X10_data$Token_BoW_precision
LineVul_Precision_10900 <- balance_10900$LineVul_precision
BoW_Precision_10900 <- balance_10900$BoW_precision
LineVul_Precision_imbalance <- imbalance$LineVul_precision
BoW_Precision_imbalance <- imbalance$BoW_precision
#Group 2: recall
LineVul_Recall <- X10_data$`LineVul_10%_recall`
TFIDF_Recall <- X10_data$Token_TFIDF_recall
BoW_Recall <- X10_data$Token_BoW_recall
LineVul_Recall_10900 <- balance_10900$LineVul_recall
BoW_Recall_10900 <- balance_10900$BoW_recall
LineVul_Recall_imbalance <- imbalance$LineVul_recall
BoW_Recall_imbalance <- imbalance$BoW_recall

#Group 3: f1-score
LineVul_F1 <- X10_data$`LineVul_10%_f1-score`
TFIDF_F1 <- X10_data$`Token_TFIDF_f1-score`
BoW_F1 <- X10_data$`Token_BoW_f1-score`
LineVul_F1_10900 <- balance_10900$`LineVul_f1-score`
BoW_F1_10900 <- balance_10900$`BoW_f1-score`
LineVul_F1_imbalance <- imbalance$`LineVul_f1-score`
Bow_F1_imbalance <- imbalance$`BoW_f1-score`


# Assuming you have two groups: LineVul_Precision, TFIDF_Precision, and BoW_Precision
precision_12 <- t.test(LineVul_Precision, TFIDF_Precision)
precision_13 <- t.test(LineVul_Precision, BoW_Precision)
precision_23 <- t.test(TFIDF_Precision, BoW_Precision)
precision_13_10900 <- t.test(LineVul_Precision_10900, BoW_Precision_10900)
precision_13_imbalance <- t.test(LineVul_Precision_imbalance, BoW_Precision_imbalance)

recall_12 <- t.test(LineVul_Recall, TFIDF_Recall)
recall_13 <- t.test(LineVul_Recall, BoW_Recall)
recall_23 <- t.test(TFIDF_Recall, BoW_Recall)
recall_13_10900 <- t.test(LineVul_Recall_10900, BoW_Recall_10900)
recall_13_imbalance <- t.test(LineVul_Recall_imbalance, BoW_Recall_imbalance)

f1_12 <- t.test(LineVul_F1,TFIDF_F1)
f1_13 <- t.test(LineVul_F1,BoW_F1)
f1_23 <- t.test(TFIDF_F1,BoW_F1)
f1_13_10900 <- t.test(LineVul_F1_10900, BoW_F1_10900)
f1_13_imbalance <- t.test(LineVul_F1_imbalance, Bow_F1_imbalance)

# Create a dataframe to store the t-test results
t_test_results <- data.frame(
  Comparison = c("Precision_LineVul vs. Precision_RF", "Recall_LineVul vs. Recall_BoW", "f1_LineVul vs. f1_BoW"),
  p_value = c(precision_13$p.value, recall_13$p.value, f1_13$p.value)
)
t_test_results_10900 <- data.frame(
  Comparison = c("10900_Precision_LineVul vs. Precision_RF", "10900_Recall_LineVul vs. Recall_BoW", "10900_f1_LineVul vs. f1_BoW"),
  p_value = c(precision_13_10900$p.value, recall_13_10900$p.value, f1_13_10900$p.value)
)
t_test_results_imbalance <- data.frame(
  Comparison = c("imbalance_Precision_LineVul vs. Precision_RF", "imbalance_Recall_LineVul vs. Recall_BoW", "imbalance_f1_LineVul vs. f1_BoW"),
  p_value = c(precision_13_imbalance$p.value, recall_13_imbalance$p.value, f1_13_imbalance$p.value)
)

# Create a significance column based on p-value
t_test_results$Significance <- ifelse(t_test_results$p_value < 0.05, "Significant", "Not Significant")
t_test_results_10900$Significance <- ifelse(t_test_results_10900$p_value < 0.05, "Significant", "Not Significant")
t_test_results_imbalance$Significance <- ifelse(t_test_results_imbalance$p_value < 0.05, "Significant", "Not Significant")

# Print the table
print(t_test_results)
print(t_test_results_10900)
print(t_test_results_imbalance)
f1_12 <- t.test(LineVul_F1,TFIDF_F1)
f1_13 <- t.test(LineVul_F1,BoW_F1)
f1_23 <- t.test(TFIDF_F1,BoW_F1)

print(f1_12)
print(f1_13)
print(f1_23)
