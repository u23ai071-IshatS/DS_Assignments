# ------------------ Load libraries ------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(titanic)     # install.packages("titanic")
library(GGally)      # install.packages("GGally")

# ------------------ Load dataset --------------------
data("titanic_train")
df <- titanic_train
head(df)

# ------------------ Frequency table -----------------
freq_table <- table(df$Pclass)
rel_freq <- prop.table(freq_table) * 100
cum_freq <- cumsum(freq_table)

freq_df <- data.frame(
  Absolute = as.vector(freq_table),
  `Relative (%)` = as.vector(rel_freq),
  Cumulative = as.vector(cum_freq),
  row.names = names(freq_table)
)
print(freq_df)

# ------------------ Crosstab: Sex vs Survived -------
table <- as.data.frame.matrix(table(df$Sex, df$Survived))
table$Total <- rowSums(table)
table <- rbind(table, Total = colSums(table))
colnames(table) <- c("Survived = 0", "Survived = 1", "All")
rownames(table) <- c("Female", "Male", "All")
print(table)

# ------------------ Probabilities -------------------
total <- nrow(df)

# Joint probability: P(Sex=female, Survived=1)
joint_prob <- nrow(
  df %>% filter(Sex == "female", Survived == 1)
) / total
cat(
  "Joint Probability P(Sex=Female, Survived=1):",
  round(joint_prob, 4), "\n"
)

# Marginal probabilities
p_female <- nrow(df %>% filter(Sex == "female")) / total
p_survived <- nrow(df %>% filter(Survived == 1)) / total
cat("\nMarginal Probabilities:\n")
cat("P(Sex=Female):", round(p_female, 4), "\n")
cat("P(Survived=1):", round(p_survived, 4), "\n")

# Conditional probabilities
cat("\nConditional Probabilities:\n")

# P(Survived=1 | Sex=female)
cond1 <- joint_prob / p_female
cat("P(Survived=1 | Sex=female):", round(cond1, 4), "\n")
female_given_survive <- prop.table(table(df$Sex, df$Survived), 1)
print(female_given_survive)

# P(Sex=female | Survived=1)
cond2 <- joint_prob / p_survived
cat("P(Sex=female | Survived=1):", round(cond2, 4), "\n")
survive_given_female <- prop.table(table(df$Sex, df$Survived), 2)
print(survive_given_female)

# ------------------ Correlation ---------------------
df_clean <- df %>% select(Age, Fare) %>% drop_na()
corr <- cor(df_clean$Age, df_clean$Fare, method = "pearson")
cat("\nPearson Correlation:", round(corr, 4), "\n")

# ------------------ Heatmap -------------------------
corr_matrix <- cor(df_clean)
melted_corr <- as.data.frame(as.table(corr_matrix))

p1 <- ggplot(melted_corr, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme_minimal()
print(p1)

# ------------------ Pairplot ------------------------
p2 <- ggpairs(df_clean %>% select(Age, Fare))
print(p2)

# ------------------ Scatter plot --------------------
p3 <- ggplot(df_clean, aes(x = Age, y = Fare)) +
  geom_point(alpha = 0.6, color = "cyan") +
  labs(title = "Scatter Plot: Age vs Fare") +
  theme_minimal()
print(p3)

# ------------------ Bar plot: Survival by Class -----
class_survival <- table(df$Pclass, df$Survived)
barplot(
  class_survival,
  beside = FALSE, col = c("skyblue", "orange"),
  main = "Survival by Class", xlab = "Class", ylab = "Count",
  legend = rownames(class_survival)
)
