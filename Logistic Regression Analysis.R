# ===============================
# 1. Load Required Libraries
# ===============================
library(readr)
library(dplyr)
library(ggplot2)
library(ResourceSelection)  # For Hosmer-Lemeshow test
library(pROC)               # For ROC/AUC analysis
library(broom)              # For tidy model output

# ===============================
# 2. Load the Dataset
# ===============================
df <- read_csv("C://Users//Admin//Desktop//Consumer//finalyearproject.csv")

# ===============================
# 3. Clean and Prepare Variables
# ===============================

# Trim and convert categorical variables
df$preference <- trimws(df$preference)
df$preference_binary <- ifelse(df$preference == "Yes", 1, 0)

 # Convert to factors
df$Gender <- factor(trimws(df$Gender))
df$Age <- factor(trimws(df$Age), levels = c("18-25", "26-35", "36-45", "46 and above"))
df$`Income level` <- factor(trimws(df$`Income level`))
df$`Education Level` <- factor(trimws(df$`Education Level`))

View(df$Age)
# Behavioral & attitudinal factors
df$Price <- factor(df$Price)
df$Convenience <- factor(df$Convenience)
df$`Trust/Security` <- factor(df$`Trust/Security`)
df$`Product Variety` <- factor(df$`Product Variety`)
df$`Customer service` <- factor(df$`Customer service`)

# ===============================
# 4. Fit Logistic Regression Model
# ===============================
model <- glm(
  preference_binary ~ Gender + Age + `Income level` + `Education Level` + 
    Price + Convenience + `Trust/Security` + `Product Variety` + `Customer service`,
  data = df,
  family = binomial
)

summary(model)  # View model summary
tidy(model, exponentiate = TRUE, conf.int = TRUE)  # Odds ratios

# ===============================
# 5. Model Evaluation
# ===============================

## 5.1 Hosmer-Lemeshow Test
# Prepare clean dataset for test
df_clean <- df %>%
  select(preference_binary, Gender, Age, `Income level`, `Education Level`, 
         Price, Convenience, `Trust/Security`, `Product Variety`, `Customer service`) %>%
  na.omit()

model_clean <- glm(
  preference_binary ~ .,
  data = df_clean,
  family = binomial
)

hoslem.test(df_clean$preference_binary, fitted(model_clean), g = 10)

## 5.2 ROC Curve and AUC
roc_result <- roc(df_clean$preference_binary, fitted(model_clean))
auc(roc_result)  # Print AUC value
plot(roc_result, main = "ROC Curve for Shopping Preference Model", col = "blue")
