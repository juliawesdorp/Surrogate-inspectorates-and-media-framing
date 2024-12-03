# ==== 1. INTRODUCTION =====
rm(list = ls())

# Used packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(car)
library(DescTools)
library(fmsb)
#Import dataset via Environment.

# ==== 2. DESCRIPTIVES =====
# Combine all variables calculating surrogates
dataset <- dataset %>%
  mutate(surrogate = pmax(Q12_Internationaal, Q12_Rijk, Q12_Provincie, Q12_Gemeente, Q12_Waterschap, 
                          Q12_Toezichthouder, Q12_Politiek, Q12_Advocaat, Q12_Privaat, Q12_Burger, 
                          Q12_Onderzoeksinstituut, Q12_Media, Q12_BNer, Q12_Stichting, Q12_Belangenvereniging, 
                          Q12_Overig, na.rm = TRUE))

dataset["Q9_Conflict"][dataset["Q9_Conflict"] == 1] <- 0
dataset["Q9_Conflict"][dataset["Q9_Conflict"] == 2] <- 1
dataset["Q10_Sensatie"][dataset["Q10_Sensatie"] == 1] <- 0
dataset["Q10_Sensatie"][dataset["Q10_Sensatie"] == 2] <- 1
dataset$binary_personalization <- ifelse(dataset$Q8_Person>1,1,0)
dataset$binary_toon <- ifelse(dataset$Q7_Toon>1,0,1)

# ==== 3. ANALYSES =====
# ==== 3.1 Binary logistic regression - creating models=====
# Creating the models
# Conflict
log_model_conflict_control <- glm(Q9_Conflict ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_conflict <- glm(Q9_Conflict ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
summary(log_model_conflict)
summary(log_model_conflict_control)

# Sensation
log_model_sensatie <- glm(Q10_Sensatie ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_sensatie_control <- glm(Q10_Sensatie ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
summary(log_model_sensatie)

# Personalization
log_model_pers <- glm(binary_personalization ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_pers_control <- glm(binary_personalization ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
summary(log_model_pers)
summary(log_model_pers_control)

# Valence
log_model_toon <- glm(binary_toon ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_toon_control <- glm(binary_toon ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial()) 
summary(log_model_toon)

# ==== 3.2 Binary logistic regression - analyzing models =====
# CONFLICT
## Calculate Chi squared
summary(log_model_conflict)
modelChi_conflict <- log_model_conflict$null.deviance - log_model_conflict$deviance
chidf_conflict <- log_model_conflict$df.null - log_model_conflict$df.residual
chisq.prob_conflict <- 1 - pchisq(modelChi_conflict, chidf_conflict)


## Confidence interval and odds ratio
exp(log_model_conflict$coefficients)
exp(confint(log_model_conflict))

# PERSONALIZATION
## Calculate Chi squared
summary(log_model_pers)
modelChi_p <- log_model_pers$null.deviance - log_model_pers$deviance
chidf_p <- log_model_pers$df.null - log_model_pers$df.residual
chisq.prob_p <- 1 - pchisq(modelChi_p, chidf_p)

## Confidence interval and odds ratio
exp(log_model_pers$coefficients)
exp(confint(log_model_pers))

#SENSATION
## Calculate Chi squared
summary(log_model_sensatie)
modelChi_s <- log_model_sensatie$null.deviance - log_model_sensatie$deviance
chidf_s <- log_model_sensatie$df.null - log_model_sensatie$df.residual
chisq.prob_s <- 1 - pchisq(modelChi_s, chidf_s)
chisq.prob_s

## Confidence interval and odds ratio
exp(log_model_sensatie$coefficients)
exp(confint(log_model_sensatie))

## Model Control - Sensation
model_difference_sensatie <- log_model_sensatie_control$deviance - log_model_sensatie$deviance
chidf_difference_sensatie <- log_model_sensatie_control$df.residual - log_model_sensatie$df.residual
chisq.prob_difference_s <- 1 - pchisq(model_difference_sensatie, chidf_difference_sensatie)

# VALENCE
## Calculate Chi squared
summary(log_model_toon)
modelChi_t <- log_model_toon$null.deviance - log_model_toon$deviance
chidf_t <- log_model_toon$df.null - log_model_toon$df.residual
chisq.prob_t <- 1 - pchisq(modelChi_t, chidf_t)

## Confidence interval and odds ratio
exp(log_model_toon$coefficients)
exp(confint(log_model_toon))

## Model Control - Valence
model_difference_toon <- log_model_toon_control$deviance - log_model_toon$deviance
chidf_difference_toon <- log_model_toon_control$df.residual - log_model_toon$df.residual
chisq.prob_difference_t <- 1 - pchisq(model_difference_toon, chidf_difference_toon)

# ==== 4. GRAPHS =====
## Graph - Valence over the years
valence_per_year <- dataset %>%
  group_by(year, Q7_Toon) %>%
  summarise(article_count = n())

filtered_data <- valence_per_year %>%
  filter(Q7_Toon == "1")

valence_years <- ggplot(filtered_data, aes(x=year, y=article_count)) +
  geom_line() +
  geom_smooth(method = "lm", col = "blue")+
    geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  labs(x ="Year", y = "Valence", title = "Negativity over the years") +
  scale_x_continuous(breaks = pretty(dataset$year, n = 8))+
  theme(
    axis.title.x = element_text(face="bold", size = 14),
    axis.title.y = element_text(face="bold", size = 14),
    axis.text = element_text(face="bold", size = 12)
  )


