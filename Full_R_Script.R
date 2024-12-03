# ==== 1. INTRODUCTION =====
rm(list = ls())

# Numbers and the regulatory agencies
## 1 = NVWA
## 2 = ILT
## 3 = ACM
## 4 = SodM
## 5 = DCMR
## 8 = AP
## Number 6 and 7 were not used in this study.

# Used packages (install them when necessary)
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

table(dataset$surrogate)

dataset["Q9_Conflict"][dataset["Q9_Conflict"] == 1] <- 0
dataset["Q9_Conflict"][dataset["Q9_Conflict"] == 2] <- 1

dataset["Q10_Sensatie"][dataset["Q10_Sensatie"] == 1] <- 0
dataset["Q10_Sensatie"][dataset["Q10_Sensatie"] == 2] <- 1

# ==== 3. ANALYSES =====
# ==== 3.1 Binary logistic regression - creating models=====
# Creating the models
# Conflict
table(dataset$Q9_Conflict)
log_model_conflict_control <- glm(Q9_Conflict ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_conflict <- glm(Q9_Conflict ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
summary(log_model_conflict)
summary(log_model_conflict_control)

# Sensation
table(dataset$Q10_Sensatie)

log_model_sensatie <- glm(Q10_Sensatie ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_sensatie_control <- glm(Q10_Sensatie ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
summary(log_model_sensatie)

# Personalization
table(dataset$binary_personalization)

log_model_pers <- glm(binary_personalization ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_pers_control <- glm(binary_personalization ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
summary(log_model_pers)
summary(log_model_pers_control)

# Valence
table(dataset$binary_toon)
table(dataset$Q7_Toon)
log_model_toon <- glm(binary_toon ~ surrogate + Q3_Inspectie + Q1_Medium, data = dataset, family = binomial())
log_model_toon_control <- glm(binary_toon ~ Q3_Inspectie + Q1_Medium, data = dataset, family = binomial()) 
summary(log_model_toon)

# ==== 3.2 Binary logistic regression - analyzing models =====
# CONFLICT
## Calculate Chi squared
summary(log_model_conflict)
modelChi_conflict <- log_model_conflict$null.deviance - log_model_conflict$deviance
modelChi_conflict
chidf_conflict <- log_model_conflict$df.null - log_model_conflict$df.residual
chidf_conflict
chisq.prob_conflict <- 1 - pchisq(modelChi_conflict, chidf_conflict)
chisq.prob_conflict

## Confidence interval and odds ratio
exp(log_model_conflict$coefficients)
exp(confint(log_model_conflict))

# PERSONALIZATION
## Calculate Chi squared
summary(log_model_pers)
modelChi_p <- log_model_pers$null.deviance - log_model_pers$deviance
modelChi_p
chidf_p <- log_model_pers$df.null - log_model_pers$df.residual
chidf_p
chisq.prob_p <- 1 - pchisq(modelChi_p, chidf_p)
chisq.prob_p

## Confidence interval and odds ratio
exp(log_model_pers$coefficients)
exp(confint(log_model_pers))

#SENSATION
## Calculate Chi squared
summary(log_model_sensatie)
modelChi_s <- log_model_sensatie$null.deviance - log_model_sensatie$deviance
modelChi_s
chidf_s <- log_model_sensatie$df.null - log_model_sensatie$df.residual
chidf_s
chisq.prob_s <- 1 - pchisq(modelChi_s, chidf_s)
chisq.prob_s

## Confidence interval and odds ratio
exp(log_model_sensatie$coefficients)
exp(confint(log_model_sensatie))

## Model Control - Sensation
model_difference_sensatie <- log_model_sensatie_control$deviance - log_model_sensatie$deviance
chidf_difference_sensatie <- log_model_sensatie_control$df.residual - log_model_sensatie$df.residual
chisq.prob_difference_s <- 1 - pchisq(model_difference_sensatie, chidf_difference_sensatie)
model_difference_sensatie
chidf_difference_sensatie
chisq.prob_difference_s

# VALENCE
## Calculate Chi squared
summary(log_model_toon)
modelChi_t <- log_model_toon$null.deviance - log_model_toon$deviance
modelChi_t
chidf_t <- log_model_toon$df.null - log_model_toon$df.residual
chidf_t
chisq.prob_t <- 1 - pchisq(modelChi_t, chidf_t)
chisq.prob_t

## Confidence interval and odds ratio
exp(log_model_toon$coefficients)
exp(confint(log_model_toon))

## Model Control - Valence
model_difference_toon <- log_model_toon_control$deviance - log_model_toon$deviance
chidf_difference_toon <- log_model_toon_control$df.residual - log_model_toon$df.residual
chisq.prob_difference_t <- 1 - pchisq(model_difference_toon, chidf_difference_toon)
model_difference_toon
chidf_difference_toon
chisq.prob_difference_t

# ==== 3.3 Binary logistic regression - R2 =====
## McFadden
PseudoR2(log_model_conflict, which = "McFadden")
PseudoR2(log_model_pers, which = "McFadden")
PseudoR2(log_model_sensatie, which = "McFadden")
PseudoR2(log_model_toon, which = "McFadden")

## Cox, Snell
PseudoR2(log_model_conflict, which = "CoxSnell")
PseudoR2(log_model_pers, which = "CoxSnell")
PseudoR2(log_model_sensatie, which = "CoxSnell")
PseudoR2(log_model_toon, which = "CoxSnell")

## Nagelkerke
NagelkerkeR2(log_model_conflict)
NagelkerkeR2(log_model_sensatie)
NagelkerkeR2(log_model_pers)
NagelkerkeR2(log_model_toon)

# ==== 4. GRAPHS =====
## Graph - Years & amount of attention
dataset$date <- dmy(dataset$Q5_Datum)
dataset$year <- lubridate::year(dataset$date)
articles_per_year <- dataset %>%
  group_by(year, Q3_Inspectie) %>%
  summarise(article_count = n())

names_regulator <- c(
  "1" = "NVWA",
  "2" = "ILT",
  "3" = "ACM",
  "4" = "SodM",
  "5" = "DCMR",
  "6" = "AP")

colors <- c("red2", "royalblue1", "forestgreen", "orange", "deeppink1", "darkmagenta")
articles_per_year$Q3_Inspectie <- factor(articles_per_year$Q3_Inspectie)

time_regulators <- ggplot(articles_per_year, aes(x=year, y=article_count, color = Q3_Inspectie)) +
  geom_line() + 
  geom_point() +
  geom_smooth(method = "lm", col = "blue")+
  labs(x ="Year", y = "Number of articles", title = "Media attention over the years per regulator")+
  facet_wrap(~Q3_Inspectie, scales = "free_y", labeller = labeller(Q3_Inspectie = names_regulator))+
  scale_x_continuous(breaks = pretty(dataset$year, n = 8)) +
  scale_color_manual(values = colors)
time_regulators

## Graph - Valence over the years
valence_per_year <- dataset %>%
  group_by(year, Q7_Toon) %>%
  summarise(article_count = n())
valence_per_year

filtered_data <- valence_per_year %>%
  filter(Q7_Toon == "1")
filtered_data
table(dataset$year)
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

valence_years

## Graph - Percentages negativity  - because each year has different sample sizes
df_n <- data.frame(
  Year = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
  Percentage = c(45.7, 38.7, 49.4, 51.1, 45.2, 53.1, 46.1, 48.9, 47.1, 46.5, 50.8, 54.8, 54.8)
)

negativity_controlled <- ggplot(df_n, aes(x=Year, y = Percentage, group = 1)) + 
  geom_line( color="black") +
  geom_point (shape=21, color="black", fill="#69b3a2", size=4)+
  geom_smooth(method = "lm", col = "blue")+
  ylim(30, 70)+
  ggtitle("Percentage negativity per year")+
  theme(
    axis.title.x = element_text(face="bold", size = 14),
    axis.title.y = element_text(face="bold", size = 14),
    axis.text = element_text(face="bold", size = 12)
  )

negativity_controlled
