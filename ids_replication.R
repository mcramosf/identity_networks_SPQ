# Reproducibility Code --------------------------

# This script allows reproducing the results
# for Ramos, Maria C., & Nicolas Restrepo Ochoa (2024)
# "Identity Networks as a New Frontier: 
# Conceptualizing, Mapping, 
# and Analyzing Identity Structures 
# and Their Impact on Well-being." Social 
# Psychology Quarterly

library(tidyverse)

# Data Import -------------------------------
mydf <- read.csv("replication_dataset_ids_062024.csv")

# Descriptive Statistics --------------------------------------------------
# This section reproduces results in Table 1

summary(mydf)
map(mydf, sd)

# Correlations ------------------
# This section reproduces results in Table 2

library(psych)

names(mydf)
key_vars <- mydf %>% 
  select("interfering_density", 
          "facilitatory_density", 
          "interfering_avg_tie_strength", 
          "facilitatory_avg_tie_strength",
         "n_identities_claimed",
          "physical_health", 
         "happiness", 
         "life_satisfaction", 
         "depression", 
         "self_esteem", 
         "sense_of_mastery")

cor_keyvars <- round(cor(key_vars,
                          method = c("spearman")),2)

p_values <- round(corr.test(key_vars, use = "pairwise.complete.obs", method = "spearman")$p,4)

# combine correlation matrix and p-values into a single matrix
result <- matrix(NA, nrow = ncol(cor_keyvars), ncol = ncol(cor_keyvars))
result[upper.tri(result)] <- cor_keyvars[upper.tri(cor_keyvars)]
result[lower.tri(result)] <- p_values[lower.tri(p_values)]

# set column and row names
colnames(result) <- rownames(result) <- colnames(cor_keyvars)

result
result <- as.data.frame(result)

# Holm adjustment

p_values_holm <-  round(corr.test(key_vars, use = "pairwise.complete.obs", method = "spearman", 
                                  adjust = "holm")$p,4)


# Data Pre-processing for Modeling ----------------------------------

# Standardizing continuous variables
num_vars <- c("physical_health", 
              "happiness", 
              "life_satisfaction", 
              "depression",
              "self_esteem",
              "sense_of_mastery", 
              "interfering_density",
              "facilitatory_density",
              "interfering_avg_tie_strength", 
              "facilitatory_avg_tie_strength",
              "age", 
              "income", 
              "n_identities_claimed")

standardized_data <- mydf %>% 
  mutate_at(num_vars, scale)

summary(standardized_data)

# Testing Normality Assumptions  ----------------
library(lavaan)
library(MVN)

henze_zirkler_test <- mvn(mydf, 
                              mvnTest = "hz", 
                          multivariatePlot = "qq")

short_df <- mydf %>% 
  select("physical_health", 
         "happiness", 
         "life_satisfaction", 
         "depression",
         "self_esteem",
         "sense_of_mastery", 
         "interfering_density",
         "facilitatory_density",
         "interfering_avg_tie_strength", 
         "facilitatory_avg_tie_strength")

henze_zirkler_test2 <- mvn(short_df, 
                          mvnTest = "hz", 
                          multivariatePlot = "qq")

# CFA Models ---------------------------------
# This section reproduces results in Table 3

# This is CFA Model 1 in Table 3
modela <- 'well_being  =~ physical_health +
happiness + life_satisfaction + depression + 
self_esteem + sense_of_mastery'

modela_fit <- cfa(modela, data = standardized_data, 
                  estimator = "DWLS")
summary(modela_fit, fit.measures = T, 
        standardized = T)

# Model b: Model a but adding highest 
# correlation happinness and life-satisfaction
# This is CFA Model 2 in Table 3
modelb <- 'well_being  =~ physical_health +
happiness + life_satisfaction + depression +
self_esteem + sense_of_mastery

happiness ~~ life_satisfaction
'

modelb_fit <- cfa(modelb, data = standardized_data, 
                  estimator = "DWLS")
summary(modelb_fit, fit.measures = T, 
        standardized = T)


# Model c: Model b + correlations at the .77
# This is CFA Model 3 in Table 3
modelc <- 'well_being  =~ physical_health +
happiness + life_satisfaction + depression +
self_esteem + sense_of_mastery

happiness ~~ life_satisfaction
depression ~~ self_esteem
'

modelc_fit <- cfa(modelc, data = standardized_data, 
                  estimator = "DWLS")
summary(modelc_fit, fit.measures = T, 
        standardized = T)

# Model d: Model c + correlations at the .70 level
# This is CFA Model 4 in Table 3
modeld <- 'well_being  =~ physical_health +
happiness + life_satisfaction + depression +
self_esteem + sense_of_mastery

happiness ~~ life_satisfaction
depression ~~ self_esteem
sense_of_mastery ~~ depression 
sense_of_mastery ~~ self_esteem
'

modeld_fit <- cfa(modeld, data = standardized_data, 
                  estimator = "DWLS")
summary(modeld_fit, fit.measures = T, 
        standardized = T)


# Model comparisons

# lavTestLRT(modela_fit, modelb_fit)
# lavTestLRT(modelb_fit, modelc_fit)
# lavTestLRT(modelc_fit, modeld_fit)

# Full SEM Model --------------------------------

# This section reproduces resutls in Table 4

final_model <- '
# Measurement Model 

wb =~ physical_health +
happiness + life_satisfaction + depression +
self_esteem + sense_of_mastery
        
# Regression of interest

wb ~ interfering_density + 
interfering_avg_tie_strength + 
facilitatory_density +
facilitatory_avg_tie_strength +
n_identities_claimed + age + income + white +
male + r_has_college_degree

# Residual correlations 

happiness ~~ life_satisfaction 

'

final_model_fit <- sem(model = final_model, 
                  data = standardized_data, 
                  estimator = "DWLS")

summary(final_model_fit, 
        fit.measures = T, 
        standardized = T)


