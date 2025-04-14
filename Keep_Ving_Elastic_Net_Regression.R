#### Read libraries ####
library(readxl)
library(stats)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
library(glmnet)
library(tibble)
library(effects)

set.seed(20)

#### Read dataset ####
keep <- read_xlsx("Keep_Ving_Dataset.xlsx", 
                  sheet=1, 
                  col_names = TRUE)

author_details <- read_xlsx("author_details.xlsx", 
                  sheet=1, 
                  col_names = TRUE)

keep <- merge(keep, author_details, by = "author", all.x = TRUE)


# Drop "double" annotations used for collexeme analysis
keep <- subset(keep, ID!="double")

# Drop 5th generation (too little attestations)
keep <- subset(keep, generation!=5)

#### Grammaticalization Scores ####

adj_data <- read_xlsx("adjectivalness_analysis.xlsx", 
                  sheet=1, 
                  col_names = TRUE)

keep <- merge(keep, adj_data, by.x = "ing_form", by.y = "Word", all.x = TRUE)



collexeme_data <- read_xlsx("collexemes_keep.xlsx", 
                      sheet=1, 
                      col_names = TRUE)

keep <- merge(keep, collexeme_data, by.x = "Verb", by.y = "words", all.x = TRUE)

keep$Adjectiveness <- 1 - keep$Adjectiveness

keep$Score <- keep$Adj_score +
  keep$Durative_score +
  keep$Aktionsart_score + 
  keep$Animacy_subject_score +
  keep$Bondedness_score +
  keep$Voluntariness_score +
  keep$Adjectiveness +
  keep$Innovativeness_score


#### Grammaticalization variable selection with elastic net regression ####

# Predictor variables
X <- as.matrix(keep[, c("Adj_score",
                        "Innovativeness_score",
                        "Durative_score",
                        "Aktionsart_score",
                        "Animacy_subject_score",
                        "Bondedness_score",
                        "Adjectiveness",
                        "Voluntariness_score")])

correlation_matrix <- cor(X)
print(correlation_matrix)

# Dependent variable
Y <- keep$textDate  

# Define alphas to test
alphas <- seq(0, 1, by = 0.1)

# Initialize an empty data frame to store final results
results <- tibble(alpha = numeric(), lambda = numeric(), mse = numeric(), non_zero = numeric(), model_name = factor())

# Set up the cross-validation loop
for (i in seq_along(alphas)) {
  alpha <- alphas[i]
  
  cvfit_name <- paste("fit_alpha_", i, sep = "")
  
  # Fit the model using cross-validation: mean squared error --> cv.glmnet()
  assign(cvfit_name, cv.glmnet(X, Y, alpha = alpha, type.measure = "mse", nfolds = 10))  # Assign a unique name to each cvfit
  
  # Extract the lambda value for minimum MSE
  lambda_min <- get(cvfit_name)$lambda.min
  
  # Extract mean squared error for lambda.min
  mse_min <- get(cvfit_name)$cvm[which(get(cvfit_name)$lambda == lambda_min)]
  
  # Extract non-zero coefficients for lambda.min
  non_zero <- get(cvfit_name)$nzero[which(get(cvfit_name)$lambda == lambda_min)]
  
  # Combine alpha, lambda, mse, and non-zero coefficients into a data frame
  alpha_lambda_mse <- tibble(
    alpha = alpha,
    lambda = lambda_min,
    mse = mse_min,
    non_zero = non_zero,
    model_name = cvfit_name
  )
  
  # Append to results data frame
  results <- bind_rows(results, alpha_lambda_mse)
}


# Print results
print(results)

# Get coefficients
coefficients <- coef(fit_alpha_3, s = "lambda.min") 

# Print coefficients
print(coefficients)

# Extract coefficients and variable names
coef_values <- as.matrix(coefficients)[-1, ]  # Exclude intercept
variable_names <- names(coef_values)

# Convert coefficients to a data frame
coef_keep <- data.frame(variable = variable_names, coefficient = coef_values)

# Arrange the data frame by coefficient value in descending order
coef_keep <- coef_keep %>%
  arrange(desc(coefficient))

# Rename variables for clean plot
coef_keep$variable <- sub("Adj_score", "Adjective coordination", coef_keep$variable)
coef_keep$variable <- sub("Innovativeness_score", "Innovativeness of the verb", coef_keep$variable)
coef_keep$variable <- sub("Durative_score", "Durativity", coef_keep$variable)
coef_keep$variable <- sub("Animacy_subject_score", "Aktionsart", coef_keep$variable)
coef_keep$variable <- sub("Aktionsart_score", "Animacy", coef_keep$variable)
coef_keep$variable <- sub("Bondedness_score", "Bondedness", coef_keep$variable)
coef_keep$variable <- sub("Adjectiveness_score", "Adjectiveness", coef_keep$variable)
coef_keep$variable <- sub("Voluntariness_score", "Voluntariness", coef_keep$variable)

# Create scatter plot with flipped axes
ggplot(coef_keep, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(color = "black") +
  labs(title = "Elastic net regression with year of attestation as dependent variable",
       x = "Coefficient Value", y = "Predictor Variables") +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5))


#### Author level ####

# One-hot encode author
encoded_authors <- model.matrix(~ author - 1, data = keep)

# Prepare the data
X_authors <- encoded_authors  # Predictor variables
Y_score <- keep$Score  # Dependent variable

# Define alphas to test
alphas <- seq(0, 1, by = 0.1)

# Initialize an empty dataframe to store results
results_authors <- tibble(alpha = numeric(), lambda = numeric(), mse = numeric(), non_zero = numeric(), model_name = factor())

# Set up the cross-validation loop
for (i in seq_along(alphas)) {
  alpha <- alphas[i]
  
  cvfit_name <- paste("authorfit_alpha_", i, sep = "")
  
  # Fit the model using cross-validation: mean squared error --> cv.glmnet()
  assign(cvfit_name, cv.glmnet(X_authors, Y_score, alpha = alpha, type.measure = "mse", nfolds = 10))  # Assign a unique name to each cvfit
  
  # Extract the lambda value for minimum MSE
  lambda_min <- get(cvfit_name)$lambda.min
  
  # Extract mean squared error for lambda.min
  mse_min <- get(cvfit_name)$cvm[which(get(cvfit_name)$lambda == lambda_min)]
  
  # Extract non-zero coefficients for lambda.min
  non_zero <- get(cvfit_name)$nzero[which(get(cvfit_name)$lambda == lambda_min)]
  
  # Combine alpha, lambda, mse, and non-zero coefficients into a dataframe
  alpha_lambda_mse <- tibble(
    alpha = alpha,
    lambda = lambda_min,
    mse = mse_min,
    non_zero = non_zero,
    model_name = cvfit_name
  )
  
  # Append to results dataframe
  results_authors <- bind_rows(results_authors, alpha_lambda_mse)
}

# Print results
print(results_authors)

# Get coefficients
author_coefficients <- coef(authorfit_alpha_2, s = "lambda.min")

# Print coefficients
print(author_coefficients)

# Extract coefficients and variable names
author_values <- as.matrix(author_coefficients)[-1, ]  # Exclude intercept
author_names <- names(author_values)

# Convert coefficients to a data frame
author_coef_keep <- data.frame(variable = author_names, coefficient = author_values)

# Arrange the data frame by coefficient value in descending order
author_coef_keep <- author_coef_keep %>%
  arrange(desc(author_coef_keep))

# Clean up data frame
author_coef_keep$variable <- sub("^author", "", author_coef_keep$variable)
author_coef_keep$author <- author_coef_keep$variable

author_coef_keep <- left_join(author_coef_keep , keep[c("author", "generation", "Born", "Died", "Profession", "place_binary")], by = "author")
author_coef_keep <- author_coef_keep[!duplicated(author_coef_keep$author), ]

# Create scatter plot with flipped axes
ggplot(author_coef_keep, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(aes(color = as.factor(generation))) +
  labs(title = "Elastic net regression with summative grammaticalization score as dependent variable",
       x = "Coefficient Value", y = "Authors",
       color = "Generation") +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5))

birth_coeff_model <- lm(coefficient ~ Born, data = author_coef_keep)
print(summary(birth_coeff_model))
plot(allEffects(birth_coeff_model))

library(ggrepel)

ggplot(author_coef_keep, aes(x = Born, y = coefficient)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = author), 
                  box.padding = 0.5,
                  point.padding = 0.5,
                  max.overlaps = 40) +  # Adjust the max.overlaps value
  labs(title = "", x = "Birth year", y = "Coefficient") +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5))


