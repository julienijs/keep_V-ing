#### Read libraries ####
library(readxl)
library(stats)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
library(glmnet)
library(tibble)

#### Read dataset ####
keep <- read_xlsx("Keep_Ving_Dataset.xlsx", 
                  sheet=1, 
                  col_names = TRUE)

# Drop "double" annotations used for collexeme analysis
keep <- subset(keep, ID!="double")


#### Grammaticalization Scores ####

keep$Score <- keep$Adj_score + 
  keep$Verb_score + 
  keep$Durative_score +
  keep$Aktionsart_score + 
  keep$Animacy_subject_score +
  keep$Bondedness_score +
  keep$Adjectiveness_score +
  keep$Voluntariness_score

#### Grammaticalization variable selection with elastic net regression ####

# Predictor variables
X <- as.matrix(keep[, c("Adj_score", "Verb_score", "Durative_score", "Aktionsart_score", 
                        "Animacy_subject_score", "Bondedness_score", "Adjectiveness_score", 
                        "Voluntariness_score")])

# Dependent variable
y <- keep$textDate  

# Set seed for reproducibility
set.seed(50)

# Define alphas to test
alphas <- seq(0, 1, by = 0.1)

# Initialize an empty dataframe to store results
results <- tibble(alpha = numeric(), lambda = numeric(), mse = numeric(), non_zero = numeric())

# Set up the cross-validation loop
for (i in seq_along(alphas)) {
  alpha <- alphas[i]
  
  # Fit the model using cross-validation: mean squared error --> cv.glmnet()
  cvfit <- cv.glmnet(X, y, alpha = alpha, type.measure = "mse")
  
  # Extract the lambda value for minimum MSE
  lambda_min <- cvfit$lambda.min
  
  # Extract mean squared error for lambda.min
  mse_min <- cvfit$cvm[which(cvfit$lambda == lambda_min)]
  
  # Extract non-zero coefficients
  non_zero <- cvfit$nzero[which(cvfit$lambda == lambda_min)]
  
  # Combine alpha, lambda, mse, and non-zero coefficients into a dataframe
  alpha_lambda_mse <- tibble(
    alpha = alpha,
    lambda = lambda_min,
    mse = mse_min,
    non_zero = non_zero
  )
  
  # Append to results dataframe
  results <- bind_rows(results, alpha_lambda_mse)
}

# Print results
print(results)

# Train model with best alpha and lambda from results
variables_enet_model <- glmnet(X, y, alpha = 0.6, lambda = lambda_min)

# Get coefficients
coefficients <- coef(variables_enet_model)

# Print coefficients
print(coefficients)

# Extract coefficients and variable names
coef_values <- as.matrix(coefficients)[-1, ]  # Exclude intercept
variable_names <- names(coef_values)

# Convert coefficients to a data frame
coef_df <- data.frame(variable = variable_names, coefficient = coef_values)

# Arrange the data frame by coefficient value in descending order
coef_df <- coef_df %>%
  arrange(desc(coefficient))

# Create scatter plot with flipped axes
ggplot(coef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(color = "black") +
  labs(title = "Elastic net regression with year of attestation as dependent variable",
       x = "Coefficient Value", y = "Predictor Variables") +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5))

#### Author level ####

# One-hot encode author
encoded_authors <- model.matrix(~ author - 1, data = keep)

# Prepare the data
X_authors <- encoded_authors  # Predictor variables
y_score <- keep$Score  # Dependent variable

# Define alphas to test
alphas <- seq(0, 1, by = 0.1)

# Initialize an empty dataframe to store results
results_authors <- tibble(alpha = numeric(), lambda = numeric(), mse = numeric(), non_zero = numeric())

# Set up the cross-validation loop
for (i in seq_along(alphas)) {
  alpha <- alphas[i]
  
  # Fit the model using cross-validation: mean squared error --> cv.glmnet()
  cvfit <- cv.glmnet(X_authors, y_score, alpha = alpha, type.measure = "mse")
  
  #Extract the lambda value for minimum MSE
  lambda_min <- cvfit$lambda.min
  
  # Extract mean squared error for lambda.min
  mse_min <- cvfit$cvm[which(cvfit$lambda == lambda_min)]
  
  # Extract non-zero coefficients
  non_zero <- cvfit$nzero[which(cvfit$lambda == lambda_min)]
  
  # Combine alpha, lambda, mse, and non-zero coefficients into a dataframe
  alpha_lambda_mse <- tibble(
    alpha = alpha,
    lambda = lambda_min,
    mse = mse_min,
    non_zero = non_zero
  )
  
  # Append to results dataframe
  results_authors <- bind_rows(results_authors, alpha_lambda_mse)
}

# Print results
print(results_authors)

# Refit the model with the best lambda
authors_enet_model <- glmnet(X_authors, y_score, alpha = 0.2, lambda = 0.112)

# Get coefficients
author_coefficients <- coef(authors_enet_model)

# Extract coefficients and author names
author_coef_values <- as.matrix(author_coefficients)[-1, ]  # Exclude intercept
author_names <- names(author_coef_values)

# Convert coefficients to a data frame
author_coef_df <- data.frame(variable = author_names, coefficient = author_coef_values)

# Arrange the data frame by coefficient value in descending order
author_coef_df <- author_coef_df %>%
  arrange(desc(coefficient))

# Clean up data frame
author_coef_df$variable <- sub("^author", "", author_coef_df$variable)

# Create scatter plot with flipped axes
ggplot(author_coef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(color = "black") +
  labs(title = "Elastic net regression with summative grammaticalization score as dependent variable",
       x = "Coefficient Value", y = "Authors") +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5))
