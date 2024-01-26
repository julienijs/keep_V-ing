# Loading libraries and packages

install.packages(c("glmnet", "doMC"))
library(readxl)

# from https://github.com/AntheSevenants/ElasticToolsR :
source("Dataset.R")
source("ElasticNet.R")

#### Read dataset ####
keep <- read_xlsx("Keep_Ving_Dataset.xlsx", 
                  sheet=1, 
                  col_names = TRUE)

# Drop "double" annotations used for collexeme analysis
keep <- subset(keep, ID!="double")

subset <- keep[, c("Construction", "author", "Animacy", "Verb_innovation")]

# Convert author & Change to factor
subset$Construction <- as.factor(subset$Construction)
subset$author <- as.factor(subset$author)
subset$Animacy <- as.factor(subset$Animacy)
subset$Verb_innovation <- as.factor(subset$Verb_innovation)

ds <- dataset(df=subset,
              response_variable_column="Construction",
              to_binary_columns=c("author"),
              other_columns=c("Animacy", "Verb_innovation"))

# Convert the data to a feature matrix
feature_matrix <- ds$as_matrix()

# Retrieve feature list
feature_list <- ds$as_feature_list()

#### Perform elastic net regression ####

# Define net object
net <- elastic_net(ds=ds,
                   nfolds=20,
                   type.measure="class")

models <- net$do_elastic_net_regression_auto_alpha(k=10)

models$results
#models$fits

fit <- net$do_elastic_net_regression(alpha=0.6)

coefficients_with_labels <- net$attach_coefficients(fit)

# Save to csv
write.csv(coefficients_with_labels, "elastic_net_output.csv", row.names=FALSE)

#### Visualization with probabilities ####

barplot(coefficients_with_labels$coefficient, 
        names.arg = coefficients_with_labels$feature, 
        col = "lightblue")

# Logistic function to convert log-odds to probabilities
logistic <- function(x) {
  1 / (1 + exp(-x))
}

# Calculate the predicted probabilities for the new observation
probabilities <- logistic(coefficients_with_labels$coefficient)

# Create scatter plot with probabilities
plot(x = probabilities, y = seq_along(probabilities), pch = 16, col = "black",
     xlab = "", ylab = "",
     xlim = c(0, 1), xaxt = "n")

# Add feature labels
text(x = probabilities, y = seq_along(probabilities), 
     labels = coefficients_with_labels$feature, pos = 4, cex = 0.8)

# Customize x-axis labels with adjusted position
axis(1, at = seq(0, 1, by = 0.1), 
     labels = c("complex-transitive", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "continuative"), 
     adj = 0.5)

#### Visualization when the subject is inanimate ####

# Find the coefficient of _is_Inanimate
is_inanimate_coefficient <- coefficients_with_labels$coefficient[coefficients_with_labels$feature == "_is_Inanimate"]

# Calculate probabilities when subject is inanimate
coefficients_with_inanimate <- coefficients_with_labels$coefficient + is_inanimate_coefficient
probabilities_with_inanimate <- logistic(coefficients_with_inanimate)

# Create scatter plot with probabilities including the effect of _is_Inanimate
plot(x = probabilities_with_inanimate, y = seq_along(probabilities_with_inanimate), pch = 16, col = "black",
     xlab = "", ylab = "",
     xlim = c(0, 1), xaxt = "n")

# Add feature labels
text(x = probabilities_with_inanimate, y = seq_along(probabilities_with_inanimate), 
     labels = coefficients_with_labels$feature, pos = 4, cex = 0.8)

# Customize x-axis labels with adjusted position
axis(1, at = seq(0, 1, by = 0.1), 
     labels = c("complex-transitive", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "continuative"), 
     adj = 0.5)


#### Visualization when the verb is innovative ####

# Find the coefficient of _is_Innovative_verb
is_innovative_coefficient <- coefficients_with_labels$coefficient[coefficients_with_labels$feature == "_is_Innovative verb"]

# Calculate probabilities when verb is innovative
coefficients_with_innovative <- coefficients_with_labels$coefficient + is_innovative_coefficient
probabilities_with_innovative <- logistic(coefficients_with_innovative)

# Create scatter plot with probabilities including the effect of _is_Innovative_verb
plot(x = probabilities_with_innovative, y = seq_along(probabilities_with_innovative), pch = 16, col = "black",
     xlab = "", ylab = "",
     xlim = c(0, 1), xaxt = "n")

# Add feature labels
text(x = probabilities_with_innovative, y = seq_along(probabilities_with_innovative), 
     labels = coefficients_with_labels$feature, pos = 4, cex = 0.8)

# Customize x-axis labels with adjusted position
axis(1, at = seq(0, 1, by = 0.1), 
     labels = c("complex-transitive", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "continuative"), 
     adj = 0.5)


