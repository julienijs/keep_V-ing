# Loading libraries and packages

# from https://github.com/AntheSevenants/ElasticToolsR :
install.packages(c("glmnet", "doMC"))
source("Dataset.R")
source("ElasticNet.R")

library(readxl)

#### Read dataset ####
keep <- read_xlsx("Keep_Ving_Dataset.xlsx", 
                  sheet=1, 
                  col_names = TRUE)

# Drop "double" annotations used for collexeme analysis
keep <- subset(keep, ID!="double")

subset <- keep[, c("Construction", "author", "textDecade")]

# convert author & Change to factor
subset$Construction <- as.factor(subset$Construction)
subset$author <- as.factor(subset$author)

ds <- dataset(df=subset,
              response_variable_column="Construction",
              to_binary_columns=c("author"),
              other_columns=c("textDecade"))

# convert the data to a feature matrix
feature_matrix <- ds$as_matrix()

# retrieve feature list
feature_list <- ds$as_feature_list()

#### Perform elastic net regression ####

# define net object
net <- elastic_net(ds=ds,
                   nfolds=20,
                   type.measure="class")

models <- net$do_elastic_net_regression_auto_alpha(k=10)

models$results
models$fits

fit <- net$do_elastic_net_regression(alpha=0.1)

coefficients_with_labels <- net$attach_coefficients(fit)

names(coefficients_with_labels)[names(coefficients_with_labels) == "feature"] <- "Author"