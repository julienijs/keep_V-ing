#### Read libraries ####
library(readxl)
library(effects)
library(stats)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)

#### Read dataset ####
keep <- read_xlsx("Keep_Ving_Dataset.xlsx", 
                  sheet=1, 
                  col_names = TRUE)

# Drop "double" annotations used for collexeme analysis
keep <- subset(keep, ID!="double")

# Check number of attestations per generation
generation_table <- table(keep$generation)
print(generation_table)

# Drop 5th generation (too little attestations)
keep <- subset(keep, generation!=5)

# Type of keep constructions per generation
Generation <- table(keep$Construction, keep$generation)
print(Generation)

# Number attestations per author
author_table <- table(keep$author)
print(author_table) # some authors occur very frequently, others a lot less


#### Grammaticalization Scores ####

keep$Score <- keep$Adj_score + 
              keep$Verb_score + 
              keep$Durative_score +
              keep$Aktionsart_score + 
              keep$Animacy_subject_score +
              keep$Bondedness_score +
              keep$Adjectiveness_score +
              keep$Voluntariness_score

# Histogram of the scores
histogram_facet <- ggplot(keep, aes(x = Score)) +
  geom_bar(stat = "count", color = "black") +
  labs(x = "Score", y = "Frequency", title = "Histogram of the summative grammaticalization scores") +
  facet_wrap(~ Construction)

print(histogram_facet)

# Calculate the mean score and standard deviation per construction level
construction_summary <- keep %>%
  group_by(Construction) %>%
  summarise(mean_score = mean(Score),
            sd_score = sd(Score))

# Print the summary
print(construction_summary)

#### Aggregate level ####

# Score vs text date
score_date_model <- lm(Score ~ textDate, data = keep)
summary(score_date_model)

# Score vs construction
score_construction_model <- lm(Score ~ Construction + textDecade, data = keep)
summary(score_construction_model)

# Visualization
aggregate_plot <- ggplot(keep, aes(x = textDecade, y =Score, shape = as.factor(generation))) +
  geom_jitter() +
  labs(x = "Year of attestation", y = "Summative grammaticalization score", title = "", shape = "Generations") +
  facet_wrap(~ Construction)

print(aggregate_plot)

#### Grammaticalization variables ####

# Text date vs grammaticalization variables
score_model <- lm(textDate ~
                    Adj_score + 
                    Verb_score + 
                    Durative_score +
                    Aktionsart_score + 
                    Animacy_subject_score +
                    Bondedness_score +
                    Adjectiveness_score +
                    Voluntariness_score,
                  data = keep)
summary(score_model)
plot(allEffects(score_model))
     
# Check for multicollinearity
vif_scores <- vif(score_model)
print(vif_scores)


#### Generation level ####
# Convert generation to factor
keep$generation_factor <- as.factor(keep$generation) 

# Helmert coding to compare the generations
contrasts(keep$generation_factor) <- contr.helmert(4) 

# Score vs generation
generation_model <- lm(Score ~ generation_factor + Construction, data = keep)
summary(generation_model)
plot(allEffects(generation_model))

# Score vs generation in interaction with construction
generation_construction_model <- lm(Score ~ generation_factor*Construction, data = keep)
summary(generation_construction_model)
plot(allEffects(generation_construction_model))

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
# to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#++++++++++++++++++++++++++
# Plot: intransitive vs transitive

# Calculate mean and standard deviation for transitive and intransitive construction
keep_summary <- data_summary(keep, 
                             varname="Score", 
                             groupnames=c("Construction", "generation"))

# Visualization
generation_plot <- ggplot(keep_summary, aes(x = generation, y = Score, linetype = Construction, shape = Construction, group = Construction)) +
  xlab("Generation") +
  ylab("Score") +
  ylim(0, 6) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Score - sd, ymax = Score + sd), width = .2, position = position_dodge(0.01)) +
  guides(linetype = guide_legend(title = "Construction"), shape = guide_legend(title = "Construction"))

print(generation_plot)

# Plot: intransitive, transitive and aggregated

# Calculate mean and standard deviation for both constructions together
both_constructions <- data_summary(keep, varname = "Score", groupnames = "generation")

# Add a column to indicate both constructions
both_constructions$Construction <- "Both"

# Combine data summaries
keep_summary <- rbind(keep_summary, both_constructions)

# Visualization
print(score_full_plot <- ggplot(keep_summary, aes(x = generation, y = Score, linetype = Construction, shape = Construction, group = Construction)) +
        xlab("Generation") +
        ylab("Score") +
        ylim(0, 6) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin = Score - sd, ymax = Score + sd), width = .2, position = position_dodge(0.01)) +
        guides(linetype = guide_legend(title = "Construction"), shape = guide_legend(title = "Construction")))
