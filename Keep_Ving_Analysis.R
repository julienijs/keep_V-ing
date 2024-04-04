#### Read libraries ####
library(readxl)
library(effects)
library(stats)
library(dplyr)
library(lme4)
library(ggplot2)

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

#### Grammaticalization scores over time ####

# Score vs text date
score_date_model <- lm(Score ~ textDate, data = keep)
summary(score_date_model)
plot(allEffects(score_date_model))

aggregate_plot <- ggplot(keep, aes(x = textDecade, y =Score, shape = as.factor(generation))) +
  geom_jitter() +
  labs(x = "Year of attestation", y = "Summative grammaticalization score", title = "", shape = "Generations") +
  facet_wrap(~ Construction)

print(aggregate_plot)

# Text date vs grammaticalization variables
Score_model <- lm(textDecade ~
                    StandAnimacy_score +
                    StandDurative_score +
                    StandBondedness_score +
                    StandMotion_score +
                    StandVerb_score +
                    StandAdjective_score,
                  data = keep)
summary(Score_model)
plot(allEffects(Score_model))

# Create a line plot
ggplot(keep, aes(x = textDecade, y = StandVerb_score)) +
  geom_smooth() +
  labs(x = "Text Decade", y = "Animacy Score", title = "Text Decade vs. Animacy Score")


#### How have the constructions grammaticalized over the generations? ####

# Convert generation to factor
keep$generation <- as.factor(keep$generation) 

# Helmert coding to compare the generations
contrasts(keep$generation) <- contr.helmert(4) 

# Make model
gen_model <- lm(StandScores ~ generation, data = keep)
summary(gen_model)
plot(allEffects(gen_model))

gen_construction_model <- lm(StandScores ~ generation*Construction, data = keep)
summary(gen_construction_model)
plot(allEffects(gen_construction_model))

# Make plots

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

# Plot
print(score_plot <- ggplot(keep_summary, aes(x = generation, y = Score, linetype = Construction, shape = Construction, group = Construction)) +
              xlab("Generation") +
              ylab("Score") +
              ylim(0, 6) +
              geom_line() +
              geom_point() +
              geom_errorbar(aes(ymin = Score - sd, ymax = Score + sd), width = .2, position = position_dodge(0.01)) +
              guides(linetype = guide_legend(title = "Construction"), shape = guide_legend(title = "Construction")))

# Plot: intransitive, transitive and aggregated

# Calculate mean and standard deviation for both constructions together
both_constructions <- data_summary(keep, varname = "Score", groupnames = "generation")

# Add a column to indicate both constructions
both_constructions$Construction <- "Both"

# Combine data summaries
keep_summary <- rbind(keep_summary, both_constructions)

# Plot
print(score_full_plot <- ggplot(keep_summary, aes(x = generation, y = Score, linetype = Construction, shape = Construction, group = Construction)) +
        xlab("Generation") +
        ylab("Score") +
        ylim(0, 6) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin = Score - sd, ymax = Score + sd), width = .2, position = position_dodge(0.01)) +
        guides(linetype = guide_legend(title = "Construction"), shape = guide_legend(title = "Construction")))


#### Extra plots ####

# Individual authors

# Calculate the proportion of intransitive construction for each author and decade
proportion_data <- keep %>%
  filter(author_grouped != "others") %>%
  group_by(author_grouped, textDecade) %>%
  dplyr::summarize(Proportion = sum(Construction == "Intransitive") / n())

# Plot
ggplot(proportion_data, aes(x = textDecade, y = Proportion, group = author_grouped)) +
  geom_line() +
  geom_point() +
  labs(x = "Decade", y = "Proportion of intransitive construction") +
  facet_wrap(~ author_grouped, ncol = 3) +  # Create separate plots for each author
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5" ,"1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Make bivariate plots
Animacy <- table(keep$Construction, keep$Animacy)
print(Animacy)
chisq.test(Animacy)
plot(Animacy,
     xlab = "Construction",
     ylab = "Animacy",
     main = "")

Verb_innovation <- table(keep$Construction, keep$Verb_innovation)
print(Verb_innovation)
chisq.test(Verb_innovation)
plot(Verb_innovation,
     xlab = "Construction",
     ylab = "Innovativeness of the ing-form",
     main = "")

Bondedness <- table(keep$Construction, keep$Bondedness)
print(Bondedness)
chisq.test(Bondedness)
plot(Bondedness,
     xlab = "Construction",
     ylab = "Bondedness",
     main = "")

Motion_verb <- table(keep$Construction, keep$Motion_verb)
print(Motion_verb)
chisq.test(Motion_verb)
plot(Motion_verb,
     xlab = "Construction",
     ylab = "Motion verb",
     main = "")

Durative_marker <- table(keep$Construction, keep$Durative_marker)
print(Durative_marker)
chisq.test(Durative_marker)
plot(Durative_marker,
     xlab = "Construction",
     ylab = "Durative marker",
     main = "")


