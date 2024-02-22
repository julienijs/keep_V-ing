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

#### General information about the dataset ####

# Check number of attestations per generation
generation_table <- table(keep$generation)
print(generation_table)

# Drop 5th generation (too little attestations)
keep <- subset(keep, generation!=5)

generation_histogram <- barplot(generation_table, ylab = "Number of attestations",  
                                xlab = "Generation",
                                main ="Histogram of the number of attestations per generation",
                                col = "grey", 
                                ylim = c(0, 120))

# Histogram of the raw scores
score_table <- table(keep$Score)
mean(keep$Score)
sd(keep$Score)
barplot(score_table,
        main = "Histogram of the number of attestations per score",
        ylab = "Number of attestations",
        xlab = "Summative grammaticalization scores",
        ylim = c(0, 160))

# Type of keep constructions per generation
Generation <- table(keep$Construction, keep$generation)
print(Generation)
chisq.test(Generation)
plot(Generation,
     xlab = "Construction",
     ylab = "Generation",
     main = "")

# Number attestations per author
author_table <- table(keep$author)
print(author_table) # some authors occur very frequently, others a lot less

#### Grammaticalization scores over time ####

# Standardizing the scores
keep$StandScores <- (keep$Score-mean(keep$Score))/sd(keep$Score)
keep$StandAnimacy_score <- (keep$Animacy_score-mean(keep$Animacy_score))/sd(keep$Animacy_score)
keep$StandDurative_score <- (keep$Durative_score-mean(keep$Durative_score))/sd(keep$Durative_score)
keep$StandBondedness_score <- (keep$Bondedness_score-mean(keep$Bondedness_score))/sd(keep$Bondedness_score)
keep$StandMotion_score <- (keep$Motion_score-mean(keep$Motion_score))/sd(keep$Motion_score)
keep$StandVerb_score <- (keep$Verb_score-mean(keep$Verb_score))/sd(keep$Verb_score)
keep$StandAdjective_score <- (keep$Adjective_score-mean(keep$Adjective_score))/sd(keep$Adjective_score)

# Score vs text date
Decade_model <- lm(StandScores ~ textDecade, data = keep)
summary(Decade_model)
plot(allEffects(Decade_model))

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
ggplot(keep, aes(x = textDecade, y = StandAnimacy_score)) +
  geom_point() +
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


