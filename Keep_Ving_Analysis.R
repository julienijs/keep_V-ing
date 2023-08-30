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

generation_histogram <- barplot(generation_table, ylab = "Number of attestations",  
                                xlab = "Generation",
                                main ="Histogram of the number of attestations per generation",
                                col = "grey", 
                                ylim = c(0, 120))

#### General information about the dataset ####
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

# Group infrequent authors together:
# Step 1: Count occurrences of each author
author_counts <- keep %>%
  group_by(author) %>%
  summarize(count = n())

# Step 2: Create a new factor column with 'others' for authors with <= 5 occurrences
keep <- keep %>%
  left_join(author_counts, by = "author") %>%
  mutate(author_grouped = factor(ifelse(count <= 5, "others", author)))

# Step 3: Relevel the 'author_grouped' factor with 'others' as the reference level
keep$author_grouped <- relevel(keep$author_grouped, ref = "others")

#### Grammaticalization scores over time ####

Decade_model <- lm(Score ~ textDecade, data = keep)
summary(Decade_model)
plot(allEffects(Decade_model))

Score_model <- lm(textDecade ~
                    Animacy_score +
                    Continuative_score +
                    Bondedness_score +
                    Motion_score +
                    Verb_score,
                  data = keep)
summary(Score_model)
plot(allEffects(Score_model))

#### Question 1: which variables distinguish between complex-transitive and pure continuative keep? ####

# Convert to factors and relevel
keep$Construction <- as.factor(keep$Construction)
keep$Construction <- relevel(keep$Construction, ref = "Complex-transitive")
keep$Animacy <- as.factor(keep$Animacy)
keep$Animacy <- relevel(keep$Animacy, ref = "Animate")
keep$Verb_innovation <- as.factor(keep$Verb_innovation)
keep$Verb_innovation <- relevel(keep$Verb_innovation, ref = "Collexeme")
keep$Motion_verb <- as.factor(keep$Motion_verb)
keep$Motion_verb <- relevel(keep$Motion_verb, ref = "Motion verb")
keep$Bondedness <- as.factor(keep$Bondedness)
keep$Bondedness <- relevel(keep$Bondedness, ref = "Low bondedness")
keep$Continuative_marker <- as.factor(keep$Continuative_marker)
keep$Continuative_marker <- relevel(keep$Continuative_marker, ref = "Present")

# Make models:
# With random effect
Fixed_and_Random <- glmer(as.factor(Construction) ~ 
                            Animacy + 
                            Verb_innovation +
                            Bondedness + 
                            Motion_verb +
                            Continuative_marker + 
                            (1|author_grouped), 
                          family=binomial(link = "logit"), 
                          data=keep)

summary(Fixed_and_Random)
plot(allEffects(Fixed_and_Random))

# Without random effect
Fixed_Only <- glm(as.factor(Construction) ~
                    Animacy + 
                    Verb_innovation +
                    Bondedness + 
                    Motion_verb +
                    Continuative_marker, 
                  family=binomial(link="logit"), 
                  data=keep)

summary(Fixed_Only) # Higher AIC --> model is worse

# Make plots
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

Continuative_marker <- table(keep$Construction, keep$Continuative_marker)
print(Continuative_marker)
chisq.test(Continuative_marker)
plot(Continuative_marker,
     xlab = "Construction",
     ylab = "Continuative marker",
     main = "")

#### Question 2: How have the constructions grammaticalized over the generations? ####

# Standardizing the scores
keep$StandScores <- (keep$Score-mean(keep$Score))/sd(keep$Score)

# Convert generation to factor
keep$generation <- as.factor(keep$generation) 

# Helmert coding to compare the generations
contrasts(keep$generation) <- contr.helmert(4) 

# Make model
t <- lm(StandScores ~ generation*Construction, data = keep)
summary(t)
plot(allEffects(t))

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

keep_summary <- data_summary(keep, 
                             varname="Score", 
                             groupnames=c("Construction", "generation"))

print(gen_plot <- ggplot(keep_summary, 
                         aes(x = generation, 
                             y = Score, 
                             color = Construction,
                             group = Construction))+
        xlab("Generation")+
        ylab("Score")+
        ylim(0, 6) +
        geom_line()+
        geom_point() +
        geom_errorbar(aes(ymin=Score-sd, 
                          ymax=Score+sd), 
                      width=.2,
                      position=position_dodge(0.01))+ 
        guides(color=guide_legend(title="Construction")))

#### Individuals ####
