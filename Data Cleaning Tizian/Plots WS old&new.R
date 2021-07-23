library(tidyverse)
library(stringr)
library(purrr)
library(ggplot2)
library(Hmisc)

setwd("C:/Users/tizma/Desktop/Hiwi/Williams data cleaning")

newData <- read.csv2("allBlocks_long.csv")

oldData <- read.csv("WS data 2013 semantics.csv")


#NewData

meanData <- newData %>% as.tibble() %>% group_by(Relation, quantifier) %>% summarise(mean = mean(score), sd = sd(score))



#Old Data

names(oldData)[4] <- "quantifier"
names(oldData)[11] <- "score"


oldData$quantifier <- oldData$quantifier %>%  
  str_replace_all("Not All", "notall") %>%
  str_replace_all("All", "all") %>%
  str_replace_all("Some", "some") %>%
  str_replace_all("Or", "or") %>%
  str_replace_all("Two", "two")

oldMeanData <- oldData %>% as.tibble() %>% group_by(Relation, quantifier) %>% summarise(mean = mean(score), sd = sd(score))


# Combine old and new

meanData$batch <- rep("new", 9)
oldMeanData$batch <- rep("old", 9)

plotData <- rbind(meanData, oldMeanData)
plotData


# Plot Data


#Truthconditional
truthconBar <- ggplot(plotData[plotData$Relation == "truthconditional",], aes(quantifier, mean, fill = batch)) +
  geom_bar(stat = "identity", width = 0.5,  color = "black", position = position_dodge())+
  theme_minimal() + 
  ggtitle("Truthconditional old vs. new") + 
#  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
#                width = .2,
#                position = position_dodge(.5))

truthconBar 

#Entailment
entailmentBar <- ggplot(plotData[plotData$Relation == "entailment",], aes(quantifier, mean, fill = batch)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", position = position_dodge())+
  theme_minimal() + 
  ggtitle("Entailment old vs. new") + 
#  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
#                width = .2,
#                position = position_dodge(.5))

entailmentBar


#Binomial confidence interval

binconData <- subset(newData, quantifier == "or" & Relation == "entailment")[,"score"] %>% table()

binconf(binconData[2], binconData[1]+binconData[2])

scorecount <- table(newData[,c("quantifier","score", "Relation") ])


scorecount <- as.data.frame(scorecount)

#Entailment

NrOfEntailmentTrials <- 220
scorecountEntailment <- subset(scorecount, Relation == "entailment" & score == 1)
scorecountEntailment$NrTrials <- rep(NrOfEntailmentTrials, 5)

scorecountEntailment <- scorecountEntailment %>% as.tibble() %>% group_by(quantifier) %>%
  mutate(binconf = binconf(Freq, NrTrials))

scorecountEntailment


#TruthConditional
scorecountTruthcon <- subset(scorecount, Relation == "truthconditional" & score == 1)

NrOfZeroCases <- subset(scorecount, Relation == "truthconditional" & score == 0)

scorecountTruthcon$NrOfZeroCases <- NrOfZeroCases$Freq

scorecountTruthcon <- scorecountTruthcon %>% as.tibble() %>% group_by(quantifier) %>%
  mutate(binconf = binconf(Freq, Freq+NrOfZeroCases))

scorecountTruthcon





