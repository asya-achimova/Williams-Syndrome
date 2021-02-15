library(purrr)
library(rlang)
library(ggplot2)

# number of objects that behave in a particular manner
# I put the name "Constant" at the end, because this value should not be modified
allStateNumbersConst <- c(0,1,2,3)

# the names of a all (four) utterances
utteranceNames <- c("some","all","one","two","notall","none")

# utteranceSelector
#utteranceSelector <- c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
#utteranceSelector <- c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
utteranceSelector <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
utteranceValidIndices <- which(utteranceSelector)
utteranceInValidIndices <- which(utteranceSelector==FALSE)

# corresponding utterance truth functions.
# utterances as boolean functions of stateNumber
# f(stateNumber) -> boolean
utteranceFunctions <- list(
  some <- function(stateNumber){stateNumber > 0},
  all <- function(stateNumber) {stateNumber == 3},
  one <- function(stateNumber){stateNumber == 1},
  two <- function(stateNumber){stateNumber == 2},
  notall <- function(stateNumber){stateNumber != 3},
  none <- function(stateNumber){stateNumber == 0}
)
# f(stateNumber) -> 0/1
utteranceFunctions01 <- list(
  some <- function(stateNumber){ifelse(stateNumber > 0, 1, 0)},
  all <- function(stateNumber) {ifelse(stateNumber == 3, 1, 0)},
  one <- function(stateNumber){ifelse(stateNumber == 1, 1, 0)},
  two <- function(stateNumber){ifelse(stateNumber == 2, 1, 0)},
  notall <- function(stateNumber){ifelse(stateNumber != 3, 1, 0)},
  none <- function(stateNumber){ifelse(stateNumber == 0, 1, 0)}
)

# From this, create a truth value matrix. 
# Rows: states (i.e. vectorOfallStatesConstant values) 
# Columns: utterances 
truthValuesForAllUtterancesForAllStateNumsConst <- array( 
  c(
    lapply(allStateNumbersConst, utteranceFunctions[[1]]),
    lapply(allStateNumbersConst, utteranceFunctions[[2]]),
    lapply(allStateNumbersConst, utteranceFunctions[[3]]),
    lapply(allStateNumbersConst, utteranceFunctions[[4]]), 
    lapply(allStateNumbersConst, utteranceFunctions[[5]]), 
    lapply(allStateNumbersConst, utteranceFunctions[[6]])), 
  dim = c(4,6))
colnames(truthValuesForAllUtterancesForAllStateNumsConst) = c("some","all","one","two","notall","none")
rownames(truthValuesForAllUtterancesForAllStateNumsConst) = c("zero","one","two","three")
## and with 0 and 1 s also.
truthValuesForAllUtterancesForAllStateNumsConst01 <- array( 
  c(
    lapply(allStateNumbersConst, utteranceFunctions01[[1]]),
    lapply(allStateNumbersConst, utteranceFunctions01[[2]]),
    lapply(allStateNumbersConst, utteranceFunctions01[[3]]),
    lapply(allStateNumbersConst, utteranceFunctions01[[4]]),
    lapply(allStateNumbersConst, utteranceFunctions01[[5]]),
    lapply(allStateNumbersConst, utteranceFunctions01[[6]])), 
  dim = c(4,6))
colnames(truthValuesForAllUtterancesForAllStateNumsConst01) = c("some","all","one","two","notall","none")
rownames(truthValuesForAllUtterancesForAllStateNumsConst01) = c("zero","one","two","three")

# returns a uniform prob. mass over a list (e.g. a set of states)
getUniformPrior <- function(states){
  return(rep(1/length(states), length(states)))
}

allStatesConst = matrix(
  c(c(FALSE,FALSE,FALSE),
    c(FALSE,FALSE,TRUE),
    c(FALSE,TRUE,FALSE),
    c(FALSE,TRUE,TRUE),
    c(TRUE,FALSE,FALSE),
    c(TRUE,FALSE,TRUE),
    c(TRUE,TRUE,FALSE),
    c(TRUE,TRUE,TRUE)),
  nrow = 8,
  ncol = 3,
  byrow = TRUE)
allStateNamesConst <- c("fff","fft","ftf","ftt","tff","tft","ttf","ttt")
rownames(allStatesConst) = allStateNamesConst



#
allNumIndividualStatesConst <- apply(allStatesConst, 1, sum)

# determines the number of true values.
getNumStatesTrue <- function(states) {
  sum(states)
}

# From this, create a truth value matrix. 
# Rows: states (i.e. vectorOfallStatesConstant values) 
# Columns: utterances 
truthValuesForAllUtterancesForAllStatesConst <- array( 
  c(
    lapply(allNumIndividualStatesConst, utteranceFunctions[[1]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions[[2]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions[[3]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions[[4]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions[[5]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions[[6]])), 
  dim = c(8,6))
colnames(truthValuesForAllUtterancesForAllStatesConst) = c("some","all","one","two","notall","none")
rownames(truthValuesForAllUtterancesForAllStatesConst) = allStateNamesConst

## and with 0 and 1 s also.
truthValuesForAllUtterancesForAllStatesConst01 <- array( 
  c(
    lapply(allNumIndividualStatesConst, utteranceFunctions01[[1]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions01[[2]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions01[[3]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions01[[4]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions01[[5]]),
    lapply(allNumIndividualStatesConst, utteranceFunctions01[[6]])), 
  dim = c(8,6))
colnames(truthValuesForAllUtterancesForAllStatesConst01) = c("some","all","one","two","notall","none")
rownames(truthValuesForAllUtterancesForAllStatesConst01) = allStateNamesConst


# setting the "states Prior" as a global variable to the uniform prior (over all states).
stateIndividualPriorConst <- getUniformPrior(allNumIndividualStatesConst)

stateNumPriorConst <- getUniformPrior(allStateNumbersConst)

# returns log likelihoods for all possible states given an utterance
literalListenerIndividualStates <- function(uttIndex){
  truthValues <- unlist(truthValuesForAllUtterancesForAllStatesConst01[,uttIndex])+1e-100
  posterior <- truthValues * stateIndividualPriorConst
  posterior <- posterior/sum(posterior) 
  return(log(posterior))
}

literalListenerMatrixConst <- array(
  c(
    literalListenerIndividualStates(1),
    literalListenerIndividualStates(2),
    literalListenerIndividualStates(3),
    literalListenerIndividualStates(4),
    literalListenerIndividualStates(5),
    literalListenerIndividualStates(6)),
  dim = c(8,6)
)
colnames(literalListenerMatrixConst) = c("some","all","one","two","notall","none")
rownames(literalListenerMatrixConst) = allStateNamesConst

# returns log likelihoods for all possible states given an utterance
literalListenerNumStates <- function(uttIndex){
  truthValues <- unlist(truthValuesForAllUtterancesForAllStateNumsConst01[,uttIndex])+1e-100
  posterior <- truthValues * stateNumPriorConst
  posterior <- posterior/sum(posterior) 
  return(log(posterior))
}

alphaConst <- 1

getAllStatesTF <- function(stateTF, accessTF) {
  apply(allStatesConst)
}


speaker <- function(stateTF, accessTF) {
  # first get the constallations that are possible given the access and the state of the three objects
  accessIndices <- which(accessTF, TRUE)
  relevantStateValues <- stateTF[accessIndices]
  relevantStates <- as.matrix(allStatesConst[,accessIndices])
  applicableRows <- which(apply(relevantStates, 1, identical, relevantStateValues),TRUE)
  # the state that the speaker believes may apply
  applicableStates <- allStatesConst[applicableRows,]
  
  # uniform over the applicable states.
  P_o_as <- rep(1/length(applicableRows), length(applicableRows))
  # now we calculate the weighted sum... 
  Lik_w_oa <-  exp(alphaConst * literalListenerMatrixConst[applicableRows, ])
  if(!is.matrix(Lik_w_oa)) {
    Lik_w_oa <- t(as.matrix(Lik_w_oa))
  }
  Lik_w_oa[,utteranceInValidIndices] <- NA
  P_w_oa <- t(apply(Lik_w_oa, 1, function(a){return(a/sum(a, na.rm = TRUE))}))
  return(P_o_as %*% P_w_oa)
}

# # TEST Speaker function...: 
# stateTF <- c(TRUE, TRUE, FALSE)
# accessTF <- c(TRUE, TRUE, FALSE)

listener <- function(utteranceIndex, accessTF) {
  statePriors <- rep(1/nrow(allStatesConst), nrow(allStatesConst))
  statePosteriors <- as.matrix(rep(0, nrow(allStatesConst)))
  rownames(statePosteriors) <- allStateNamesConst
  for(i in c(1:nrow(allStatesConst))) {
    statePosteriors[i] <- speaker(allStatesConst[i,],accessTF)[utteranceIndex]
  }
  return(statePosteriors / sum(statePosteriors))
}

# Quantifiers 1 "some",2 "all",3 "one", 4 "two",5 "notall",6 "none"

# # TEST Listener function...: 
# stateTF <- c(TRUE, TRUE, FALSE)
#accessTF <- c(FALSE, FALSE, FALSE)

accessTF <- c(TRUE, TRUE, TRUE)
round(listener(1, accessTF),2)

states <- c(0,1,2,3)
probability <- c(0, 0.45, 0.45, 0.08)

kn_speaker <- as.data.frame(cbind(states, probability))
kn_speaker$speaker <- "knowledgeable"

accessTF <- c(FALSE, FALSE, FALSE)
round(listener(1, accessTF),2)

probability <- c(0.12, 0.36, 0.36, 0.12)
ign_speaker <- as.data.frame(cbind(states, probability))
ign_speaker$speaker <- "ignorant"

output <- rbind(kn_speaker, ign_speaker)

ggplot(output, aes(x = states, y = probability, fill = speaker)) +
  geom_bar(stat = "identity", color="grey45", width = 0.8, position=position_dodge()) +
  ylim (0,0.7) +
#  labs(title = "I bet that some of the clouds will turn black") +
  annotate(geom="text", x=1.5, y=.6, label="I bet that some of the clouds will turn black",
           color="red") +
  scale_fill_brewer(palette="Accent") +
  xlab("How many clouds turn black") +
  ylab("Probability that listener picks a number") +
  theme_bw()

#### Plot average probability. Quantifier 'some' #####

accessTF <- c(TRUE, TRUE, TRUE)
result <- as.data.frame(round(listener(1, accessTF),2))
result$number <- c(0, 1, 1, 2, 1, 2, 2, 3)
result$speaker <- "knowledgeable"
colnames(result) <- c("probability", "number", "speaker")

accessTF <- c(FALSE, FALSE, TRUE)
result2 <- as.data.frame(round(listener(1, accessTF),2))
result2$number <- c(0, 1, 1, 2, 1, 2, 2, 3)
result2$speaker <- "ignorant"
colnames(result2) <- c("probability", "number", "speaker")

output2 <- rbind(result, result2)

ggplot(output2, aes(x = number, y = probability, fill = speaker)) +
  geom_bar(stat = "identity", color="grey45", width = 0.8, position=position_dodge()) +
  ylim (0,0.4) +
  #  labs(title = "I bet that some of the clouds will turn black") +
  annotate(geom="text", x=1.5, y=.3, label="I bet that some of the clouds will turn black",
           color="red") +
  scale_fill_brewer(palette="Accent") +
  xlab("How many clouds turn black") +
  ylab("Probability that listener picks a number") +
  theme_bw()

#### Plot average probability, restructure x axis Quantifier 'some' #####

ggplot(output2, aes(x = speaker, y = probability, fill = as.factor(number))) +
  geom_bar(stat = "identity", color = "grey", size=0.05,width = 0.8, position=position_dodge()) +
  ylim (0,0.3) +
  scale_x_discrete(limits = c("ignorant", "knowledgeable"),
                   labels = c("Ignorant (adults)", "Knowledgeable (children)")) +
  labs(title = "Average probability of different states") +
  annotate(geom="text", x=1.5, y=.25, label="I bet that some of the clouds will turn black",
           color="red") +
  scale_fill_brewer(palette="Blues", name = "Number of clouds") +
  xlab("The type of speaker that the listener assumes") +
  ylab("Probability of acceptance") +
  theme_bw() +
  ggsave("barplot_by_speaker.pdf", width = 5.5, height = 4, units = "in")
