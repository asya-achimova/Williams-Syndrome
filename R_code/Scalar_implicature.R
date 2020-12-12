library(purrr)
library(rlang)
states <- c(0,1,2,3)

utterances <- list(
  some <- function(){states > 0},
  all <- function() {states == 3},
  two <- function(){states == 2},
  notall <- function(){states != 3}
)

names(utterances) <- c("some","all","two","notall")

utteranceNames <- c("some","all","two","notall")

getUniformPrior <- function(states){
  probability <- 1/length(states)
  prior <- rep(probability, length(states))
  return(prior)
}


literalListener <- function(utterance, states){
  statesPrior <- getUniformPrior(states)
  posterior <- rep(0, length(states))
  truthValue <- utterance()
  posterior[which(truthValue == TRUE)] <- 1
  posterior <- posterior * statesPrior
  posterior/sum(posterior) 
}


speaker <- function(state, alpha, utterances){
  utterancesPrior <- getUniformPrior(utterances)
  output <- rep(0, length(utterances))
  for (word in c(1:length(utterances))) {
    ll <- literalListener(utterances[[word]],states)
    utility <- exp(alpha * log(ll[state+1])) * utterancesPrior[word]
    output[word] <- utility
  }
  if (sum(output) != 0){
    return(output/sum(output))
  } else {return(output)}
}

alpha <- 1

quantifier <- data[i,"quantifier"]
index <- which(quantifier %in% utteranceNames)

pragmaticListener <- function(utterance, index){
  output <- rep(0, length(states))
  for (state in states){
    uttProb <- speaker(state,alpha,utterances)
    output[state+1] <- uttProb[index]
  }
  return(output/sum(output))
}

randomDraw <- function(probability){
  draw <- rbinom(1,1,probability)
  ifelse(draw == 1, TRUE, FALSE)
}

getBelief <- function(access,actualState){
  allStates <- ifelse(access,state,randomDraw(0.5))
  sum(allStates)
}
access <- c(TRUE, TRUE, FALSE)
actualState <- c(TRUE, TRUE, TRUE)

uncertainSpeaker <- function(actualState, access, alpha, utterances){
  utterancesPrior <- getUniformPrior(utterances)
  belief <- getBelief(access, actualState)
  output <- rep(0, length(utterances))
  for (word in c(1:length(utterances))) {
    ll <- literalListener(utterances[[word]],states)
    utility <- exp(alpha * log(ll[belief+1])) * utterancesPrior[word]
    output[word] <- utility
  }
  if (sum(output) != 0){
    return(output/sum(output))
  } else {return(output)}
}

uncertainListener <- function(access, utterance, index){
  output <- rep(0, length(states))
  for (state in states){
    uttProb <- uncertainSpeaker(actualState, access, alpha, utterances)
    output[state+1] <- uttProb[index]
  }
  return(output/sum(output))
}
