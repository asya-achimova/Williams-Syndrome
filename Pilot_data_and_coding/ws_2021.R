library(tidyverse)
library(stringr)
library(purrr)


setwd("C:/Users/tizma/Desktop/Hiwi/Williams data cleaning")

#data <- read.csv("ws_2021_condition1.csv")
data <- read.csv("Condition 1 Randomized_July 8, 2021_17.24.csv")



head(data)
data <- data[-2,]
colnames(data)
data$c <- NULL



#Exclude Progress under 99% and special cases

exclude <- c(1,2) # Exclude Header and Test-Trial
rawdata <- data
data <- data[data$Progress == 99 | data$Progress == 100, ] %>% slice(-exclude)

NrOfParticipants <- dim(data)[1]



#Split data into block parts to get the in-block order
head(data)
all <- data$allblock_DO
all <- all %>% as_tibble()

notAll <- data$notallblock_DO
notAll <- notAll %>% as_tibble()

or <- data$orblock_DO %>% as_tibble()

some <- data$someblock_DO %>% as_tibble()

two <- data$twoblock_DO %>% as_tibble()


column_names <- c("1","2","3","4","5","6",
                  "7","8","9","10","11","12")

# Include answers

answers <- data[,18:77] 

answersAll <- answers[1:12]
answersNotAll <- answers[13:24]
answersOr <- answers[25:36]
answersSome <- answers[37:48]
answersTwo <- answers[49:60]


answersAll <- unlist(as.list(t(answersAll)))
answersNotAll <- unlist(as.list(t(answersNotAll)))
answersOr <- unlist(as.list(t(answersOr)))
answersSome <- unlist(as.list(t(answersSome)))
answersTwo <- unlist(as.list(t(answersTwo)))






#Seperate Block order specifier

#All
all_new <- all %>%
  separate(
    col = 1,
    into = column_names,
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
) %>% mutate_at(column_names, str_replace, "Q", "")


all_blocks <- data$FL_3_DO %>% 
  str_replace_all("FL_28", "all") %>%
  str_replace_all("FL_30", "notall") %>%
  str_replace_all("FL_31", "or") %>%
  str_replace_all("FL_32", "some") %>%
  str_replace_all("FL_33", "two") %>%
  as_tibble() %>% 
  separate(
    col = 1,
    into = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"),
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
)

all_new <- cbind(all_new, all_blocks)


all_new$quantifier <- "all"
all_new$subjectid <- seq(1,NrOfParticipants,1)
all_new$duration <- data$Duration..in.seconds. 
all_new$language <- data$UserLanguage





#Not all
notAll_new <- notAll %>%
  separate(
    col = 1,
    into = column_names,
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  ) %>% mutate_at(column_names, str_replace, "Q", "")

notAll_blocks <- data$FL_3_DO %>% 
  str_replace_all("FL_28", "all") %>%
  str_replace_all("FL_30", "notall") %>%
  str_replace_all("FL_31", "or") %>%
  str_replace_all("FL_32", "some") %>%
  str_replace_all("FL_33", "two") %>%
  as_tibble() %>% 
  separate(
    col = 1,
    into = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"),
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  )

notAll_new <- cbind(notAll_new, notAll_blocks)

notAll_new$quantifier <- "notall"
notAll_new$subjectid <- seq(1,NrOfParticipants,1)
notAll_new$duration <- data$Duration..in.seconds.
notAll_new$language <- data$UserLanguage


#Or
or_new <- or %>%
  separate(
    col = 1,
    into = column_names,
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  ) %>% mutate_at(column_names, str_replace, "Q", "")

or_blocks <- data$FL_3_DO %>% 
  str_replace_all("FL_28", "all") %>%
  str_replace_all("FL_30", "notall") %>%
  str_replace_all("FL_31", "or") %>%
  str_replace_all("FL_32", "some") %>%
  str_replace_all("FL_33", "two") %>%
  as_tibble() %>% 
  separate(
    col = 1,
    into = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"),
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  )

or_new <- cbind(or_new, or_blocks)

or_new$quantifier <- "or"
or_new$subjectid <- seq(1,NrOfParticipants,1)
or_new$duration <- data$Duration..in.seconds. 
or_new$language <- data$UserLanguage


#Some
some_new <- some %>%
  separate(
    col = 1,
    into = column_names,
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  ) %>% mutate_at(column_names, str_replace, "Q", "")

some_blocks <- data$FL_3_DO %>% 
  str_replace_all("FL_28", "all") %>%
  str_replace_all("FL_30", "notall") %>%
  str_replace_all("FL_31", "or") %>%
  str_replace_all("FL_32", "some") %>%
  str_replace_all("FL_33", "two") %>%
  as_tibble() %>% 
  separate(
    col = 1,
    into = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"),
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  )

some_new <- cbind(some_new, some_blocks) 

some_new$quantifier <- "some"
some_new$subjectid <- seq(1,NrOfParticipants,1)
some_new$duration <- data$Duration..in.seconds. 
some_new$language <- data$UserLanguage


#two
two_new <- two %>%
  separate(
    col = 1,
    into = column_names,
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  ) %>% mutate_at(column_names, str_replace, "Q", "")

two_blocks <- data$FL_3_DO %>% 
  str_replace_all("FL_28", "all") %>%
  str_replace_all("FL_30", "notall") %>%
  str_replace_all("FL_31", "or") %>%
  str_replace_all("FL_32", "some") %>%
  str_replace_all("FL_33", "two") %>%
  as_tibble() %>% 
  separate(
    col = 1,
    into = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"),
    sep = "\\|",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  )

two_new <- cbind(two_new, two_blocks) 
two_new$quantifier <- "two"
two_new$subjectid <- seq(1,NrOfParticipants,1)
two_new$duration <- data$Duration..in.seconds. 
two_new$language <- data$UserLanguage




#Shift long to wide

all_long <- all_new %>% pivot_longer(
  cols = 1:12,
  names_to = "item",
  values_to = "trial"
)

notAll_long <- notAll_new %>% pivot_longer(
  cols = 1:12,
  names_to = "item",
  values_to = "trial"
)

or_long <- or_new %>% pivot_longer(
  cols = 1:12,
  names_to = "item",
  values_to = "trial"
)

some_long <- some_new %>% pivot_longer(
  cols = 1:12,
  names_to = "item",
  values_to = "trial"
)

two_long <- two_new %>% pivot_longer(
  cols = 1:12,
  names_to = "item",
  values_to = "trial"
)


#Add answers to long format blocks

all_long$answer <- answersAll
notAll_long$answer <- answersNotAll
or_long$answer <- answersOr
some_long$answer <- answersSome
two_long$answer <- answersTwo


# Append blocks long format



allBlocks_long <- rbind(all_long, notAll_long, or_long, some_long, two_long)


# corresponding videos

videoNames <- readLines("videonames_raw.txt", warn = F)

videoNames1 <-  grep("1", videoNames, value = T)
videoNames2 <-  grep("2", videoNames, value = T)



videosByBlock <- split(videoNames1, ceiling(seq_along(videoNames1)/12))
videosByBlock <- rep(videosByBlock, each = NrOfParticipants)

videos_long <- unlist(videosByBlock, use.names = F)

allBlocks_long$videoFile <- videos_long



# Creat final file

colOrder <- c("subjectid", "duration",   "language",   
              "Block 1",  "Block 2",   "Block 3",    "Block 4", "Block 5",    
              "trial", "videoFile", "quantifier", "item", "answer")


allBlocks_long <- allBlocks_long[,colOrder]

### July 23, 2021 ###


coding <- read.csv("WS coding 2021.csv")
codingMin <- coding[,c("Item", "Scene","Outcome", "Correct_Response", "Relation", "Condition")]

codingByBlock <- split(codingMin, as.factor(codingMin$Condition))
codingByBlock <- rep(codingByBlock, each = NrOfParticipants)

coding_long <- do.call("rbind", codingByBlock)

tail(coding_long)

allBlocks_long <- cbind(allBlocks_long, coding_long)


colOrder <- c("subjectid", "duration",   "language",   
              "Block 1",  "Block 2",   "Block 3",    "Block 4", "Block 5",    
              "trial", "videoFile", "quantifier", "item", 
              "Item", "Outcome", "Relation", "Condition" ,"answer",
            "Correct_Response")


allBlocks_long <- allBlocks_long[,colOrder]


######## Old code with error ####

#Code answers

coding <- read.csv("WS coding.csv")

codingMin <- coding[,c("Item", "Outcome", "Relation", "Condition")]

codingMin <- codingMin[c(1:26,27,28,34,29,30,31,32,35,33,36,37:nrow(codingMin)),]      #Change order of OR items

codingByBlock <- split(codingMin, as.factor(codingMin$Condition))
codingByBlock <- rep(codingByBlock, each = NrOfParticipants)

coding_long <- do.call("rbind", codingByBlock)

tail(coding_long)

allBlocks_long <- cbind(allBlocks_long, coding_long)


allBlocks_long[12,]


# Rearrange Order

colOrder <- c("subjectid", "duration",   "language",   
              "Block 1",  "Block 2",   "Block 3",    "Block 4", "Block 5",    
              "trial", "videoFile", "quantifier", "item", 
              "Item", "Outcome", "Relation", "Condition" ,"answer")


allBlocks_long <- allBlocks_long[,colOrder]



#Adding score
scores <- coding[,c("Item", "Response", "Correct", "Relation", "Score")]

scores$correctResponse <- paste(scores$Response, scores$Correct, sep=": ")

scores[scores$Correct == "wrong" | scores$Correct == "pragmatic" ,]
scores[scores$Score == 0,]

wrongRows <- rownames(scores[scores$Score == 0,]) %>% as.integer




correctAnswers <- scores$correctResponse

correctAnswers[wrongRows[1]] <- "yes: correct"
correctAnswers[wrongRows[2]] <- "no: correct"
correctAnswers[wrongRows[3]] <- "no: correct"
correctAnswers[wrongRows[4]] <- "yes: semantic"
correctAnswers[wrongRows[5]] <- "yes: semantic"
correctAnswers[wrongRows[6]] <- "yes: semantic"
correctAnswers[wrongRows[7]] <- "yes: semantic"
correctAnswers[wrongRows[8]] <- "yes: semantic"

correctAnswers <- correctAnswers %>% str_replace_all(": correct", "") %>% 
  str_replace_all(": semantic", "") %>%
  str_replace_all("yes", "Yes") %>%
  str_replace_all("no", "No")

coding$correctAnswers <- correctAnswers

coding[c("Condition", "correctAnswers")]

correctAnswersByBlock <- split(coding[c("Condition", "correctAnswers")],
                               as.factor(coding$Condition))

correctAnswersByBlock <- rep(correctAnswersByBlock, 
                             each = NrOfParticipants)

correctAnswersByBlockLong <- do.call("rbind", correctAnswersByBlock)

correctAnswersByBlockLong <- correctAnswersByBlockLong$correctAnswers

allBlocks_long$correctAnswers <- correctAnswersByBlockLong

allBlocks_long$score <- ifelse(allBlocks_long$answer == allBlocks_long$correctAnswers, 1, 0)
  
write.csv2(allBlocks_long, "allBlocks_long.csv", row.names = F)


data_long <-read.csv("allBlocks_long.csv", sep = ";")
or <- data_long %>%
  filter(quantifier == "or" & subjectid == 1)

###PREPARE DATA FOR PLOTS
# byRelation <- split(allBlocks_long, as.factor(allBlocks_long$Relation))
# 
# byRelation[2]
# 
# byQuantifierTruthconditional <- split(byRelation[1], as.factor(byRelation[1]$Condition))
# 
# 
# allBlocks_long %>% as.tibble() %>% group_by(Relation, quantifier) %>% summarise(mean = mean(score))
# 
# view(subset(allBlocks_long, quantifier == "some" & Relation == "entailment"))





##DOUBLE CHECK OR TRUTHCONDITIONAL


# orCheck <- subset(allBlocks_long, quantifier == "or" & Relation == "truthconditional")
# 
# orCheck <- orCheck[,c("item", "Item", "Relation", "score")]
# orCheck
# 
# 
# 
# scoreByItem <- aggregate(score ~ item, data = orCheck, mean)
# 
# orCheckResult <- unique(orCheck[,c("item", "Item")]) %>% arrange(item) %>% cbind(scoreByItem) %>% select(-item)
# orCheckResult
# 
# 
# subset(allBlocks_long, Condition == "Or")[1:50,10:19]
# 
# allBlocks_long[2200:2300,c("videoFile", "Item")]
