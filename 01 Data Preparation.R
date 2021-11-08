library(tidyverse)
library(igraph)

#########################################################################################
### Load functions
#########################################################################################

source("cascade_helpers.R")

#########################################################################################
### Load data
#########################################################################################

tweets <- read_csv(file = "", progress = TRUE)

#########################################################################################
### Fix rumors
#########################################################################################

#Correcting rumor r_33 and r_45 and removing rumor_id r_1
#Rumor r_33 is actually FALSE and r_45 is TRUE but in the csv the veracity is equal to the rumor_id

tweets$veracity[tweets$rumor_id == "r_33"] <- FALSE
tweets$veracity[tweets$rumor_id == "r_45"] <- TRUE
tweets <- tweets %>% filter(rumor_id != "r_-1")

#########################################################################################
### Add sentiment data
#########################################################################################

emotions <- read_csv("", progress = TRUE)

tweets <- tweets %>% left_join(emotions, by = c("tid" = "tweet_id"))

#########################################################################################
### Add cascade & rumor information
#########################################################################################

## Add cascade information
tweets <- tweets %>% group_by(cascade_id) %>% 
  arrange(tweet_date) %>%
  mutate(cascade_root_date = min(tweet_date),
         cascade_size = row_number()) %>% 
  ungroup() %>% 
  mutate(cascade_root_hours_diff = as.numeric(difftime(tweet_date, cascade_root_date, tz = "UTC", units = "hours")),
         tweet_age = as.numeric(difftime(max(tweet_date), tweet_date, tz = "UTC", units = "days")))

## Calculation of retweet count
tweets <- tweets %>% group_by(cascade_id) %>% group_modify(~ retweet_count(.x)) %>% ungroup()

## Calculation of structural virality
tweets <- tweets %>% group_by(cascade_id) %>% group_modify(~ structural_virality(.x)) %>% ungroup()

#########################################################################################
### Prepare regression dataframe
#########################################################################################

## Filter mixed rumors & scale variables

retweet_df <- tweets %>% 
  filter(veracity != "MIXED") %>%
  mutate(user_verified = as.factor(user_verified == TRUE)) %>%
  mutate(veracity = factor(veracity, levels = c(TRUE, FALSE), labels = c("IsRealNews", "IsFakeNews"))) %>%
  rename(cascade_duration = cascade_root_hours_diff) %>% 
  mutate(user_followees = user_followees / 1000,
         user_followers = user_followers / 1000,
         user_account_age = user_account_age / 365) 

## Emotion scores

retweet_df <- left_join(retweet_df, retweet_df %>% filter(!is.na(anger)) %>% 
                          dplyr::select(cascade_id, anger_root = anger, fear_root = fear, anticipation_root = anticipation, trust_root = trust, 
                                        surprise_root = surprise, sadness_root = sadness, joy_root = joy, disgust_root = disgust),
                        by = c("cascade_id" = "cascade_id")) %>% 
  mutate(emotions_neg = ifelse(is.na(anger_root) | misc == 1, NA, sadness_root + fear_root + surprise_root + disgust_root),
         emotions_pos = ifelse(is.na(anger_root) | misc == 1, NA, joy_root + trust_root + anticipation_root + anger_root))

## Create cascade-level dataframe

cascade_df <- retweet_df %>% group_by(cascade_id) %>% 
  arrange(tweet_date) %>% 
  summarize(cascade_size = n(),
            cascade_duration = as.numeric(difftime(max(tweet_date), min(tweet_date), tz = "UTC", units = "hours")),
            cascade_virality = max(cascade_max_virality),
            user_engagement = head(user_engagement, 1),
            user_verified = head(user_verified, 1),
            user_followers = head(user_followers, 1),
            user_followees = head(user_followees, 1),
            user_account_age = head(user_account_age, 1),
            rumor_category = head(rumor_category, 1),
            rumor_id = head(rumor_id, 1),
            veracity = head(veracity, 1),
            emotions_pos = head(emotions_pos, 1),
            emotions_neg = head(emotions_neg, 1),
            anger_root = head(anger_root, 1),
            anticipation_root = head(anticipation_root, 1),
            disgust_root = head(disgust_root, 1),
            fear_root = head(fear_root, 1),
            joy_root = head(joy_root, 1),
            sadness_root = head(sadness_root, 1),
            surprise_root = head(surprise_root, 1),
            trust_root = head(trust_root, 1))

## Calculate bipolar emotion pairs
cascade_df <- cascade_df %>%
  mutate(diff_joy_sadness = joy_root - sadness_root,
         diff_trust_disgust = trust_root - disgust_root,
         diff_anger_fear = anger_root - fear_root,
         diff_anticipation_surprise = anticipation_root - surprise_root)

## Rename variables

cascade_df <- cascade_df %>% setNames((gsub("_root","", names(.))))
cascade_df <- cascade_df %>% setNames(tolower(gsub("\\.","_",names(.))))

### Select relevant variables

cascade_df <- cascade_df %>% 
  dplyr::select(
    "rumor_id", "veracity",
    "cascade_size", "cascade_duration", "cascade_virality", 
    "user_engagement",  "user_account_age", "user_followers", "user_followees", "user_verified",
    "emotions_pos", "emotions_neg", 
    "anger", "fear", "anticipation", "trust", 
    "surprise", "sadness", "joy", "disgust", 
    "diff_joy_sadness", "diff_trust_disgust", "diff_anger_fear", "diff_anticipation_surprise")

#########################################################################################
### Save preprocessed data
#########################################################################################

regression_df <- cascade_df
save(regression_df, file = "regression_df.Rda")
