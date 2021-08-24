#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

sapply(c("lubridate", "stringr", "tidytext", "tibble", "car", "purrr"), load_packages)


### READING, DATA DICT, JOINING ####
exp_var_title <- "Tactics Version"

data <- read.csv("B Testing_August 23, 2021_09.20.csv")

datadict <- data.frame(code = names(data),
                       longname = as.character(data[1,]))

data <- data[3:nrow(data),]

data <- as_tibble(data,
                  .name_repair = "check_unique") %>%
  type.convert(as.is = TRUE)


pre_data <- data %>%
  filter(!is.na(D1)) 

pre_data <- pre_data[, c(names(pre_data[,1:52]), "S1", "Link")]

post_data <- data %>%
  filter(!is.na(Q1006)) 

post_data <- post_data[, 53:ncol(data)] %>%
  select(-Link)

joined_data <- pre_data %>%
  full_join(post_data, by = "S1") %>%
  arrange(S1, IPAddress) %>%
  unique() %>%
  mutate(ExpGroups = case_when(Link == 'http://packages.kognito.com/abt_tactics_1/' ~ 'Explicit',
                                Link == 'http://packages.kognito.com/abt_tactics_2/' ~ 'General',
                                Link == 'http://packages.kognito.com/abt_tactics_3/' ~ 'None')) %>%
  filter(!is.na(ExpGroups)) %>%
  mutate(ExpGroups = factor(ExpGroups, levels = c("None", "General", "Explicit"))) %>%
  mutate(Q1001 = factor(Q1001, levels = c("Extremely negative", "Moderately negative", "Slightly negative",
                                          "Neither positive nor negative",
                                          "Slightly positive", "Moderately positive", "Extremely positive"))) %>%
  mutate(Q1002 = factor(Q1002, levels = c("Not innovative at all", "Slightly innovative", "Moderately innovate", 
                                          "Very innovative", "Extremely innovative"),
                        labels = c("Not innovative at all", "Slightly innovative", "Moderately innovative", 
                                            "Very innovative", "Extremely innovative"))) %>%
  mutate(Q1003 = factor(Q1003, levels = c("Not well made at all", "Slightly well made", "Moderately well made",
                                          "Very well made", "Extremely well made"))) %>%
  mutate(Q1004 = factor(Q1004, levels = c("Not often at all", "Slightly often", "Moderately often", 
                                          "Very often", "Extremely often"))) %>%
  mutate(Q1005 = factor(Q1005, levels = c("Not likely at all", "Slightly likely", "Moderately likely", 
                                          "Very likely", "Extremely likely"))) %>%
  mutate(Q1007 = factor(Q1007, levels = c("Not necessary at all", "Slightly necessary", "Moderately necessary", 
                                          "Very necessary", "Extremely necessary")))    

groups <- unique(joined_data$ExpGroups)

### SCORES ####
score_means <- joined_data %>%
  group_by(ExpGroups) %>%
  summarise(n = n(),
            MeanScore = mean(Q44, na.rm = TRUE)) 

score_anova <- aov(Q44 ~ ExpGroups, data = joined_data)
Anova(score_anova)

contrast1 = c(-2,1,1)
contrast2 = c(0,-1,1)
# contrast3 = c(2,-1,-1)
# contrast4 = c(-1,2,-1)
# contrast5 = c(-1,-1,2)

contrasts(joined_data$ExpGroups) = cbind(contrast1, contrast2)

score_anova <- aov(Q44 ~ ExpGroups, data = joined_data)
summary.lm(score_anova)
#basically - No tactics isn't sufficiently different from the ones with tactics.
#but general vs explicit tactics is a stat sig difference
#starting to think the generalized tactics make things harder by being tricky


ggplot(joined_data) +
  geom_histogram(aes(x = Q44, fill = ExpGroups, group = ExpGroups)) +
  facet_grid(ExpGroups ~ .) +
  geom_vline(data = score_means, aes(xintercept = MeanScore, group = ExpGroups)) +
  geom_text(data = score_means, aes(x = MeanScore, y = 35, group = ExpGroups,
                                    label = round(MeanScore, 2)),
            nudge_x = 7) +
  scale_fill_manual(values = JB_palette[c(2,4,6)]) +
  theme(legend.position = "none") +
  labs(title = paste0("Score Distributions by ", exp_var_title),
       x = "Reported Score",
       y = "Participants",
       fill = "Tactics Version") 


### NPS ####
NPS_overall_means <- joined_data %>%
  filter(!is.na(Q1006_NPS_GROUP)) %>%
  group_by(ExpGroups) %>%
  summarise(expgroup_n = n(),
            MeanNPS = mean(Q1006, na.rm = TRUE)) 

NPS_group_scores <- joined_data %>%
  filter(!is.na(Q1006_NPS_GROUP)) %>%
  group_by(ExpGroups, Q1006_NPS_GROUP) %>%
  summarise(NPS_group_n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = Q1006_NPS_GROUP, values_from = NPS_group_n) %>%
  group_by(ExpGroups) %>%
  mutate(Total = sum(Detractor, Passive, Promoter)) %>%
  mutate(NPS_score = ((Promoter/Total) - (Detractor/Total)) * 100)


NPS_boxes <- data.frame(category = c("Needs Improvement", "Good", "Great", "Excellent"),
                        xmin = c(-100, 0, 30, 70),
                        xmax = c(0, 30, 70, 100))




ggplot(NPS_group_scores) +
  geom_rect(data = NPS_boxes, mapping=aes(xmin=xmin, xmax=xmax, ymin = 0.25, ymax = 0.75, fill=category),
            alpha = 0.75) +
  scale_y_continuous(limits = c(0, 1), breaks = NULL, minor_breaks = NULL) +
  facet_grid(ExpGroups ~ .) +
  geom_vline(aes(xintercept = NPS_score), size = 1) +
  geom_text(aes(x = NPS_score, y = 0.5, label = round(NPS_score, 1)),
            nudge_x = -10) +
  scale_x_continuous(limits = c(-100, 100), 
                     breaks = c(-100, 0, 30, 70,100),
                     minor_breaks = NULL) +
  scale_fill_manual(values = c("Needs Improvement" = "tomato",
                               "Good" = "gold",
                               "Great" = "palegreen3",
                               "Excellent" = "steelblue3")) +
  theme(legend.position = "bottom") + 
  labs(title = paste0("NPS Scores by ", exp_var_title),
       x = "NPS Score",
       y = "",
       fill = "Category") 


ggplot(joined_data) +
  geom_histogram(aes(x = Q1006, fill = Q1006_NPS_GROUP),
                 binwidth = 1) +
  facet_grid(. ~ ExpGroups) +
  # geom_vline(data = NPS_overall_means, aes(xintercept = MeanNPS, group = ExpGroups)) +
  # geom_text(data = NPS_overall_means, aes(x = MeanNPS, y = 30, group = ExpGroups,
  #                                   label = round(MeanNPS, 1)),
  #           nudge_x = .5) +
  scale_x_continuous(limits = c(-0.5, 10.5), breaks = seq(0, 10, 1),
                     minor_breaks = NULL) +
  scale_fill_manual(values = c("Detractor" = "tomato",
                               "Passive" = "gray60",
                               "Promoter" = "steelblue3")) +
  theme(legend.position = "bottom") +
  labs(title = paste0("NPS Distributions by ", exp_var_title),
       x = "NPS Rating",
       y = "Participants",
       fill = "Tactics Version") + 
  coord_flip()

NPS_score_lm <- lm(Q1006 ~ Q44, data = joined_data)  
summary(NPS_score_lm)

NPS_score_tactic_lm <- lm(Q1006 ~ Q44 + ExpGroups, data = joined_data)  
summary(NPS_score_tactic_lm)

NPS_score_tactic_interact <- lm(Q1006 ~ Q44 + ExpGroups + Q44*ExpGroups, data = joined_data)  
summary(NPS_score_tactic_interact)

ggplot(joined_data, aes(x = Q44, y = Q1006)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_grid(ExpGroups ~ .) +
  labs(title = "Score is not a statistically significant predictor of NPS",
       subtitle = paste0("For any ", exp_var_title),
       x = "Score",
       y = "NPS") +
  scale_y_continuous(limits = c(-0.5, 10.5), breaks = seq(0, 10, 2)) 
  

### MIKAT ITEMS ####

#recode to be scored variables
#this code sucks and i'm sorry but at least it's clear, right?
#... right?
#so to be safe: the Q number == SOME VALUE is the important part
#TRUE ~ 0 just means not that is wrong.
joined_data <- joined_data %>%
  mutate(Q32 = case_when(Q32 == FALSE ~ 1,
                         TRUE ~ 0),
         Q33 = case_when(Q33 == TRUE ~ 1,
                         TRUE ~ 0),
         Q34 = case_when(Q34 == TRUE ~ 1,
                         TRUE ~ 0),
         Q35 = case_when(Q35 == FALSE ~ 1,
                         TRUE ~ 0),
         Q36 = case_when(Q36 == TRUE ~ 1,
                         TRUE ~ 0),
         Q37.1 = case_when(Q37.1 == FALSE ~ 1,
                         TRUE ~ 0)) %>%
  mutate(Q1022 = case_when(Q1022 == FALSE ~ 1,
                         TRUE ~ 0),
         Q1023 = case_when(Q1023 == TRUE ~ 1,
                         TRUE ~ 0),
         Q1024 = case_when(Q1024 == TRUE ~ 1,
                         TRUE ~ 0),
         Q1025 = case_when(Q1025 == FALSE ~ 1,
                         TRUE ~ 0),
         Q1026 = case_when(Q1026 == TRUE ~ 1,
                         TRUE ~ 0),
         Q1027 = case_when(Q1027 == FALSE ~ 1,
                           TRUE ~ 0)) %>%
  mutate(MIKAT_pre_score = pmap_dbl(.l = list(Q32, Q33, Q34, Q35, Q36, Q37.1),
                                    .f = sum)) %>%
  mutate(MIKAT_post_score = pmap_dbl(.l = list(Q1022, Q1023, Q1024, Q1025, Q1026, Q1027),
                                    .f = sum)) %>%
  mutate(MIKAT_change = pmap_dbl(.l = list(MIKAT_post_score, MIKAT_pre_score),
                                 .f = function(x, y){x-y}))

MIKAT_ttest <- t.test(joined_data$MIKAT_pre_score, joined_data$MIKAT_post_score, paired = TRUE, alternative = "less")

MIKAT_stat_subtitle <- ifelse(MIKAT_ttest$p.value <= 0.05,
                             "Post-test scores were significantly higher, indicating knowledge gain",
                             "Post-test scores were NOT significantly higher")
  
ggplot(joined_data) + 
  geom_histogram(aes(x = MIKAT_pre_score, fill = JB_palette[1]),
                 binwidth = 1, alpha = 0.5) +
  geom_histogram(aes(x = MIKAT_post_score, fill = JB_palette[4]),
                 binwidth = 1, alpha = 0.5) +
  #label the posttest mean
  geom_vline(aes(xintercept = mean(MIKAT_post_score)), size = 1, color = "black") +
  geom_text(aes(x = mean(MIKAT_post_score), y = 100, 
                label = paste0("Post mean:", round(mean(MIKAT_post_score), 2))),
            nudge_x = .95, color = "black") +
  #label the pretest mean
  geom_vline(aes(xintercept = mean(MIKAT_pre_score)), size = 1, color = JB_palette[1]) +
  geom_text(aes(x = mean(MIKAT_pre_score), y = 100, 
                label = paste0("Pre mean:", round(mean(MIKAT_pre_score), 2))),
            nudge_x = -.95, color = JB_palette[1]) +
  scale_fill_identity(name = 'Pre/Post', guide = 'legend',labels = c('Post', 'Pre')) +
  theme(legend.position = "none") + 
  labs(title = paste0("Adapted MIKAT Pre/Post Scores for the full sample"),
       subtitle = MIKAT_stat_subtitle,
       caption = "According to a one-sided paired sample t-test",
       x = "Adapted MIKAT Score",
       y = "Participants") 


MIKAT_grouped_means <- joined_data %>%
  select(MIKAT_pre_score, MIKAT_post_score, ExpGroups) %>%
  group_by(ExpGroups) %>%
  summarise(MIKAT_pre_mean = mean(MIKAT_pre_score),
            MIKAT_post_mean = mean(MIKAT_post_score)) %>%
  mutate(MIKAT_change = MIKAT_post_mean - MIKAT_pre_mean)

MIKAT_grouped_ttests <- vector("character", length(groups))
for(group in 1:length(groups)){
  temp <- t.test(filter(joined_data, ExpGroups == groups[group])$MIKAT_pre_score,
                 filter(joined_data, ExpGroups == groups[group])$MIKAT_post_score,
                 paired = TRUE, alternative = "less")
  
  if(temp$p.value <= 0.05) {
    MIKAT_grouped_ttests[group] <- as.character(groups[group])
  } 
}


if (all.equal(as.character(groups), MIKAT_grouped_ttests)){
  MIKAT_grouped_subtitle <- "Post-test scores were significantly higher for all groups,\nindicating knowledge gain"
} else if (all.equal(MIKAT_grouped_ttests, vector("character", length(groups)))){
  MIKAT_grouped_subtitle <- "Post-test scores were NOT significantly higher for any of the groups"
} else {
  MIKAT_grouped_subtitle <- paste0("Post-test scores were significantly higher for these groups:",
                                   paste0(MIKAT_grouped_ttests, collapse = ", "))
}


ggplot(joined_data) + 
  geom_histogram(aes(x = MIKAT_pre_score, fill = JB_palette[1], group = ExpGroups),
                 binwidth = 1, alpha = 0.5) +
  geom_histogram(aes(x = MIKAT_post_score, fill = JB_palette[4], group = ExpGroups),
                 binwidth = 1, alpha = 0.5) +
  facet_grid(ExpGroups ~ .) +
  #label the posttest mean
  geom_vline(data = MIKAT_grouped_means, 
             aes(xintercept = MIKAT_post_mean, group = ExpGroups), size = 1, color = "black") +
  geom_text(data = MIKAT_grouped_means, 
            aes(x = MIKAT_post_mean, group = ExpGroups, y = 100, 
                label = paste0("Post mean:", round(MIKAT_post_mean, 2))),
            nudge_x = .95, color = "black") +
  #label the pretest mean
  geom_vline(data = MIKAT_grouped_means, 
             aes(xintercept = MIKAT_pre_mean, group = ExpGroups), size = 1, color = JB_palette[1]) +
  geom_text(data = MIKAT_grouped_means, 
            aes(x = MIKAT_pre_mean, group = ExpGroups, y = 100, 
                label = paste0("Pre mean:", round(MIKAT_pre_mean, 2))),
            nudge_x = -.95, color = JB_palette[1]) +
  scale_fill_identity(name = 'Pre/Post', guide = 'legend',labels = c('Post', 'Pre')) +
  theme(legend.position = "none") + 
  labs(title = paste0("Adapted MIKAT Pre/Post Scores by ", exp_var_title),
       subtitle = MIKAT_grouped_subtitle,
       caption = "According to a one-sided paired sample t-test",
       x = "Adapted MIKAT Score",
       y = "Participants") 

MIKAT_anova <- aov(MIKAT_change ~ ExpGroups, data = joined_data)
Anova(MIKAT_anova)

contrast1 = c(-2,1,1)
contrast2 = c(0,-1,1)
# contrast3 = c(2,-1,-1)
# contrast4 = c(-1,2,-1)
# contrast5 = c(-1,-1,2)

contrasts(joined_data$ExpGroups) = cbind(contrast1, contrast2)

MIKAT_anova <- aov(Q44 ~ ExpGroups, data = joined_data)
summary.lm(MIKAT_anova)
#same story with the results from the ANOVA


### PRODUCT QUESTIONS ####

product_questions <- c(paste0("Q", 1001:1005), "Q1007")


product_question_graphing <- function(q_col){
  label <- datadict[datadict$code == q_col, "longname"]
  
  q_col <- as.name(q_col)
  
  question_means <- joined_data %>%
    group_by(ExpGroups) %>%
    summarise(n = n(),
              MeanRating = mean(as.numeric({{q_col}}), na.rm = TRUE)) 
  
  response_levels <- joined_data %>% 
    pull({{q_col}}) %>% 
    levels()
  
  indices_to_keep <- c(1, ceiling(length(response_levels)/2), length(response_levels))
  indices_to_discard <- seq(1, length(response_levels), 1)
  indices_to_discard <- indices_to_discard[!indices_to_discard %in% indices_to_keep]
  
  response_labels <- response_levels
  response_labels[indices_to_discard] <- ""
  response_labels[indices_to_keep] <- paste0(indices_to_keep, ": ", response_labels[indices_to_keep])
  
  ggplot(joined_data) +
    geom_histogram(aes(x = as.integer({{q_col}}),
                       fill = ExpGroups, group = ExpGroups),
                   binwidth = 1) +
    facet_grid(ExpGroups ~ .) +
    scale_x_continuous(limits = c(0.5, length(response_levels)+0.5), 
                       breaks = seq(1, length(response_levels), 1),
                       minor_breaks = NULL, 
                       labels = response_labels) +
    geom_vline(data = question_means, aes(xintercept = MeanRating, group = ExpGroups)) +
    geom_text(data = question_means, aes(x = MeanRating, y = 35, group = ExpGroups,
                                         label = round(MeanRating, 2)),
              nudge_x = 0.5) +
    scale_fill_manual(values = JB_palette[c(2,4,6)]) +
    theme(legend.position = "none") +
    labs(subtitle = paste0("Rating Distributions by ", exp_var_title),
         title = linetrunc(label, 50),
         x = "Rating",
         y = "Participants",
         fill = "Tactics Version") 
}

for(question in product_questions){
  print(product_question_graphing(question))
}


