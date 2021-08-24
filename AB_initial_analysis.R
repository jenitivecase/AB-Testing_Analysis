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
  mutate(ExpGroups = factor(ExpGroups, levels = c("None", "General", "Explicit")))


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
  theme(legend.position = "bottom") +
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
  geom_rect(data = NPS_boxes, mapping=aes(xmin=xmin, xmax=xmax, ymin = 0.2, ymax = 0.8, fill=category),
            alpha = 0.8) +
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
  
