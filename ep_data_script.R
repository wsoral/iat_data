#install.packages(lme4)
#install.packages(car)
#install.packages(emmeans)
#install.packages(tidyverse)
library(tidyverse)
library(lme4)
library(car)
library(emmeans)

rawData <- read_csv("IATrawData.csv")

rawData %>%
  filter(correct & correctLDT > 29) %>%
  mutate(rt = ifelse(rt < 300, 300, rt)) %>%
  mutate(rt = ifelse(rt > 3000, 3000, rt))-> procData

procData$content <- relevel(as.factor(procData$content), "neutral")
procData$level <- relevel(as.factor(procData$level), "I")
procData$block <- relevel(as.factor(procData$block), "congruent")

procData %>%
  group_by(level, content, block) %>%
  summarise(srt = sd(log(rt))) %>% 
  spread("block","srt") %>% 
  mutate(poolSD = sqrt((congruent^2 + incongruent^2)/2)) %>% 
  select(level, content, poolSD)
            
procData %>%
  group_by(level, content, block) %>%
  summarise(mrt = mean(log(rt), trim=0.05)) %>%
  spread(key = "block", value = "mrt") %>%
  left_join(procData %>%
              group_by(level, content, block) %>%
              summarise(srt = sd(log(rt))) %>% 
              spread("block","srt") %>% 
              mutate(poolSD = sqrt((congruent^2 + incongruent^2)/2)) %>% 
              select(level, content, poolSD),
            by = c("level","content")) %>%
  mutate(D = (incongruent - congruent)/poolSD)



fitLmer <- lmer(log(rt) ~ level*content*block + (1 | lp), data=procData)

summary(fitLmer)

Anova(fitLmer, type=3)

emmeans(fitLmer, pairwise ~ content | block | level, adj="scheffe")



