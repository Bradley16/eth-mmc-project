library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)

results <- read.csv("results.csv")

TMO_name <- function(letter) {
  return(revalue(letter,
                 c("O"="Original", "D"="Drago", "K"="Kuang", "M"="Mertens",
                   "W"="WardHistAdj")))
}
Scene_name <- function(letter) {
  letter <- as.factor(letter)
  return(revalue(letter,
                 c("K"="Kalamaja2", "N"="Niguliste", "P"="Ptln1", "T"="Toompea4")))
}


ratings <- results %>%
  select(-Timestamp) %>%
  melt() %>%
  mutate(Scene=Scene_name(str_sub(variable, 1, 1)),
         TMO=TMO_name(str_sub(variable, 2, 2))) %>%
  mutate(TMO=relevel(as.factor(TMO), ref="Original")) %>%
  rename(Rating=value) %>%
  select(-variable)


ratings %>%
  group_by(TMO) %>%
  summarise(MeanRating=mean(Rating)) %>%
  arrange(desc(MeanRating))

# Overall
fit <- lm(Rating ~ TMO, data=ratings)
summary(fit)

# By image
fit <- lm(Rating ~ TMO, data=(ratings %>% filter(Scene=="Kalamaja2")))
summary(fit)

fit <- lm(Rating ~ TMO, data=(ratings %>% filter(Scene=="Niguliste")))
summary(fit)

fit <- lm(Rating ~ TMO, data=(ratings %>% filter(Scene=="Ptln1")))
summary(fit)

fit <- lm(Rating ~ TMO, data=(ratings %>% filter(Scene=="Toompea4")))
summary(fit)


