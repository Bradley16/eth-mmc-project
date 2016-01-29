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
  select(-variable) %>%
  mutate(Rating=(Rating-1)/6) # Scale


ratings %>%
  group_by(TMO) %>%
  summarise(MeanRating=mean(Rating)) %>%
  arrange(desc(MeanRating))

fit <- lm(Rating ~ TMO, data=ratings)
summary(fit)





# ---- Fit beta distribution to each TMO ----
estBetaParams <- function(vec) {
  mu <- mean(vec)
  var <- var(vec)
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
estBetaParamA <- function(vec) {
  return(estBetaParams(vec)$alpha)
}
estBetaParamB <- function(vec) {
  return(estBetaParams(vec)$beta)
}

ratings %>%
  group_by(TMO) %>%
  summarise(Alpha=estBetaParamA(Rating),
            Beta=estBetaParamB(Rating))

plot_beta_dist = function(alpha, beta) {
  x <- seq(0, 1, length=100)
  y <- dbeta(x, alpha, beta)
  ggplot() +
    geom_line(aes(x=x, y=y)) +
    xlim(0, 1)
}



