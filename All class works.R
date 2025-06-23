library(tidyverse)
library(ggplot2)
library(ggfortify)
library(FactoMineR)
library(ca)
library(vcd)
library(lsr)
library(corrplot)
library(lme4)
library(cluster)
theme_set(theme_bw())
library(lmerTest)

setwd('C:/Users/irina/Documents/АНАЛИЗ ДАННЫХ')



studentnames <- as_tibble(c(5,7,7,5,5,7,4,6,5,9,7))
studentnames %>% 
  ggplot(aes(value)) + 
  geom_histogram(aes(y = after_stat(density)), fill = "grey90") + 
  stat_function(fun = dnorm, args = list(mean = 6.09, sd = 1.45), color = "red")+
  stat_function(fun = dnorm, args = list(mean = 6, sd = 1), color = "darkgreen")+
  stat_function(fun = dnorm, args = list(mean = 7, sd = 2), color = "navy")+
  theme_classic()

mean(studentnames$value)
sd(studentnames$value)


stnamesgener <- as_tibble(c(rnorm(1000, mean = 6.09, sd = 1.45)))


stnamesgener %>% 
  ggplot(aes(value)) + 
  geom_histogram(aes(y = after_stat(density)), fill = "grey90") + 
  stat_function(fun = dnorm, args = list(mean = 6.09, sd = 1.45), color = "red")+
  stat_function(fun = dnorm, args = list(mean = 6, sd = 1), color = "darkgreen")+
  stat_function(fun = dnorm, args = list(mean = 7, sd = 2), color = "navy")+
  theme_classic()

vowels <- read_csv("https://raw.githubusercontent.com/agricolamz/2021_da4l/master/data/phonTools_hillenbrand_1995.csv")
t.test(vowels$dur, mu = 300)

t.test(vowels$dur, mu = 275)

mean(vowels$dur)


diet <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/stcp-Rdataset-Diet.csv")
t.test(diet$pre.weight, diet$weight6weeks, paired = TRUE)



icelandic <- read_csv("https://raw.githubusercontent.com/LingData2019/LingData2020/master/data/icelandic.csv")

icelandic %>% 
  group_by(roundness) %>% 
  summarize(mean(vowel.dur))



icelandic %>% 
  ggplot() +
  geom_boxplot(aes(x = roundness, y = vowel.dur), notch = TRUE) +
  geom_point(aes(x = 1, y = 91.8), colour = "red") +
  geom_point(aes(x = 2, y = 83.7), colour = "red")


icelandic %>% 
  ggplot(aes(x = roundness, y = vowel.dur)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, colour="red", fill="red")


round <- icelandic %>% 
  filter(roundness == 'round')

unrounded <- icelandic %>% 
  filter(roundness == 'unrounded')

t.test(round$vowel.dur, unrounded$vowel.dur, paired = FALSE)
wilcox.test(round$vowel.dur, unrounded$vowel.dur, paired = FALSE)



soc <- read_csv("https://raw.githubusercontent.com/olesar/2023dav4compling/main/data/socling.csv") %>%
  mutate_if(is.character, factor)
soc <- soc %>% mutate(moscow = factor(ifelse(region == "Moscow", "Moscow", "Not Moscow")))

soc1 <- soc %>% mutate(pickle = factor(ifelse(phrase == "соленый огурец", "yes", "no")))

soc1.tabulated <- soc1 %>%
  select(pickle, moscow) %>%
  table()
soc1.chisq <- chisq.test(soc1.tabulated)
soc1.chisq
soc1.chisq$expected
soc1.chisq$residuals

# Проведенный тест хи-квадрат показал (X-squared = 1.6191, df = 1, p-value = 0.2032),
# а значит, при 5%-ом уровне значимости мы не имеем основания отвергать нулевую гипотезу о независимости двух факторов.

fisher.test(soc1.tabulated)


tivo <- read_csv("https://raw.githubusercontent.com/olesar/2023dav4compling/main/data/spanish_tivo.csv") %>%
  select(Last_consonant,Theme_vowel) %>%
  mutate_all(factor)
tivo.chisq <- chisq.test(table(tivo))

cramersV(table(tivo))

mosaic(tivo$Last_consonant ~ tivo$Theme_vowel)
tivo %>% 
  table() %>% 
  mosaicplot(shade = TRUE)


corrplot(tivo.chisq$residuals, is.cor = FALSE)



chekhov <- read_tsv("https://github.com/agricolamz/DS_for_DH/raw/master/data/tidy_chekhov.tsv")
zoshenko <- read_tsv("https://github.com/agricolamz/DS_for_DH/raw/master/data/tidy_zoshenko.csv")

chekhov$author <- "Чехов"
zoshenko$author <- "Зощенко"

chekhov_zoshenko <-
  chekhov %>% 
  bind_rows(zoshenko) %>% 
  filter(str_detect(word, "деньг")) %>% 
  group_by(author, titles, n_words) %>% 
  summarise(n = sum(n)) %>% 
  mutate(log_ratio = log(n/n_words)) %>%
  ungroup()

fit1 <- lm(log_ratio ~ 1, data = chekhov_zoshenko)
summary(fit1)

fit2 <- lm(log_ratio ~ author, data = chekhov_zoshenko)
summary(fit2)

plot(fit1)
plot(fit2)

summary(chekhov_zoshenko)

fit3 <- lm(log_ratio ~ author + n_words, data = chekhov_zoshenko)
summary(fit3)

plot(fit3)

fit4 <- lmer(log_ratio ~ author + (1|titles), data = chekhov_zoshenko)
summary(fit4)



constr <- read_csv('https://raw.githubusercontent.com/LingData2019/LingData/master/data/loaddata.csv')
constr_factorized <- constr %>% 
  mutate(across(where(is.character), as.factor))

fit5 <- glm(CONSTRUCTION ~ VERB, data = constr_factorized, family = 'binomial')
summary(fit5)
plot(fit5)

fit6 <- glm(CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE, data = constr_factorized, family = 'binomial')
summary(fit6)
plot(fit6)

null.model <- glm(CONSTRUCTION ~ 1, data = constr_factorized, family = 'binomial')
step(object = null.model,
     direction = "forward",
     scope = CONSTRUCTION ~ VERB + REDUCED + PARTICIPLE)

summary(null.model)

step(object = null.model,
     direction = "forward",
     scope = CONSTRUCTION ~ VERB * REDUCED * PARTICIPLE)



reg_bnc <- read.csv("https://goo.gl/19QywL")
pca <- prcomp(reg_bnc[,-1], center = TRUE, scale. = TRUE)
summary(pca)

autoplot(pca,
         shape = FALSE,
         loadings = TRUE,
         label = TRUE,
         loadings.label = TRUE)+
  theme_bw()

reg_bnc <- cbind(reg_bnc, pca$x)
reg_bnc %>% 
  ggplot(aes(PC1, PC2, color = Reg))+
  geom_point()+
  stat_ellipse()+
  theme_bw()



poems <- read_tsv('poetry_last_in_lines.csv')


poems.select_1 <- poems %>% 
  group_by(Decade) %>% 
  summarize(count_1 = n())

poems.select_2 <- poems %>% 
  group_by(Decade, UPoS) %>% 
  left_join(poems.select_1) %>% 
  summarize(count_pos = n() / count_1) %>% 
  unique()

poems.select_3 <- poems %>% 
  group_by(Decade, UPoS) %>% 
  summarize(count_pos = n())

poems.select <- poems.select_3 %>% 
  pivot_wider(id_cols = UPoS,
              names_from = Decade,
              values_from = count_pos)

poems.select[is.na(poems.select)] <- 0


row.names(poems.select) <- poems.select[1]
  



poems.tabulated <- poems.select %>%
  table()

poems.tabulated

pca <- prcomp(poems.select[,2:3], center = TRUE, scale. = TRUE)
summary(pca)

autoplot(pca,
         shape = FALSE,
         loadings = TRUE,
         label = TRUE,
         loadings.label = TRUE)+
  theme_bw()


df <- read.csv("https://raw.githubusercontent.com/agricolamz/2018-MAG_R_course/master/data/baltic.csv")

df_dist <- dist(t(df[,3:6]))

plot(hclust(df_dist, method = "ward.D"))

plot(cluster::diana(df_dist))








icelandic <- read_csv('https://raw.githubusercontent.com/olesar/2023dav4compling/main/data/icelandic.csv')

i_place <- icelandic %>% 
  group_by(place) %>% 
  summarize(mean(vowel.dur))

i_speaker <- icelandic %>% 
  group_by(speaker) %>% 
  summarize(mean(vowel.dur))


icelandic %>% 
  ggplot(aes(x = place, y = vowel.dur, fill = place)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="line") +
  theme_bw() +
  geom_jitter(alpha = 0.3)

icelandic %>% 
  ggplot(aes(x = speaker, y = vowel.dur, fill = speaker)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="line") +
  theme_bw() +
  geom_jitter(alpha = 0.3)


i_word <- icelandic %>% 
  group_by(word) %>% 
  summarize(mean(vowel.dur))

icelandic %>% 
  ggplot(aes(x = word, y = vowel.dur, fill = word)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="line") +
  theme_bw() +
  geom_jitter(alpha = 0.3)

fit1 <- lmer(vowel.dur ~ place + (1 | speaker), data = icelandic)
summary(fit1)

fit2 <- glm(vowel.dur ~ place, data = icelandic)
summary(fit2)

fit3 <- lmer(vowel.dur ~ (1 | speaker), data = icelandic)
summary(fit3)

plot(fit1)
plot(fit2)
plot(fit3)

fit4 <- lmer(vowel.dur ~ place + (1+place | speaker), data = icelandic)
summary(fit4)

plot(fit4)


icelandic1 <- icelandic %>%
  mutate(place_labial = ifelse(place == "labial", 1, 2)) %>% 
  mutate(place_velar = ifelse(place == "velar", 3, 0))

icelandic1 <- icelandic1 %>%
  mutate(place_final = ifelse(place_velar == 3, 3, place_labial))

fit5 <- lmer(vowel.dur ~ place_final + (1 | speaker), data = icelandic1)
summary(fit5)


summary(icelandic1)



plot(fit5)



icelandic_df <- as.data.frame(icelandic1)
icelandic_df <- within(icelandic_df, sample <- factor(speaker:word))









data <- data.frame(
  Color = c('Red', 'Green', 'Blue', 'Green', 'Red'),
  Price = c(200, 150, 300, 150, 200)
)

library(lsr)
cramersV(soc1.chisq$observed)

some_data <- soc1.chisq$observed

cramersV(some_data)
