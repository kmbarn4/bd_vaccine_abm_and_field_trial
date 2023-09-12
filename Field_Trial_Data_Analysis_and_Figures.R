### Clean Field Swab and Challenge Data and Figures ###

## Load in Field Swab Data
field_swab_data <- read.csv("/Users/kmbarnett/Desktop/upload to github/field trial data files/all_years_field_swab_data.csv")
head(field_swab_data)

## Field Swab Infection Intensity Model ####
library(glmmTMB)
infection_intensity_model_covar = glmmTMB(round(swab1) ~ treatment*before.after + (1 | pond) + (1 | year), family="nbinom2", ziformula = ~ treatment*before.after + (1 | pond) + (1 | year), data = field_swab_data)
summary(infection_intensity_model_covar)

library(emmeans)
field_inten_emm_log = emmeans(infection_intensity_model_covar, ~ treatment*before.after)
field_inten_emm_log

## Interaction Plot - Field Swab Infection Intensity ####
field_inf_inten <- read.csv("/Users/kmbarnett/Desktop/upload to github/field trial data files/field_inf_inten_all_years.csv", header = T)
head(field_inf_inten)

library(ggplot2)
library(tidyverse)

positions <- c("before", "after")
field_inf_inten %>%
  ggplot(aes(factor(time), bd_load)) +
  geom_line(size = 1.2, aes(group = treatment, color = treatment)) +
  geom_point(size = 2.6, aes(color = treatment), shape = 15) + theme_classic() + scale_x_discrete(limits= positions) +
  scale_y_continuous(limits = c(0, 13)) + geom_linerange(aes(x =factor(time), ymin = lower.CL, ymax = upper.CL, colour = treatment)) +
  scale_colour_manual(values = c("#a6bddb", "#02818a")) 

#### Field Swab Prevalence Model ####
library("glmmTMB")
field_prev_model = glmmTMB(infected ~ treatment*before.after + (1 | pond) + (1 | year), family="binomial", data = field_swab_data)
field_null_prev_model = glmmTMB(infected ~ 1, family="binomial", data = field_swab_data)
anova(field_prev_model, field_null_prev_model)
summary(field_prev_model)
field_null_prev_model

library(emmeans)
emm_field_prev = emmeans(field_prev_model, ~ treatment*before.after, type = "response")
emm_field_prev

### Interaction Plot - Field Swab Prevalence
field_prev_swab <- read.csv("/Users/kmbarnett/Desktop/upload to github/field trial data files/prev_field_swab_all_years.csv", header = T)
head(field_prev_swab)

positions <- c("before", "after")
field_prev_swab %>%
  ggplot(aes(factor(time), prob_of_infection)) +
  geom_line(size = 1.2, aes(group = treatment, color = treatment)) +
  geom_point(size = 2.6, aes(color = treatment), shape = 15) + theme_classic() + scale_x_discrete(limits= positions) +
  geom_linerange(aes(x =factor(time), ymin = lower.CL, ymax = upper.CL, colour = treatment)) +
  scale_colour_manual(values = c("#a6bddb", "#02818a")) 

## Load in Challenge Experiment Data
challenge_data <- read.csv(file = '/Users/kmbarnett/Desktop/upload to github/field trial data files/all_challenge_data.csv')
head(challenge_data)

## Challenge Experiment Infection Intensity Model ####
library(glmmTMB)

challenge_infection_intensity_model = glmmTMB(round(swab2) ~ treatment*before.after + (1 | pond), family="nbinom2", ziformula = ~treatment*before.after + (1 | pond), data = challenge_data, REML = FALSE)
summary(challenge_infection_intensity_model)

library(emmeans)
challenge_emm_log = emmeans(challenge_infection_intensity_model, ~ treatment*before.after)
challenge_emm_log

## Interaction Plot - Challenge Experiment Infection Intensity ####
challenge_inf_inten <- read.csv("/Users/kmbarnett/Desktop/upload to github/field trial data files/challenge_inf_inten.csv", header = T)
head(challenge_inf_inten)

library(ggplot2)
library(tidyverse)

positions <- c("before", "after")
challenge_inf_inten %>%
  ggplot(aes(factor(time), bd_load)) +
  geom_line(size = 1.2, aes(group = treatment, color = treatment)) +
  geom_point(size = 2.6, aes(color = treatment), shape = 15) + theme_classic() + scale_x_discrete(limits= positions) +
  scale_y_continuous(limits = c(0, 13)) + geom_linerange(aes(x =factor(time), ymin = lower.CL, ymax = upper.CL, colour = treatment)) +
  scale_colour_manual(values = c("#d95f02", "#7570b3")) 

## Challenge Experiment Prevalence Model ####
challenge_prev_model = glmmTMB(bd_end ~ treatment*before.after + (1 | pond), family="binomial", data = challenge_data)
challenge_null_prev_model = glmmTMB(bd_end ~ 1, family="binomial", data = challenge_data)
anova(challenge_prev_model, challenge_null_prev_model)
summary(challenge_prev_model)

library(emmeans)
emm_challenge_prev = emmeans(challenge_prev_model, ~ treatment*before.after, type = "response")
emm_challenge_prev

## Interaction Plot - Challenge Prevalence ####
challenge_prev <- read.csv("/Users/kmbarnett/Desktop/upload to github/field trial data files/challenge_prev.csv", header = T)
head(challenge_prev)

library(ggplot2)
library(tidyverse)

positions <- c("before", "after")
challenge_prev %>%
  ggplot(aes(factor(time), prob_of_infection)) +
  geom_line(size = 1.2, aes(group = treatment, color = treatment)) +
  geom_point(size = 2.6, aes(color = treatment), shape = 15) + theme_classic() + scale_x_discrete(limits= positions) +
  scale_y_continuous(limits = c(0, 1)) + geom_linerange(aes(x =factor(time), ymin = lower_CI, ymax = upper_CI, colour = treatment)) +
  scale_colour_manual(values = c("#d95f02", "#7570b3")) 
