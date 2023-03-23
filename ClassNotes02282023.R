# Class Notes 
# Gabi Faton
# 02-28-2023

library(tidyverse)
library(palmerpenguins)
library(gvlma)  # gvlma()
library(GGally)
library(car)
library(MASS)
library(rstatix) # identify_outliers()
library(ggiraph)
library(ggiraphExtra)

# Multiple regression

head(penguins)

penguinslm3 = penguins %>%
  filter(!is.na(bill_length_mm),
         !is.na(bill_length_mm),
         !is.na(species))

dim(penguinslm3)


lm3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguinslm3)
summary(lm3)
coef(lm3[1])
anova(lm3)
MyResults = broom::tidy(lm3, confint=T, conf.level=0.95) %>%
  mutate_if(is.numeric,round,2)
MyResults$estimate

# visualize model

ggPredict(lm3, se=T, interactive = T)

lm3Predictions = predict(lm3, interval = "confidence", level = 0.95)
head(lm3Predictions)
head(penguinslm3)


Penguinslm3Predict = cbind(penguinslm3, lm3Predictions)
head(Penguinslm3Predict)

# alpha is for transparency, fill must be place in first of course

ggplot(data=Penguinslm3Predict, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(aes(ymin=lwr,ymax=upr, fill=species), alpha=0.5) +
  geom_line(aes(y=fit))+
  geom_point()


# generate new data

NewDataBillLenghtmm = seq(min(penguinslm3$bill_length_mm),max(penguinslm3$bill_length_mm),
                          by=0.1)

NewData = expand.grid(bill_length_mm = NewDataBillLenghtmm, species = unique(penguinslm3$species))

NewDataPredictlm3 = cbind(NewData, predict(lm3, NewData=NewData, interval = "confidence"))

# tidy verse augment function

lm3Predict = lm3 %>%
  broom::augment(data = penguinslm3, se_fit=T, interval="confidence")
glimpse(lm3Predict)

# generate new data
newdata=penguinslm3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)
lm3Predictions=lm3 %>%
  broom::augment(newdata=newdata, se_fit=T, interval="confidence")
head(lm3Predict)

# Visualize

# Interaction Term

lm4 = lm(bill_depth_mm~bill_length_mm+species+bill_length_mm:species, data=penguinslm3)

AIC(lm3,lm4)
BestModel = step(lm4)
summary(BestModel)

# Plot with Interaction

lm4Predict = lm4 %>%
  broom::augment(interval = "confidence")


ggplot(data=lm4Predict)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_line(aes(y=.fitted,x=bill_length_mm, color=species))+
  geom_ribbon(aes(ymin=.lower,ymax=.upper,x=bill_length_mm,fill=species), alpa=0.5)

# depth~lengthh + flipper

gentoo = penguins %>%
  filter(species=="Gentoo")
lmgentoo1 = lm(bill_depth_mm~bill_length_mm, data=gentoo)
lmgentoo2 = lm(bill_depth_mm~bill_length_mm + flipper_length_mm, data = gentoo)
lmgentoo3 = lm(bill_depth_mm~bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)

newdata1= gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm=median(gentoo$flipper_length_mm, na.rm=T),
         body_mass_g = median(gentoo$body_mass_g, na.rm=T))

lmgentoo3predict = lmgentoo3 %>%
  broom::augment(newdata1=newdata1, interval = "confidence")

ggplot(data = lmgentoo3predict)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo)+
  geom_line(aes(y=.fitted, x=bill_length_mm))+
  geom_ribbon(aes(ymin=.lower,ymax=.upper,x=bill_length_mm), alpa=0.5)


# ANOVA
 head(penguins)

 Penguinlm = lm(body_mass_g ~ species + sex, data=penguins)
 summary(Penguinlm)
 anova(Penguinlm)  

 Penguinanova = aov(body_mass_g ~ species + sex, data=penguins)
 
 penguins %>%
   group_by(sex) %>%
   summarize(mean_body_mass_g = mean(body_mass_g))
 
 penguins %>%
   group_by(species) %>%
   summarize(mean_body_mass_g = mean(body_mass_g, na.rm=TRUE))
 TukeyHSD(Penguinanova)

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 