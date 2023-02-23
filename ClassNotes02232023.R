# Gabi Faton
# 02-23-2023


library(palmerpenguins)
library(tidyverse)
library(GGally)
library(broom)

head(penguins)

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm,body_mass_g) %>%
  GGally::ggpairs(aes(color=species))


penguins %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()


# Linear Model

lm1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
class(lm1)
summary(lm1)


ggplot(data = penguins,aes(x=bill_length_mm, y=bill_deptj_mm)) +
  geom_point()+
  geom_smooth(method = "lm")

plot(lm1)


gentoo=penguins %>%
  filter(species=="Gentoo")
gentoo %>%
  select(bill_depth_mm, bill_length_mm, data=gentoo)

lm2=lm(bill_depth_mm~bill_length_mm, data=gentoo)
summary(lm2)

plot(lm2)

ggplot(data = gentoo, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point() +
  geom_smooth(data = gentoo, aes(x=bill_length_mm, y=bill_depth_mm), method = "lm")

# plot each species separately

ggplot(data = penguins)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method = "lm")+
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm,))





















