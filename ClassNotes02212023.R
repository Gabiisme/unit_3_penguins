# Class Notes
# Gabi Faton
# 02-21-2023

# Intro to T-test


library(palmerpenguins)
library(tidyverse)
library(rstatix)

head(penguins)

ggplot(data=penguins)+
  geom_histogram(aes(x=body_mass_g, fill=species)
                 
#one sample t-test
gentoo=penguins%>%
  filter(species=="Gentoo")
head(gentoo)


ggplot(data=gentoo)+
  geom_histogram(aes(x=body_mass_g))

ggplot(data=gentoo)+
  stat_qq(AES(sample=body_mass_g))

gentoo%>%
  summarize(mean_body_mass_g=mean(body_mass_g,na.rm=T)),(
    sd_body_mass_g=sd(body_mass_g, na.rm=T))



# Rub T-test
t.test(gentoo$body_mass_g, mu=5500)

testtestresults = gentoo %>%
  t_test(body_mass_g -1, mu=5500)

# two-sample t-test
data_for_t_test= penguins%>%
  filter(species%in% c("Gentoo", "Adelie")
         !is.na(body_mass_g))%>%
  select(species, body_mass_g)%>%
  select(species, body_mass_g)%>%
  droplevels()

summary(data_for_t_test)

data_for_t_test%>%
  group_by(species)%>%
  summarize(mean=mean(body_mass_g)+
            (stat_qq(aes(sample=body_mass_g)) +
            facet_wrap(`species, scales="free")

            
            
#### Correction

ggplot(data=data_for_t_test)+
  statqq(aes(sample=body_mass_g))=
  facet_wrap(`species, scale="free")


# Check equality of variance assumption
data_for_t_test%>%
  levene_test(body_mass_g~species)

t.test(data_for_t_test$body_mass_g~data_for_t_test$species)

# Bill length correlated to bill depth

ggplot(data=gentoo)+
  geom_point(aes(x=bill_lenght_mm, y=bill_depth_mm))

cor(x=gentoo$bill_lenght_mm, y=gentoo$bill_depth_mm, use="complete.obs")
cor.test(x=gentoo$bill_lenght_mm,y=gentoo$bill_depth_mm,use"complete.obs")

gentoo%>%
  cor_test(bill_lenght_mm, bill_depth_mm)


# Correlation Matrix

cor(gentoo[ , c(3:6)], use= "complete.obs")

library(GGally)

penguins%>%
  select(species,bill_lenght_mm, boll_depth_mm, flipper_lenght_m, boy_mass_g)%>%
  ggpairs(aes(color=species))

                 