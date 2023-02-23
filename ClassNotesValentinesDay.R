
  # Gabi Faton
  #2023-02-14
  # GG Plot

library(tidyverse)
library(palmerpenguins)

head(penguins)

# create a scatter Plot with tidy verse GG plot 

Penguin_Without_nas = penguins%>%
  filter(!is.na(flipper_length_mm))

ggplot(data=penguins)+
  geom_point(aes(x=flipper_lenght_mm, y=body_mass_g, color=species, shape=species)) +
  geom_smooth(aes(x= flipper_lenght_mm, y=body_mass_g)) +
  xlab("F;ipper lenght mm") +
  ylab("Body Mass g")
  theme_bw()
ggsave(filename="figure/flipper.png", width= 7, height= 5 , units="in", dpi= 300)


penguins_ts = penguins %>%
  group_by(year, species) %>%
  summarize(num_penguins =n())
ggplot(data=penguins_ts) +
  geom_line(aes(x=year, y=num_penguins, color=species))

ggplot(data=penguins) +
  geom_histogram(aes(x=flipper_lenght_mm, color=species), possition="identity", alpa= 0.5)


scale_fill_manual(values=c("darkorange", "cyan4", "darkorchid"))


ggplot(data=penguins) +
  geom_boxplot(aes(y=flipperA_lenght_mm, x=species)) +
  geom_jitter(aes(y=flipper_lenght_mm, x=species, color=species), width=0.2)

ggplot(data=penguins)+
  geom_bar(aes(x=sex, fill=species))+
  facet_wrap(~species, nrow=3)

ggplot(data=penguins)+
  geom_bar(aes(x=island, fill=species))+
  facet_wrap(~species, nrow=3)
coord_flip()
