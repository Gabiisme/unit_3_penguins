# Gabi Faton
# 03-23-2023

library(palmerpenguins)
library(tidyverse)
library(GGally)
library(broom)

head(penguins)

pen_drop_na = penguins %>%
  drop_na()

pen_num = pen_drop_na %>%
  select(ends_with("mm"), body_mass_g) 

head(pen_drop_na)

pen_meta = pen_drop_na %>%
  select(species, sex, island, year)
head(pen_num)

# run the pca

pen_pca = prcomp(pen_num, scale. = T, center = T)

pen_pca
str(pen_pca)

pen_pca$sdev

dim(pen_num)

head(pen_pca$x)


summary(pen_pca)$importance
summary(pen_pca)$importance[2,]


# calculate Proportion of varience from SDevs

(pen_pca$sdev)^2/sum(pen_pca$sdev^2)


# scree plot
 plot(pen_pca)

# only showing the standard deviation, we want the proportion varience

 pca_scree = data.frame(pc = c(1:4),
                        var = summary(pen_pca)$importance[2,])
 ggplot(aes(x=pc, y=var), data=pca_scree) +
   geom_col() +
   geom_point(size=4) +
   geom_line() +
   ggtitle("Scree plot") + ylab("Proportion variance explained") + xlab("PC") +
   theme_bw() 

# to visualize a pc, use a biplot 

head(pen_pca$x)

pen_pca_meta = cbind(pen_pca$x, pen_meta)

ggplot() +
  geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), data=pen_pca_meta) +
  coord_fixed(ratio=1) 

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

biplot(pen_pca) # points are row numbers
ggbiplot(pen_pca)


ggbiplot(pen_pca, scale = 1, groups = pen_meta$species, ellipse = T)

ggbiplot(pen_pca, scale = 1, groups = pen_meta$species, ellipse = T, alpha=0) + # use alpha=0 to hide the points
  geom_point(aes(color=pen_meta$species, shape=pen_meta$sex)) + # draw the points with specified shape
  # geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), data=pen_pca_meta)
  xlim(-3,3) + # might need to widen axis limits if arrow labels are getting cut off
  coord_fixed(ratio=1) + # keep ratio of x and y axis even to avoid biasing interpretation of PC1 and PC2
  theme_bw() 












