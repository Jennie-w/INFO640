install.packages("broom")

library(tidyverse)
library(lubridate)
library(broom)

?trees

data(trees)
glimpse(trees)

ggplot(trees, aes(x=trees$Girth, y=trees$Height)) +
  geom_point() +labs(title = "Tree height by girth")

ggplot(trees, aes(x=trees$Girth, y=trees$Height)) +
  geom_point() +
  stat_smooth(method = "lm", se=FALSE) +
  labs(title = "Tree height by girth")

lm_trees <- lm(Height ~ Girth, data = trees)
lm_trees

summary(lm_trees)

coef(lm_trees)

fitted_trees <- fitted.values(lm_trees)
fitted_trees

residual_trees <- residuals(lm_trees)
residual_trees

lm_matrix_trees <- broom::augment(lm_trees)
head(lm_matrix_trees)

lm_matrix_trees %>%
  arrange(desc(.resid)) %>%
  head()

lm_matrix_trees$.resid_abs <- abs(lm_matrix_trees$.resid)
lm_matrix_trees %>%
  arrange(desc(.resid_abs)) %>%
  head()

trees %>%
  filter(Girth == 13.8)

new_trees <- data.frame("Girth" = 19.0)
predict(lm_trees, newdata = new_trees)

mytree <- broom::augment(lm_trees, newdata=new_trees)
mytree

ggplot(data=trees, aes(x=Girth, y=Height))+
  geom_point() +
  stat_smooth(method="lm") +
  geom_point(data = mytree, aes(y=.fitted), size =3, color="red") +
  labs("Tree height by girth")

tree_null <- lm(Height ~ 1, data=trees)
tree_null

mean(trees$Height)

ggplot(data = trees, aes(x=Girth, y=Height)) +
  geom_point() +
  geom_hline(yintercept = 76) +
  labs(title = "Tree height with null model")

ggplot(data = trees, aes(x=Girth, y=Height)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title = "Tree height with null model")

summary(lm_trees)

data(longley)
?longley

install.packages("GGally")
library(GGally)

ggpairs(data = trees, columns= 1:3)