# Loading libraries
library(ggplot2)
library(agricolae)

rm(list = ls()) # Cleaning my global environment

################################################################################
## Analysis of experimental data with R: Example of a 2x3 factorial organized
## in a Randomized Complete Block Design (RCBD) with no significant interactions

## Treatments are herbicide application rate [0, 4 and 8 lb/acre] and
## delay in cultivation after spray [3 or 10 days]

## Response variable is square root of the number of quack-grass shoots
## per square foot after spraying with herbicide (maleic hydrazide).
################################################################################


################################################################################
## STEP 1: CREATING DATAFRAME WITH EXPERIMENT DATA
################################################################################

# Treatments
delay <- c(3,3,3,3,3,3,3,3,3,3,3,3,10,10,10,10,10,10,10,10,10,10,10,10)
rate <- c(0,4,8,0,4,8,0,4,8,0,4,8,0,4,8,0,4,8,0,4,8,0,4,8)

# Blocks
block <- c(1,1,1,2,2,2,3,3,3,4,4,4,1,1,1,2,2,2,3,3,3,4,4,4)

# Response variable
shoot_density <- c(15.7,9.8,7.9,14.6,14.6,10.3,16.5,11.9,9.7,14.7,12.4,9.6,
                   18.0,13.6,8.8,17.4,10.6,8.2,15.1,11.8,11.3,14.4,13.3,11.2)

# Creating data frame
mh_df <- data.frame(delay, rate, block, shoot_density)

# Converting 'rate', 'delay' and 'block' from numeric variable to factor variable
mh_df$rate <- as.factor(mh_df$rate)
mh_df$delay <- as.factor(mh_df$delay)
mh_df$block <- as.factor(mh_df$block)


print(mh_df)

################################################################################
## STEP 2: DATA VISUALISATION
################################################################################

# Create a scatterplot

scatterpl <- ggplot(mh_df, aes(x= rate, y= shoot_density, colour= delay)) +
  geom_point() +
  scale_color_brewer(palette = "Set2")

scatterpl +
  ggtitle("Quack-grass shoot density after maleic hydrazide application") +
  xlab("Herbicide application rate (lb/acre)") +
  ylab("Grass shoot density\n(square root of shoots/sq ft)") +
  labs(colour = "Delay in\ncultivation\nafter spray\n(days)") +
  theme_classic()

################################################################################
## STEP 3: ANALYSIS OF VARIANCE WITH R PACKAGE AGRICOLAE
################################################################################

anova_model <- aov(shoot_density ~ block + rate*delay, data=mh_df)

summary(anova_model)

mean_comparison <- LSD.test(anova_model, "rate", console=TRUE)








