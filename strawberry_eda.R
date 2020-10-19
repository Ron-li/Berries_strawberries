library(knitr)
library(ggplot2)
library(tidyverse)

df <- read.csv("/Users/amelia/Documents/mssp/MA615/Hw_berries/Berries_strawberries/data/strawberries.csv")
df %<>% filter(is.na(Value) == "FALSE")

df %>% group_by(State) %>% summarize(total=sum(Value)) -> t1
t1

df %>% group_by(Year) %>% summarize(total=sum(Value)) -> t2
t2

# boxplot of different states
bp1 <- ggplot(df, aes(x = State, y = Value))
bp1 <- bp1 + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(x = "Different states")
bp1

# excluding outliers
bp2 <- ggplot(df, aes(x = State, y = Value))
bp2 <- bp2 + geom_boxplot(outlier.colour = NA) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  coord_cartesian(ylim = c(0, 8e+6)) +
  labs(x = "Different states")
bp2

# boxplot of different years
bp3 <- ggplot(df, aes(x = factor(Year), y = Value))
bp3 <- bp3 + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(x = "Different years")
bp3

# excluding outliers
bp4 <- ggplot(df, aes(x = factor(Year), y = Value))
bp4 <- bp4 + geom_boxplot(outlier.colour = NA) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  coord_cartesian(ylim = c(0, 6e+6)) +
  labs(x = "Different years")
bp4








