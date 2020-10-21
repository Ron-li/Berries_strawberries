library(knitr)
library(ggplot2)
library(tidyverse)
library(magrittr)

sberry <- read.csv("/Users/amelia/Documents/mssp/MA615/Hw_berries/Berries_strawberries/data/strawberries.csv")
df <- filter(sberry, is.na(Value) == "FALSE")

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

# prepare data for pca
# load data
df_value <- filter(sberry, 
                Year%in%c('2019','2018'), 
                Measures=='MEASURED IN LB', 
                Materials%in%c('(TOTAL)','')
                )

# in the rear of the data, it lacks a row of (SULFUR) FERTILIZER, just add it.
df_value <- rbind(df_value, df_value[nrow(df_value), ])
df_value$Value[nrow(df_value)] <- NA
df_value$Chemical[nrow(df_value)] <- '(SULFUR) FERTILIZER'

# arrange data
df_value$MC <- paste(df_value$Materials, df_value$Chemical)
df_value <- arrange(df_value, df_value$Year, df_value$State, df_value$MC)

# handle missing value
for(i in unique(df_value$MC)){
  m <- df_value$Value[df_value$Value!=' (D)' & 
                        !is.na(df_value$Value) & 
                        df_value$MC==i] %>% 
    str_replace_all(c(','='')) %>% 
    as.numeric() %>% 
    mean()
  df_value$Value[(df_value$Value==' (D)' | 
                    is.na(df_value$Value)) & 
                   df_value$MC==i] <- m
  
}

# transform for PCA
j <- 1
for(i in unique(df_value$MC)){
  if(j==1) df_pca <- df_value$Value[df_value$MC==i]
  else df_pca <- cbind(df_pca, df_value$Value[df_value$MC==i])
  j <- 0
}
colnames(df_pca) <- unique(df_value$MC)
head(df_pca)

# start pca
pca <- prcomp(df_pca, center = T, scale. = T)
summary(pca)
print(pca)
biplot(pca, scale = 0)




