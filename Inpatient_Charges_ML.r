# Tristan Kaiser
# InpatientCharges ML Practice
# 10/27/2016

library(tidyverse)
#library(magrittr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

read_csv("inpatientCharges.csv") %>%
  setNames(tolower(names(.))) %>%
  setNames(gsub(" ", ".", names(.), fixed = TRUE)) %>%
  mutate_at(vars(dplyr::contains("average")), funs(as.numeric(gsub("[$]", "", .)))) %>%
  mutate_if(is.character, as.factor) -> data

set.seed(1)
# Shuffle the dataset, call the result shuffled
n <- nrow(data)
shuffled <- data[sample(n),]

# Split the data in train and test

# Modify (0.1 * n) to select % of dataset to test
train_indices <- 1:round(0.6 * n)
train <- shuffled[train_indices, ]

# Run ML
train %>%
  select(provider.state, average.total.payments, average.medicare.payments, total.discharges) -> rpartTrain
tree <- rpart(total.discharges ~ ., rpartTrain[1:1000,], method = "anova", control = rpart.control(cp=0.01))
fancyRpartPlot(tree)

# testData <- data[-train,]
# test_indices <- (round(0.3 * n) + 1):n
# test <- shuffled[test_indices, ]

##PLOTS##################
library(ggplot2);library(hexbin)
#shows the relationship between average total payments 
ggplot(data, aes(average.total.payments,average.covered.charges)) + 
  stat_bin_hex(bins=100) + facet_wrap(~provider.state, scales = "free")

##UNSUPERVISED K-MEANS CLUSTERING#######
data %>%
  sample_n(1000) %>%
  dist %>%
  hclust(.) %>%
  plot

data %>%
  group_by(drg.definition) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(20) %>%
  select(drg.definition) %>% unlist -> drg.definition.c
  
data %>%
  filter(drg.definition %in% drg.definition.c) %>%
  ggplot(aes(average.medicare.payments, total.discharges)) +
    stat_bin_hex(bins=100) + facet_wrap(~drg.definition, scales = "free")

library(PerformanceAnalytics)
data %>%
  filter(drg.definition %in% drg.definition.c) %>%
  select_if(is.numeric) %>%
  cor %>%
  chart.Correlation
  