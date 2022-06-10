library(tidyverse)
library(magrittr)
library(DataExplorer)
library(funModeling)
library(aod)
library(ggthemes)


## Import data
training <- read_csv("train.csv")

## Cleaning
factor_cols <- c("Pclass", "Sex", "Embarked", "Survived")
training %<>% mutate(across(all_of(factor_cols), as_factor))
levels(training$Pclass) <- c("First", "Second", "Third")
levels(training$Sex) <- c("Male", "Female")
levels(training$Survived) <- c("No", "Yes")
levels(training$Embarked) <- c("Southampton", "Cherbourg", "Queenstown")



## Numerical Variables
## Age
training %>%
        ggplot(aes(Age)) +
        geom_histogram(binwidth = 4, color = "black", fill = "grey") +
        geom_vline(xintercept = median(training$Age, na.rm = TRUE),
                   color = "red") +
        geom_rug(alpha = 0.2) +
        labs(title = "Age Distribution",
             x = "Age",
             y = "Number of Passengers") +
        theme_minimal()

## SibSp
training %>%
        ggplot(aes(SibSp)) +
        geom_histogram(binwidth = 1, color = "black", fill = "grey") +
        geom_vline(xintercept = median(training$SibSp, na.rm = TRUE)) +
        geom_rug(alpha = 0.2) +
        labs(title = "Siblings/Spouses Distribution",
             x = "Number of Siblings/Spouses Aboard",
             y = "Number of passengers") +
        theme_minimal()

## Parch
training %>%
        ggplot(aes(Parch)) +
        geom_histogram(bins = 7, color = "black", fill = "grey", stat = "count") +
        geom_vline(xintercept = median(training$Parch)) +
        geom_rug(alpha = 0.2) +
        labs(title = "Parch Distribution",
             x = "Number of Parents and Children Aboard",
             y = "Number of Passengers") +
        theme_minimal()

## Fare
training %>%
        filter(Fare < 300) %>%
        ggplot(aes(x = Fare)) +
        facet_grid(rows = vars(Pclass)) +
        geom_histogram(binwidth = 10, color = "black", fill = "grey") +
        geom_vline(xintercept = median(training$Fare)) +
        geom_rug(alpha = 0.2) +
        labs(title = "Fare Distribution",
             x = "Fare ($)",
             y = "Number of Passengers") +
        theme_minimal()




## Discrete Variables
## Pclass
training %>%
        ggplot(aes(Pclass)) +
        geom_bar() +
        labs(title = "Pclass Distribution",
             x = "Pclass",
             y = "Number of Passengers") +
        theme_minimal()

## Survived
training %>%
        ggplot(aes(Survived)) +
        geom_bar() +
        labs(title = "Survived Distribution",
             x = "Survived",
             y = "Number of Passengers") +
        theme_minimal()

## Sex
training %>%
        ggplot(aes(Sex)) +
        geom_bar() +
        labs(title = "Sex Distribution",
             x = "Sex",
             y = "Number of Passengers") +
        theme_minimal()

## Embarked
training %>%
        ggplot(aes(Embarked)) +
        geom_bar() +
        labs(title = "Embarked Distribution",
             x = "Embarked",
             y = "Number of Passengers") +
        theme_minimal()

## Bivariate Analysis

## Survived ~ Pclass
table(training$Survived, training$Pclass)
training %>%
        ggplot(aes(Survived)) +
        geom_bar() +
        facet_grid(Pclass ~ Embarked)

## Survived ~ Embarked
table(train$Survived, train$Embarked)
train %>%
        ggplot(aes(Embarked, Survived)) +
        geom_count() +
        theme_minimal()




ggplot(training, aes(x = Fare, y = as.integer(Survived) - 1)) +
        geom_point() +
        stat_smooth(method = "glm", 
                    se = FALSE, 
                    method.args = list(family = "binomial"))