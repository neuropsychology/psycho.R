library(caret)
library(tidyverse)
library(psycho)



# Regression ------------------------------------------------------------------


# Data ---------------------------------------------------------------

# Simulated
x <- rnorm(5000, 0, 1)
y <- (x)^3
y <- y + rnorm(length(x), mean=0, sd=3)
df <- data.frame(x=x, y=y)



# Partition ---------------------------------------------------------------


training <- psycho::emotion %>%
  select(-Participant_ID) %>%
  na.omit() %>%
  modelr::resample_partition(c(train = 0.7, test = 0.3))

test <- ungroup(as.data.frame(training$test))
training <- ungroup(as.data.frame(training$train))



# Model -------------------------------------------------------------------

# ML
model <- caret::train(Subjective_Arousal ~ .,
                      data=training,
                      method = "rf",
                      trControl=caret::trainControl(method="repeatedcv",
                                                    number=10,
                                                    repeats=3))
print(model)
varImp(model, scale = TRUE)

# Visualize
newdata <- psycho::emotion %>%
  select(-Participant_ID) %>%
  psycho::refdata(target="Subjective_Valence", length.out=100)

newdata$Predicted <- predict(model, newdata)
ggplot(newdata, aes(x=Subjective_Valence, y=Predicted)) +
  geom_point(data=training, aes(x=Subjective_Valence, y=Subjective_Arousal)) +
  geom_line(colour="red", size=2)


# test
newdata <- cbind(newdata, predict(model, newdata, type = "prob"))
test %>%
  select(pred=Predicted, obs=Subjective_Arousal) %>%
  defaultSummary()

ggplot(test, aes(x=Subjective_Arousal, y=Predicted)) +
  geom_point() +
  geom_smooth()






# ================================================================================
# Classification -----------------------------------------------------------------

# data
df <- psycho::emotion %>%
  mutate(Recall = as.factor(Recall)) %>%
  na.omit() %>%
  select(-Participant_ID)

training <- df %>%
  group_by(Recall) %>%
  modelr::resample_partition(c(train = 0.7, test = 0.3))

test <- ungroup(as.data.frame(training$test))
training <- ungroup(as.data.frame(training$train))

# Model
# ML
model <- caret::train(Recall ~ .,
                      data=training,
                      method = "rf",
                      trControl=caret::trainControl(method="repeatedcv",
                                                    number=10,
                                                    repeats=3))
print(model)
varImp(model, scale = TRUE)

# Visualize
newdata <- training %>%
  psycho::refdata(target="Subjective_Valence", length.out=100)

newdata <- cbind(newdata, predict(model, newdata, type = "prob"))
ggplot(newdata, aes(x=Subjective_Valence, y=`TRUE`)) +
  # geom_point(data=training, aes(x=Subjective_Valence, y=Recall)) +
  geom_line(colour="red", size=2)


# test
test$Predicted <- predict(model, test)
test %>%
  select(pred=Predicted, obs=Recall) %>%
  as.data.frame() %>%
  defaultSummary()

