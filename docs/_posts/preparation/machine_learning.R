library(caret)
library(tidyverse)




# Regression ------------------------------------------------------------------


# Data ---------------------------------------------------------------

# Simulated
x <- rnorm(5000, 0, 1)
y <- (x)^3
y <- y + rnorm(length(x), mean=0, sd=3)
df <- data.frame(x=x, y=y)



# Real
df <- psycho::emotion %>%
  mutate(x=Subjective_Valence,
         y=Subjective_Arousal)




ggplot(df, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth()


# Partition ---------------------------------------------------------------


training <- df %>%
  modelr::resample_partition(c(train = 0.7, test = 0.3))

test <- ungroup(as.data.frame(training$test))
training <- ungroup(as.data.frame(training$train))



# Model -------------------------------------------------------------------

# ML
model <- caret::train(y ~ x,
                      data=training,
                      method = "rf",
                      trControl=caret::trainControl(method="repeatedcv",
                                                    number=10,
                                                    repeats=3))
print(model)

# Visualize
newdata = data.frame(x = seq(min(df$x),
                             max(df$x),
                             length.out=100))

newdata$y <- predict(model, newdata)
ggplot(newdata, aes(x=x, y=y)) +
  geom_point(data=df, aes(x=x, y=y)) +
  geom_line(colour="red")


# test
test$predicted_y <- predict(model, test)
ggplot(test, aes(x=y, y=predicted_y)) +
  geom_point() +
  geom_smooth()



# ================================================================================
# Classification -----------------------------------------------------------------

# data
df <- psycho::emotion %>%
  mutate(Recall = as.numeric(Recall)) %>%
  na.omit() %>%
  select(-Participant_ID, -Item_Name)

training <- df %>%
  group_by(Recall) %>%
  modelr::resample_partition(c(train = 0.7, test = 0.3))

test <- ungroup(as.data.frame(training$test))
training <- ungroup(as.data.frame(training$train))

# Model
# ML
model <- caret::train(Recall ~ .,
                      data=training,
                      method = "naive_bayes",
                      trControl=caret::trainControl(method="repeatedcv",
                                                    number=10,
                                                    repeats=3))
print(model)
varImp(model, scale = TRUE)

newdata <- refgrid(df, "Trial_Order", length.out=10, type="reference")
newdata <- cbind(newdata, predict(model, newdata, type = "prob"))
ggplot(newdata, aes(x=Trial_Order, y=`TRUE`)) +
  geom_line(colour="red")


refgrid_var(x, length.out=10, varname=NULL){

  factors_df <- tidyr::expand_(df, factors_name)

  if(is.numeric(x)){
      out <- data.frame(seq(min(x),
                               max(x),
                               length.out = length.out))
      names(out) <- varname
  } else if(is.factor(x)){
    out <- levels(x)[1]
  } else{
    warning("Argument is not numeric nor factor: returning NA.")
    out <- NA
  }
}



refgrid <- function(df, at, length.out=10, type="combinations", fixed="mean"){
  # Target
  target <- data.frame(seq(min(df[at]),
                           max(df[at]),
                           length.out = length.out))
  names(target) <- at

  # Rest
  df <- select_(df, paste0("-", at))
  if(type == "reference"){
    smart_mean <- function(x){
      if(is.numeric(x)){
        out <- mean(x, na.rm=TRUE)
      } else if(is.factor(x)){
        out <- levels(x)[1]
      } else{
        warning("Argument is not numeric nor factor: returning NA.")
        out <- NA
      }
      return(out)
    }

    refgrid <- df %>%
      summarise_all(smart_mean)
  } else{
    var_order <- names(df)
    factors <- purrr::keep(df, is.factor)
    factors_name <- names(factors)

    nums <- purrr::keep(df, is.numeric) %>%
      summarise_all(funs_(fixed))

    factors_df <- tidyr::expand_(df, factors_name)
    refgrid <- merge(factors_df, nums)
    refgrid <- refgrid[var_order]
  }

  # Join
  refgrid <- merge(target, refgrid)

  return(refgrid)
}
