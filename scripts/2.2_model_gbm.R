
# Load packages ----
library(tidyverse)
library(here)
library(tidymodels)
library(doParallel)
library(xgboost)
library(vip)

# speed up computation with parallel processing
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Load data ----
load(file = here("data","player_data_full.cleaned.Rdata"))

# Begin modeling ----

## * Prepare dataset specifically for modeling using gradient boosting algorithms ----
ds_in <- player_data_full.cleaned %>% 
  mutate(id = row_number()) %>% 
  select(id, player_position, team_pct.kicks:match_pct.spoils, brownlow_votes) %>% 
  select(-c(team_pct.brownlow_votes,match_pct.brownlow_votes))

## * Split into training and testing datasets ----
ds_split <- rsample::initial_split(data = ds_in, prop = 0.2, strata = brownlow_votes)

## * Use the recipes package to define these preprocessing steps, in what is called a “recipe” ----
preprocessing_recipe <- recipes::recipe(brownlow_votes ~ ., data = training(ds_split)) %>%
  recipes::step_string2factor(all_nominal()) %>% # convert categorical variables to factors
  recipes::step_other(all_nominal(), threshold = 0.01) %>% # combine low frequency factor levels
  recipes::step_nzv(all_nominal()) %>% # remove no variance predictors which provide no predictive information 
  prep()

## * Apply our previously defined preprocessing recipe with bake() ----
ds_cv_folds <- recipes::bake(preprocessing_recipe, new_data = training(ds_split)) %>%  
  rsample::vfold_cv(v = 3)

## * Use the parsnip package to define the XGBoost model specification ----
xgboost_model <- parsnip::boost_tree(mode = "regression",
                                     trees = 100,
                                     min_n = tune(),
                                     tree_depth = tune(),
                                     learn_rate = tune(),
                                     loss_reduction = tune()) %>%
  set_engine("xgboost", objective = "reg:squarederror")

## * Use the tidymodel dials package to specify the parameter set ----
xgboost_params <- dials::parameters(min_n(),
                                    tree_depth(),
                                    learn_rate(),
                                    loss_reduction())

xgboost_grid <- dials::grid_max_entropy(xgboost_params, size = 20)

knitr::kable(head(xgboost_grid))

## * Use the new tidymodel workflows package to add a formula to our XGBoost model specification ----
xgboost_wf <- workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(brownlow_votes ~ .)

## * Tune the model! ----

# hyperparameter tuning
xgboost_tuned <- tune::tune_grid(object = xgboost_wf,
                                 resamples = ds_cv_folds,
                                 grid = xgboost_grid,
                                 metrics = yardstick::metric_set(rmse, rsq, mae),
                                 control = tune::control_grid(verbose = TRUE))

xgboost_tuned %>%
  tune::show_best(metric = "rmse") %>%
  knitr::kable()

xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("rmse")

knitr::kable(xgboost_best_params)

xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

## * Evaluate performance on test data ----

# check on training
train_processed <- bake(preprocessing_recipe, new_data = training(ds_split))

train_prediction <- xgboost_model_final %>%
  fit(formula = brownlow_votes ~ ., data = train_processed) %>%
  predict(new_data = train_processed) %>%
  bind_cols(training(ds_split))

xgboost_score_train <- train_prediction %>%
  yardstick::metrics(brownlow_votes, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

knitr::kable(xgboost_score_train)

# check on test
test_processed  <- bake(preprocessing_recipe, new_data = testing(ds_split))

test_prediction <- xgboost_model_final %>%
  fit(formula = brownlow_votes ~ ., data = train_processed) %>%
  predict(new_data = test_processed) %>%
  bind_cols(testing(ds_split))

xgboost_score_test <- test_prediction %>%
  yardstick::metrics(brownlow_votes, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

knitr::kable(xgboost_score_test)

# check that there is not an obvious issue with our model’s predictions, so plot the test data residuals
vote_prediction_residual <- test_prediction %>%
  arrange(.pred) %>%
  mutate(residual_pct = (brownlow_votes - .pred) / .pred) %>%
  select(.pred, residual_pct)

ggplot(vote_prediction_residual, aes(x = .pred, y = residual_pct)) +
  geom_point() +
  xlab("Predicted Votes") +
  ylab("Residual (%)") +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::percent)

# create model object
model_obj <- xgboost_model_final %>%
  fit(formula = brownlow_votes ~ ., data = train_processed) 

# plot variable importance
model_obj %>% vip:::vi(.)

train_prediction %>%
  select(player_position, .pred) %>% 
  ggplot(., aes(x=player_position, y=.pred)) + geom_boxplot()

train_prediction %>%
  select(match_pct.afl_fantasy_score, .pred) %>% 
  ggplot(., aes(x=match_pct.afl_fantasy_score, y=.pred)) + geom_point()



# Merge predictions back onto original dataset ----
full_processed <- bake(preprocessing_recipe, new_data = ds_in)

full_prediction <- xgboost_model_final %>%
  fit(formula = brownlow_votes ~ ., data = full_processed) %>%
  predict(new_data = full_processed) %>%
  bind_cols(ds_in)

full_out <- full_prediction %>%
  select(id,.pred) %>%
  rename(predicted_votes_raw = .pred) %>% 
  left_join(.,(player_data_full.cleaned %>% mutate(id = row_number())),by=c("id")) %>% 
  select(id:player_position) %>% 
  mutate(name = paste(player_first_name,player_last_name,sep=" "),
         season = substr(match_date,1,4))

# Calculate Votes ----

## * Apply votes using dplyr method ----
brownlow_votes <- full_out %>% 
  group_by(match_id) %>% 
  mutate(votes = rank(-predicted_votes_raw, ties.method = "random")) %>% 
  filter(votes<=3) %>% 
  mutate(votes = rank(-votes, ties.method = "random")) %>% 
  ungroup() %>% 
  select(match_id,season,name,votes)

## * Check Brownlow Medal Tally ----
brownlow_votes %>% 
  group_by(season, name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  filter(medal_tally == max(medal_tally)) %>% 
  arrange(season)
  
