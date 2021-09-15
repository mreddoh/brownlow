
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
# Subset to potentially predictive variables, found in previous step ----
model_vars <- c(
  #"match_weather_type",
  "player_position",
  "match_pct.goals",
  "match_pct.disposals",
  "match_pct.score_involvements",
  "match_pct.shots_at_goal",
  "match_pct.contested_possessions",
  "match_pct.inside_fifties",
  #"team_pct.intercepts",
  #"match_pct.kicks",
  "match_pct.handballs",
  "match_pct.clangers",
  "match_pct.tackles",
  "team_pct.metres_gained",
  "match_pct.contested_marks",
  # add new variables 
  "marks_inside_fifty",
  "match_pct.hitouts_to_advantage",
  "match_pct.ground_ball_gets",
  # more new variables
  "team_result",
  "match_pct.goal_assists",
  # and even more
  "team.pressure_acts",
  "hitout_win_percentage",
  "team_pct.intercept_marks",
  "match_pct.effective_kicks",
  "match_pct.goal_assists",
  "match_pct.score_launches",
  "match_pct.clearances"
)

ds_in <- player_data_full.cleaned %>%
  select(id, all_of(model_vars), brownlow_votes)

set.seed(3084)

## * Split into training and testing datasets ----
ds_split <- rsample::initial_split(data = ds_in, prop = 0.3, strata = brownlow_votes)

## * Use the recipes package to define these preprocessing steps, in what is called a “recipe” ----
preprocessing_recipe <- recipes::recipe(brownlow_votes ~ ., data = training(ds_split)) %>%
  recipes::step_string2factor(all_nominal()) %>% # convert categorical variables to factors
  recipes::step_other(all_nominal(), threshold = 0.01) %>% # combine low frequency factor levels
  recipes::step_rm(id) %>% # remove id variable
  recipes::step_nzv(all_nominal()) %>% # remove no variance predictors which provide no predictive information 
  prep()

save(preprocessing_recipe, file = here("output","preprocessing_recipe.RData"))

## * Apply our previously defined preprocessing recipe with bake() ----
ds_cv_folds <- recipes::bake(preprocessing_recipe, new_data = training(ds_split)) %>%  
  rsample::vfold_cv(v = 3)

## * Use the parsnip package to define the XGBoost model specification ----
xgboost_model <- parsnip::boost_tree(mode = "regression",
                                     trees = 500,
                                     min_n = tune(),
                                     tree_depth = tune(),
                                     learn_rate = tune(),
                                     loss_reduction = tune()) %>%
  set_engine("xgboost", objective = "reg:squarederror")

## * Use the tidymodel dials package to specify the parameter set ----
xgboost_params <- dials::parameters(#trees(range = c(100,1000)),
                                    min_n(range = c(15,45)),
                                    tree_depth(range = c(3,6)),
                                    learn_rate(),
                                    loss_reduction())

xgboost_grid <- dials::grid_max_entropy(xgboost_params, size = 20)

head(xgboost_grid)

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
  tune::show_best(metric = "rmse", n = 20)

 xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("rmse")

# xgboost_best_params <- xgboost_tuned %>%
#   tune::show_best(metric = "rmse", n = 20) %>%
#   filter(row_number()==3)

xgboost_best_params

xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

## * Evaluate performance on test data ----

# check on training
train_processed <- bake(preprocessing_recipe, new_data = training(ds_split))

final_model_fit <- xgboost_model_final %>%
  fit(formula = brownlow_votes ~ ., data = train_processed) 

train_prediction <- final_model_fit %>%
  predict(new_data = train_processed) %>%
  bind_cols(training(ds_split))

train_prediction %>%
  yardstick::metrics(brownlow_votes, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

version = list.files(here("output")) %>% 
  tibble() %>% 
  filter(substr(.,1,1)=="v") %>% 
  mutate(value = as.numeric(substr(.,2,5))) %>% 
  summarise(max = paste0("v",format(max(value) + 0.01, nsmall = 2),"_model_gbm.RData")) %>% 
  pull(max)

save(final_model_fit, file = here("output",version))


# check on test
test_processed  <- bake(preprocessing_recipe, new_data = testing(ds_split))

test_prediction <- final_model_fit %>%
  predict(new_data = test_processed) %>%
  bind_cols(testing(ds_split))

test_prediction %>%
  yardstick::metrics(brownlow_votes, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))


# check that there is not an obvious issue with our model’s predictions, so plot the test data residuals
test_prediction %>%
  arrange(.pred) %>%
  mutate(residual_pct = (brownlow_votes - .pred) / .pred) %>%
  select(.pred, residual_pct) %>% 
  ggplot(., aes(x = .pred, y = residual_pct)) +
  geom_point() +
  xlab("Predicted Votes") +
  ylab("Residual (%)") +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::percent)

# plot variable importance
final_model_fit %>% vip:::vip(., num_features = 30)

# SHAP values!


# Merge predictions back onto original dataset ----
full_processed <- bake(preprocessing_recipe, new_data = ds_in)

full_prediction <- predict(object = final_model_fit, new_data = full_processed) %>%
  bind_cols(ds_in)

full_out <- full_prediction %>%
  select(id,.pred) %>%
  rename(predicted_votes_raw = .pred) %>% 
  left_join(.,player_data_full.cleaned,by=c("id")) %>% 
  select(id:player_position) %>% 
  mutate(name = paste(player_first_name,player_last_name,sep=" "),
         season = substr(match_date,1,4))


# Calculate Votes ----

check <- full_out %>% 
  group_by(match_id) %>% 
  mutate(votes = rank(-predicted_votes_raw, ties.method = "random")) %>%
  mutate(votes = case_when(votes == 1 ~ 3, votes == 2 ~ 2, votes == 3 ~ 1, TRUE ~ 0)) %>% 
  select(match_id,season,match_round,name,predicted_votes_raw,votes,brownlow_votes)

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

## * Check Fyfe ----
brownlow_votes %>% 
  filter(season==2019) %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>%
  arrange(-medal_tally)

brownlow_votes %>% 
  filter(season==2015) %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>%
  arrange(-medal_tally)

brownlow_votes %>% 
  filter(name == "Nat Fyfe") %>% 
  group_by(season, name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  filter(medal_tally == max(medal_tally)) %>% 
  arrange(season)

## * Check the Neale effect ----
brownlow_votes %>% 
  filter(name == "Lachie Neale") %>% 
  group_by(season, name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  filter(medal_tally == max(medal_tally)) %>% 
  arrange(season)

## * Check 2020 ----
brownlow_votes %>% 
  filter(season==2020) %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>%
  arrange(-medal_tally)
