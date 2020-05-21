library(tidyverse)
library(tidymodels)
library(DBI)
library(vip)
library(RSQLite)
library(tictoc)


pbp_db <- tbl(DBI::dbConnect(RSQLite::SQLite(), "data/pbp_db.sqlite"), "pbp_clean_2000-2019")

tic()
raw_plays <- pbp_db %>% 
  select(game_id, game_date, game_seconds_remaining, season_type, week,season,
         play_type, yards_gained, ydstogo, down, yardline_100, qtr, posteam, posteam_score, defteam, 
         defteam_score,score_differential, shotgun,  no_huddle, 
         posteam_timeouts_remaining, defteam_timeouts_remaining, penalty, wp, goal_to_go, half_seconds_remaining) %>% 
  filter(play_type %in% c("run", "pass"), 
         penalty == 0,
         season_type == "REG", 
         !is.na(down),
         !is.na(yardline_100)) %>% 
  mutate(in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0),
         two_min_drill = if_else(half_seconds_remaining <= 120, 1, 0)) %>% 
  select(-penalty, -season_type, -half_seconds_remaining) %>% 
  collect()
toc()

tic()
all_plays <- raw_plays %>% 
  group_by(game_id, posteam) %>% 
  mutate(run = if_else(play_type == "run", 1, 0),
         pass = if_else(play_type == "pass", 1, 0),
         total_runs = if_else(play_type == "run", cumsum(run) - 1, cumsum(run)),
         total_pass = if_else(play_type == "pass", cumsum(pass) - 1, cumsum(pass)),
         previous_play = if_else(posteam == lag(posteam), 
                                 lag(play_type), "First play of Drive"),
         previous_play = if_else(is.na(previous_play), 
                                 replace_na("First play of Drive"), previous_play)) %>% 
  ungroup() %>% 
  mutate_at(vars(play_type, season, posteam, defteam, shotgun, down, qtr, no_huddle,
                 posteam_timeouts_remaining, defteam_timeouts_remaining, in_red_zone,
                 in_fg_range, previous_play, goal_to_go, two_min_drill), as.factor) %>% 
  select(-run, -pass)
toc()

skimr::skim(all_plays)
  
split_pbp <- initial_split(all_plays, 0.75, strata = play_type)

split_pbp

train_data <- training(split_pbp)
test_data <- testing(split_pbp)

train_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))

test_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))

pbp_rec <- recipe(play_type ~ ., data = train_data) %>% 
  update_role(game_id, game_date, yards_gained, new_role = "ID") %>% 
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>% 
  step_zv(all_predictors())

lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")
  
lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(pbp_rec)

tic()
pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data)
toc()

tic()
pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(play_type)) 
toc()

pbp_pred_lr %>% 
  roc_curve(truth = play_type, .pred_run) %>% 
  autoplot()

pbp_pred_lr %>% 
  roc_auc(truth = play_type, .pred_class)

pbp_pred_lr %>% 
  metrics(truth = play_type, .pred_class)


# RF Model ----------------------------------------------------------------

rf_mod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity", num.threads = 4) %>% 
  set_mode("classification")

rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(pbp_rec)

tic()
pbp_fit_rf <- rf_wflow %>% 
  fit(data = train_data)
toc()

tic()
pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(play_type, down, ydstogo)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))
toc()

pbp_pred_rf %>% 
  roc_curve(truth = play_type, .pred_pass) %>% 
  autoplot()

pbp_pred_rf %>% 
  roc_auc(truth = play_type, .pred_pass)

pbp_pred_rf %>% 
  accuracy(truth = play_type, .pred_class)

beepr::beep(3)
# Importance --------------------------------------------------------------

pbp_fit_rf %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 20)

roc_rf <- pbp_pred_rf %>% 
  roc_curve(truth = play_type, .pred_pass) %>% 
  mutate(model = "Random Forest")

roc_lr <- pbp_pred_lr %>% 
  roc_curve(truth = play_type, .pred_pass) %>% 
  mutate(model = "Logistic Regression")

bind_rows(roc_rf, roc_lr) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_manual(values = c("blue", "red"))

beepr::beep()



# Tune --------------------------------------------------------------------

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

tune_wf <- workflow() %>% 
  add_recipe(pbp_rec) %>% 
  add_model(tune_spec)

set.seed(37)
pbp_folds <- vfold_cv(train_data)

doParallel::registerDoParallel()

set.seed(37)
tune_res <- tune_grid(
  tune_wf,
  resamples = pbp_folds,
  grid = 20
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# Resample ----------------------------------------------------------------

folds <- vfold_cv(train_data, v = 10)

rf_wf_rs <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(pbp_rec)

set.seed(37)

rf_fit_rs <- 
  rf_wf_rs %>% 
  fit_resamples(folds)

collect_metrics(rf_fit_rs)


# RF Model ----------------------------------------------------------------



rf_mod <- rand_forest(trees = 100) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(pbp_rec)

pbp_fit_rf <- rf_wflow %>% 
  fit(data = train_data)

pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(play_type, down, ydstogo)) %>% 
  bind_cols(predict(pbp_fit_rf, train_data, type = "prob"))

pbp_pred_rf %>% 
  roc_curve(truth = play_type, .pred_class) %>% 
  autoplot()

pbp_pred_rf %>% 
  roc_auc(truth = play_type, .pred_class)

pbp_pred_rf %>% 
  accuracy(truth = play_type, .pred_class)


