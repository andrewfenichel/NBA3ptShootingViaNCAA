#Predicting NBA 3pt% Using College Stats

#Loading Packages
library(tidyverse)
library(skimr)
library(janitor)
library(rsample)
library(GGally)
library(glmnet)
library(modelr)
library(ranger)
library(vip)
library(pdp)
library(xgboost)
library(MASS)
library(tidyselect)

#Set the Seed
set.seed(3739)

#Loading Unproccessed Data
players_dat_uncleaned <- read_csv(file = "Data/Unproccessed/players.csv") %>%
  clean_names()

#Stage 1: Data Cleaning and Processing
players_dat <- players_dat %>%
  mutate(nba_career_length = (active_to - active_from) +1)

players_dat <- players_dat %>%
  mutate(nba_career_3ptatt = (nba_3ptapg * nba_g_played))

players_dat <- players_dat %>%
  filter(nba_career_3ptatt > 25)

players_dat <- players_dat %>%
  mutate(elite_3pt_shooter = case_when(
    nba_3ptpct >= 0.4 ~ 1,
    TRUE ~ 0))

players_dat <- players_dat %>%
  mutate(good_3pt_shooter = case_when(
    nba_3ptpct >= 0.33 ~ 1,
    TRUE ~ 0))

players_dat <- players_dat %>%
  dplyr::select(c(name, position, nba_3ptpct, ncaa_3ptapg, ncaa_3ptpct, ncaa_3ptpg, ncaa_fgapg, ncaa_fgpct, 
           ncaa_fgpg, ncaa_ft, ncaa_ftapg, ncaa_ftpg, elite_3pt_shooter, good_3pt_shooter))

players_dat %>%
  skim_without_charts()

# sum(is.na(players_dat))

players_dat <- players_dat %>%
  na.omit(players_dat, na.action = "omit")

# sum(is.na(players_dat))

players_dat <- players_dat %>%
  filter(position == "G" | position == "F" | position == "G-F" | position == "F-G")

players_dat$name <- as_factor(players_dat$name)
players_dat$position <- as_factor(players_dat$position)
# write_csv(players_dat, path = "Data/Kernal Data")

#Stage 3: The Second Random Data Split (Splitting Analysis Dataset)
players_dat$id <- 1:nrow(players_dat)
train <- players_dat %>% sample_frac(.75)
test  <- anti_join(players_dat, train, by = 'id')

players_dat_split <- tibble(
  train = train %>% list(),
  test = test %>% list()
)


#Stage 4: Exploratory Data Analysis

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_ft, nba_3ptpct), alpha = 0.6) # FT% vs NBA 3%, the classic predictor

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_ft, nba_3ptpct, color = position)) +
  geom_smooth(mapping = aes(ncaa_ft, nba_3ptpct), color = 'black')

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_ppg, nba_3ptpct, color = position))

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_ftpg, nba_3ptpct, color = position))

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_fgpct, nba_3ptpct, color = position))

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_3ptpct, nba_3ptpct, color = position)) # NCAA 3% vs NBA 3%, the logical predictor

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_3ptpct, nba_3ptpct), alpha = 0.2) +
  geom_smooth(mapping = aes(ncaa_3ptpct, nba_3ptpct), size = 1)

ggplot(data = players_dat) +
  geom_point(mapping = aes(position, nba_3ptpct))

ggplot(data = players_dat) +
  geom_boxplot(mapping = aes(position, nba_3ptpct))

ggplot(data = players_dat) +
  geom_boxplot(mapping = aes(position, nba_3ptpct))+
  ggtitle("Position X NBA 3PT %") +
  scale_y_continuous(limits = c(0, 0.6))

ggplot(data = players_dat) +
  geom_point(mapping = aes(nba_career_3ptatt, nba_3ptpct))

ggplot(data = players_dat) +
  geom_point(mapping = aes(nba_3ptapg, nba_3ptpct))

ggplot(data = players_dat) +
  geom_point(mapping =  aes(ncaa_3ptpg, nba_3ptpct))

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_3ptapg, nba_3ptpct))

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_ft, nba_3ptpct)) +
  geom_smooth(mapping = aes(ncaa_ft, nba_3ptpct))

players_dat %>% 
  select(nba_3ptpct, ncaa_3ptapg, ncaa_3ptpct, ncaa_3ptpg, ncaa_fgapg, ncaa_fgpct, ncaa_fgpg, ncaa_ft, ncaa_ftapg, ncaa_ftpg, ncaa_ppg, ncaa_games) %>%
  cor() %>% 
  corrplot::corrplot()

ggplot(data = players_dat) +
  geom_point(mapping = aes(ncaa_ft, nba_3ptpct)) +
  facet_wrap('position')

players_dat %>%
  ggplot(aes(ncaa_ft, nba_3ptpct)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(
    x = 'NCAA FT%',
    y = "NBA 3pt%",
    title = "Predicting NBA 3pt% by NCAA FT%"
  ) +
  ggthemes::theme_tufte()  +
  theme(plot.title = element_text(hjust = 0.5))

players_dat %>%
  filter(ncaa_3ptpct < 0.6) %>%
  ggplot(aes(ncaa_3ptpct, nba_3ptpct)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(
    x = 'NCAA 3pt%',
    y = "NBA 3pt%",
    title = "Predicting NBA 3pt% by NCAA 3pt%"
  ) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5))

players_dat %>%
  ggplot(aes(ncaa_3ptpg, nba_3ptpct)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(
    x = 'NCAA 3 Pointers Made / Game',
    y = "NBA 3pt%",
    title = "Predicting NBA 3pt% by NCAA 3s Made / Game"
  ) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5))


#Stage 5: Model Building and Selection
#Simple Linear Model
lm_fit_ncaaft <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_ft)
lm_fit_ncaaft %>%
  broom::glance()
modelr::mse(lm_fit_ncaaft, players_dat)

lm_fit_ncaa3ptpct <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_3ptpct)
lm_fit_ncaa3ptpct %>%
  broom::glance()
modelr::mse(lm_fit_ncaa3ptpct, players_dat)

# lm_fit_ncaafgpct <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_fgpct)
# lm_fit_ncaafgpct %>%
#   broom::glance()
# modelr::mse(lm_fit_ncaafgpct, players_dat)

lm_fit_ncaa3ptpg <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_3ptpg)
lm_fit_ncaa3ptpg %>%
  broom::glance()
modelr::mse(lm_fit_ncaa3ptpg, players_dat)

model_equation(lm_fit_ncaa3ptpg, digits = 3, trim = TRUE)

lm_fit_ncaa3ptpgncaaft <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_3ptpg + ncaa_ft)
lm_fit_ncaa3ptpgncaaft %>%
  broom::glance()
modelr::mse(lm_fit_ncaa3ptpgncaaft, players_dat)

lm_fit_ncaa3ptpgncaaftncaa3ptpct <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_3ptpg + ncaa_ft + ncaa_3ptpct)
lm_fit_ncaa3ptpgncaaftncaa3ptpct %>%
  broom::glance()
modelr::mse(lm_fit_ncaa3ptpgncaaftncaa3ptpct, players_dat)

lm_fit_almost_all <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_3ptpg + ncaa_ft + ncaa_3ptpct + ncaa_3ptapg + ncaa_fgpct)
lm_fit_almost_all %>%
  broom::glance()
modelr::mse(lm_fit_almost_all, players_dat)

lm_fit_all <- players_dat %>% lm(formula = nba_3ptpct ~ ncaa_3ptpg + ncaa_ft + ncaa_3ptpct + ncaa_3ptapg + ncaa_fgapg + ncaa_fgpct + ncaa_fgpg + ncaa_ftapg + ncaa_ftpg)
lm_fit_all %>%
  broom::glance()
modelr::mse(lm_fit_all, players_dat)
# lm_fit_all %>%
#   summary.lm()

model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}

model_equation(lm_fit_all, digits = 1, trim = TRUE)

#Basic Logistic Model
#Elite NBA 3PT Shooter
glm_fits <- players_dat_split %>% 
  mutate(mod_01 = map(train, glm, 
                      formula = elite_3pt_shooter ~ ncaa_3ptapg + ncaa_3ptpct + ncaa_3ptpg + ncaa_fgapg + ncaa_fgpct + ncaa_fgpg + ncaa_ft + ncaa_ftapg + ncaa_ftpg,
                      family = binomial))

glm_fits %>% 
  pluck("mod_01", 1) %>% 
  tidy()

glm_fits %>% 
  pluck("mod_01", 1) %>% 
  predict(type = "response") %>% 
  skim_without_charts()

demo_tib <- glm_fits %>%
  mutate(train_prob = map(mod_01, predict, type = "response"),
         train_direction = map(train_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(train_direction) %>% 
  mutate(prop = n / sum(n))

demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(elite_3pt_shooter, train_direction) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(elite_3pt_shooter))

demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  mutate(correct = if_else(train_direction == elite_3pt_shooter, 1, 0)) %>% 
  summarise(train_accuracy = mean(correct),
            train_error    = 1 - train_accuracy)

demo_tib <- demo_tib %>%
  mutate(test_prob = map2(mod_01, test, predict, type = "response"),
         test_direction = map(test_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib %>% 
  unnest(cols = c(test, test_direction)) %>% 
  mutate(correct = if_else(test_direction == elite_3pt_shooter, 1, 0)) %>% 
  summarise(test_accuracy = mean(correct),
            test_error    = 1 - test_accuracy)

#Good NBA 3PT Shooter
glm_fits_2 <- players_dat_split %>% 
  mutate(mod_01 = map(train, glm, 
                      formula = good_3pt_shooter ~ ncaa_3ptapg + ncaa_3ptpct + ncaa_3ptpg + ncaa_fgapg + ncaa_fgpct + ncaa_fgpg + ncaa_ft + ncaa_ftapg + ncaa_ftpg,
                      family = binomial))

glm_fits_2 %>% 
  pluck("mod_01", 1) %>% 
  tidy()

glm_fits_2 %>% 
  pluck("mod_01", 1) %>% 
  predict(type = "response") %>% 
  skim_without_charts()

demo_tib_2 <- glm_fits_2 %>%
  mutate(train_prob = map(mod_01, predict, type = "response"),
         train_direction = map(train_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib_2 %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(train_direction) %>% 
  mutate(prop = n / sum(n))

demo_tib_2 %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(good_3pt_shooter, train_direction) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(good_3pt_shooter))

demo_tib_2 %>% 
  unnest(cols = c(train, train_direction)) %>% 
  mutate(correct = if_else(train_direction == good_3pt_shooter, 1, 0)) %>% 
  summarise(train_accuracy = mean(correct),
            train_error    = 1 - train_accuracy)

demo_tib_2 <- demo_tib %>%
  mutate(test_prob = map2(mod_01, test, predict, type = "response"),
         test_direction = map(test_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib_2 %>% 
  unnest(cols = c(test, test_direction)) %>% 
  mutate(correct = if_else(test_direction == good_3pt_shooter, 1, 0)) %>% 
  summarise(test_accuracy = mean(correct),
            test_error    = 1 - test_accuracy)

# # K-Fold Cross Validation
# #ncaa_3ptapg
# model_def <- tibble(degree = 1:10,
#                     fmla = str_c("nba_3ptpct ~ poly(ncaa_3ptapg, ", degree, ")"))
# 
# players_5fold <- players_dat %>% 
#   crossv_kfold(5, id = "fold") %>%
#   mutate(
#     train = map(train, as_tibble),
#     test = map(test, as_tibble)
#   )
# 
# players_5fold <- players_5fold %>% 
#   crossing(model_def) %>% 
#   mutate(model_fit = map2(fmla, train, lm),
#          fold_mse = map2_dbl(model_fit, test, mse))
# 
# players_5fold %>% 
#   ggplot(aes(x = degree, y = fold_mse, color = fold)) +
#   geom_line()
# 
# players_5fold %>% 
#   group_by(degree) %>% 
#   summarize(test_mse = mean(fold_mse)) %>%
#   ggplot(aes(x = degree, y = test_mse)) +
#   geom_line() +
#   geom_point()
# 
# players_5fold %>%
#   group_by(degree) %>%
#   summarise(fold_mse= mean(fold_mse))
# 
# #ncaa_3ptpct
# model_def_2 <- tibble(degree = 1:10,
#                     fmla = str_c("nba_3ptpct ~ poly(ncaa_3ptpct, ", degree, ")"))
# 
# players_5fold_2 <- players_dat %>% 
#   crossv_kfold(5, id = "fold") %>%
#   mutate(
#     train = map(train, as_tibble),
#     test = map(test, as_tibble)
#   )
# 
# players_5fold_2 <- players_5fold_2 %>% 
#   crossing(model_def) %>% 
#   mutate(model_fit = map2(fmla, train, lm),
#          fold_mse = map2_dbl(model_fit, test, mse))
# 
# players_5fold_2 %>% 
#   ggplot(aes(x = degree, y = fold_mse, color = fold)) +
#   geom_line()
# 
# players_5fold_2 %>% 
#   group_by(degree) %>% 
#   summarize(test_mse = mean(fold_mse)) %>%
#   ggplot(aes(x = degree, y = test_mse)) +
#   geom_line() +
#   geom_point()
# 
# players_5fold_2 %>%
#   group_by(degree) %>%
#   summarise(fold_mse= mean(fold_mse))
# 
# #ncaa_ft
# model_def_3 <- tibble(degree = 1:10,
#                       fmla = str_c("nba_3ptpct ~ poly(ncaa_ft, ", degree, ")"))
# 
# players_5fold_3 <- players_dat %>% 
#   crossv_kfold(5, id = "fold") %>%
#   mutate(
#     train = map(train, as_tibble),
#     test = map(test, as_tibble)
#   )
# 
# players_5fold_3 <- players_5fold_3 %>% 
#   crossing(model_def) %>% 
#   mutate(model_fit = map2(fmla, train, lm),
#          fold_mse = map2_dbl(model_fit, test, mse))
# 
# players_5fold_3 %>% 
#   ggplot(aes(x = degree, y = fold_mse, color = fold)) +
#   geom_line()
# 
# players_5fold_3 %>% 
#   group_by(degree) %>% 
#   summarize(test_mse = mean(fold_mse)) %>%
#   ggplot(aes(x = degree, y = test_mse)) +
#   geom_line() +
#   geom_point()
# 
# players_5fold_3 %>%
#   group_by(degree) %>%
#   summarise(fold_mse= mean(fold_mse))

#Boosted model for regression
if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
  tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
  lab <- tmp[,1]
} else {
  lab <- dat[[outcome]]
}
xgb_matrix <- function(dat, outcome, exclude_vars){
  if(!is_tibble(dat)){
    dat <- as_tibble(dat)
  }
  dat_types <- dat %>% map_chr(class)
  outcome_type <- class(dat[[outcome]])
  if("character" %in% dat_types){
    print("You must encode characters as factors.")
    return(NULL)
  } else {
    if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
      tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
      lab <- tmp[,1]
    } else {
      lab <- dat[[outcome]]
    }
    mat <- dat %>% dplyr::select(-outcome, -all_of(exclude_vars)) %>%
      onehot::onehot() %>%
      predict(dat)
    return(xgb.DMatrix(data = mat, 
                       label = lab))
    
  }}

xg_error <- function(model, test_mat, metric = "mse"){
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  if(metric == "mse"){
    err <- mean((preds - vals)^2)
  } else if(metric == "misclass") {
    err <- mean(preds != vals)
  }
  return(err)
}

nba_3pt_xg_reg <- players_dat_split %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>% # tune the learning rate
  mutate(# Build xgb Dmatrices for training and test set
    train_mat = map(train, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name")), 
    test_mat = map(test, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name")),
    # Train xgb models for each learning rate
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y,
                                                                depth = 10, # tree depth, can tune
                                                                objective = "reg:squarederror"), # minimize MSE
                                                  data = x, 
                                                  nrounds = 500, # fit 500 trees, can tune
                                                  silent = TRUE)), 
    # Get training and test error
    xg_train_mse = map2(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test_mat, xg_error, metric = "mse"))

nba_3pt_xg_reg %>%
  pluck("xg_test_mse")

ggplot(nba_3pt_xg_reg) + 
  geom_line(aes(learn_rate, unlist(xg_train_mse), color = "Training Error")) +
  geom_line(aes(learn_rate, unlist(xg_test_mse), color = "Test Error")) +
  scale_color_manual("", values = c("blue", "red")) + 
  labs(x = "Learning Rate", y = "MSE")

xg_reg_mod <- nba_3pt_xg_reg %>% 
  arrange(unlist(xg_test_mse)) %>%
  pluck("xg_model", 1)

vip(xg_reg_mod)

nba_3pt_xg_reg_2 <- players_dat_split %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>% # tune the learning rate
  mutate(# Build xgb Dmatrices for training and test set
    train_mat = map(train, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name", "ncaa_3ptapg", "ncaa_fgapg", "ncaa_fgpct", "ncaa_fgpg", "ncaa_ftapg", "ncaa_ftpg")), 
    test_mat = map(test, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name", "ncaa_3ptapg", "ncaa_fgapg", "ncaa_fgpct", "ncaa_fgpg", "ncaa_ftapg", "ncaa_ftpg")),
    # Train xgb models for each learning rate
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y,
                                                                depth = 10, # tree depth, can tune
                                                                objective = "reg:squarederror"), # minimize MSE
                                                  data = x, 
                                                  nrounds = 500,
                                                  silent = TRUE)), 
    # Get training and test error
    xg_train_mse = map2(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test_mat, xg_error, metric = "mse"))

nba_3pt_xg_reg_2 %>%
  pluck("xg_test_mse")

ggplot(nba_3pt_xg_reg_2) + 
  geom_line(aes(learn_rate, unlist(xg_train_mse), color = "Training Error")) +
  geom_line(aes(learn_rate, unlist(xg_test_mse), color = "Test Error")) +
  scale_color_manual("", values = c("blue", "red")) + 
  labs(x = "Learning Rate", y = "MSE")

xg_reg_mod_2 <- nba_3pt_xg_reg_2 %>% 
  arrange(unlist(xg_test_mse)) %>%
  pluck("xg_model", 1)

vip(xg_reg_mod_2)


nba_3pt_xg_reg_3 <- players_dat_split %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>% # tune the learning rate
  mutate(# Build xgb Dmatrices for training and test set
    train_mat = map(train, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name")), 
    test_mat = map(test, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name")),
    # Train xgb models for each learning rate
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y,
                                                                depth = 5, # tree depth, can tune
                                                                objective = "reg:squarederror"), # minimize MSE
                                                  data = x, 
                                                  nrounds = 500, # fit 100000 trees, can tune
                                                  silent = TRUE)), 
    # Get training and test error
    xg_train_mse = map2(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test_mat, xg_error, metric = "mse"))

nba_3pt_xg_reg_3 %>%
  pluck("xg_test_mse")

ggplot(nba_3pt_xg_reg_3) + 
  geom_line(aes(learn_rate, unlist(xg_train_mse), color = "Training Error")) +
  geom_line(aes(learn_rate, unlist(xg_test_mse), color = "Test Error")) +
  scale_color_manual("", values = c("blue", "red")) + 
  labs(x = "Learning Rate", y = "MSE")

xg_reg_mod_3 <- nba_3pt_xg_reg_3 %>% 
  arrange(unlist(xg_test_mse)) %>%
  pluck("xg_model", 1)

vip(xg_reg_mod_3)

nba_3pt_xg_reg_8 <- players_dat_split %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>% # tune the learning rate
  mutate(# Build xgb Dmatrices for training and test set
    train_mat = map(train, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name")), 
    test_mat = map(test, xgb_matrix, outcome = all_of("nba_3ptpct"), exclude_vars = c("id", "elite_3pt_shooter", "good_3pt_shooter", "position", "name")),
    # Train xgb models for each learning rate
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y,
                                                                depth = 2, # tree depth, can tune
                                                                objective = "reg:squarederror"), # minimize MSE
                                                  data = x, 
                                                  nrounds = 500, # fit 100000 trees, can tune
                                                  silent = TRUE)), 
    # Get training and test error
    xg_train_mse = map2(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test_mat, xg_error, metric = "mse"))

nba_3pt_xg_reg_8 %>%
  pluck("xg_test_mse")

xg_reg_mod_8 <- nba_3pt_xg_reg_8 %>% 
  arrange(unlist(xg_test_mse)) %>%
  pluck("xg_model", 1)

vip(xg_reg_mod_8)
