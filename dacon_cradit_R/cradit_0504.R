# ��Ű�� �ٿ�ε�

#install.packages('magrittr')
suppressMessages(library(magrittr))
#install.packages('tidymodels')
# C:\Users\ai\AppData\Local\Temp\Rtmp4AZMmv\downloaded_packages ���⿡ ����
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("knitr")
suppressMessages(library(tidymodels)) %>% suppressWarnings()
suppressMessages(library(tidyverse)) %>% suppressWarnings()
suppressMessages(library(skimr)) %>% suppressWarnings()
suppressMessages(library(knitr)) %>% suppressWarnings()
theme_set(theme_bw())

# �����ͼ� �ҷ�����
file_path <- "C:/data/dacon_data/cradit"
files <- list.files(file_path)
files

# �� csv ���� �� ������ �̸��� janitor ��Ű���� �����ϰ� �ٲ��ش�.
# install.packages("janitor")
train <- read_csv(file.path(file_path, "train.csv"),
                  col_types = cols(
                    credit = col_factor(levels = c("0.0", "1.0", "2.0"))
                  )) %T>% 
  suppressMessages() %>% 
  janitor::clean_names()
test <- read_csv(file.path(file_path, "test.csv")) %T>%
  suppressMessages() %>% 
  janitor::clean_names()

# ������ �⺻���� Ȯ��
train %>% 
  head()
dim(train) # Į�� ���� ����
names(train)
skim(train) # �����͸� �Ⱦ�� ���ؼ� skim() �Լ�
skim(test)

# �ð�ȭ
train %>%
  ggplot(aes(x = factor(credit), y = income_total)) +
  geom_boxplot() +
  facet_grid(. ~ income_type)

# ��ó�� �ϱ�
# tidymodels������ ��ó���� �� �� recipe ��� ��Ű���� ����Ѵ�. 
# �� ��Ű������ ��ó���� �ϴ� ����� ���� ���ǽ� ó�� ������´ٰ� �����ϸ� ����.
# ������� credit ������ character Ÿ���� �������� factor ������ �ٲ�
# ���̿� ������ ���� �Ⱓ�� ���
# install.packages("recipe")

credit_recipe <- train %>% 
  recipe(credit ~ .) %>%
  # age and employment period in yrs
  step_mutate(yrs_birth = -ceiling(days_birth/365),
              yrs_employed = -ceiling(days_employed/365)) %>% 
  step_rm(index, days_birth, days_employed) %>%
  step_unknown(occyp_type) %>% 
  step_integer(all_nominal(), -all_outcomes()) %>% 
  step_center(all_predictors(), -all_outcomes()) %>% 
  prep(training = train)

print(credit_recipe)

# juice�� ���� ��ó�� ��¥��
# juice() �Լ��� ���ؼ� recipe�� �Էµ� ��ó���� ¥�� �����͸� ���´�.
train2 <- juice(credit_recipe)
test2 <- bake(credit_recipe, new_data = test)
head(train2)
head(test2)

train2 %>%
map_df(~sum(is.na(.))) %>%
  pivot_longer(cols = everything(),
       names_to = "variable",
       values_to = "na_count") %>% 
  filter(na_count > 0)

# Ʃ�� �غ��ϱ�
# validation_split() �Լ��� ����Ͽ� �򰡼��� �и�
set.seed(2021)

validation_split <- validation_split(train2, prop = 0.7, 
                                     strata = credit)
# Ʃ�� ���� ����
cores <- parallel::detectCores() -1
cores

tune_spec <- rand_forest(mtry = tune(),
                         min_n = tune(),
                         trees = 1000) %>% 
    set_engine("ranger",
               num.threads = cores) %>% 
    set_mode("classification")

# param_grid <- grid_latin_hypercube(finalize(mtry(), x = train2[,-1]),
#                                    min_n(), size = 100)

# from param tune
param_grid <- tibble(mtry = 3, min_n = 5)

# ��ũ �÷ο� ����
workflow <- workflow() %>%
  add_model(tune_spec) %>% 
  add_formula(credit ~ .)

# �� Ʃ�� with tune_grid()
# install.packages("tictoc")
# install.packages('ranger')
library(tictoc)
tic()
tune_result <- workflow %>% 
  tune_grid(validation_split,
            grid = param_grid,
            metrics = metric_set(mn_log_loss))
toc()

tune_result$.notes[[1]]$.notes
tune_result %>% 
  collect_metrics()

# Ʃ�װ�� �ð�ȭ
tune_result %>%
  collect_metrics() %>%
  filter(.metric == "mn_log_loss") %>% 
  ggplot(aes(mtry, mean, color = .metric)) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(title = "Mean Log loss")

tune_result %>% show_best()
tune_best <- tune_result %>% select_best(metric = "mn_log_loss")
tune_best$mtry
tune_best$min_n

# ==== Ʃ�׵� �� �н��ϱ�
rf_model <- 
  rand_forest(mtry = tune_best$mtry,
              min_n = tune_best$min_n,
              trees = 1000) %>% 
    set_engine("ranger", seed = 2021, 
               num.threads = cores) %>% 
    set_mode("classification")

tictoc::tic()
rf_fit <- 
    rf_model %>% 
    fit(credit ~ ., data = train2)
tictoc::toc()

options(max.print = 10)
rf_fit

# ===== �����ϱ�
result <- predict(rf_fit, test2, type = "prob")
result %>% head()
submission <- read_csv(file.path(file_path, "sample_submission.csv"))
sub_col <- names(submission)
submission <- bind_cols(submission$index, result)
names(submission) <- sub_col
write.csv(submission, row.names = FALSE,
          "C:/data/dacon_data/cradit/baseline_dacon_credit.csv")























