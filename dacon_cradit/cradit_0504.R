# 패키지 다운로드

#install.packages('magrittr')
suppressMessages(library(magrittr))
#install.packages('tidymodels')
# C:\Users\ai\AppData\Local\Temp\Rtmp4AZMmv\downloaded_packages 여기에 저장
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("knitr")
suppressMessages(library(tidymodels)) %>% suppressWarnings()
suppressMessages(library(tidyverse)) %>% suppressWarnings()
suppressMessages(library(skimr)) %>% suppressWarnings()
suppressMessages(library(knitr)) %>% suppressWarnings()
theme_set(theme_bw())

# 데이터셋 불러오기
file_path <- "C:/data/dacon_data/cradit"
files <- list.files(file_path)
files

# 각 csv 변수 각 변수의 이름을 janitor 패키지로 말끔하게 바꿔준다.
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

# 데이터 기본정보 확인
train %>% 
  head()
dim(train) # 칼럼 정보 추출
names(train)
skim(train) # 데이터를 훑어보기 위해서 skim() 함수
skim(test)

# 시각화
train %>%
  ggplot(aes(x = factor(credit), y = income_total)) +
  geom_boxplot() +
  facet_grid(. ~ income_type)

# 전처리 하기
# tidymodels에서는 전처리를 할 때 recipe 라는 패키지를 사용한다. 
# 이 패키지에는 전처리를 하는 방법을 음식 레피시 처럼 적어놓는다고 생각하면 쉽다.
# 결과값인 credit 변수와 character 타입의 변수들을 factor 변수로 바꿔
# 나이와 직업을 가진 기간을 년수
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

# juice를 통한 전처리 즙짜기
# juice() 함수를 통해서 recipe에 입력된 전처리를 짜낸 데이터를 얻어온다.
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

# 튜닝 준비하기
# validation_split() 함수를 사용하여 평가셋을 분리
set.seed(2021)

validation_split <- validation_split(train2, prop = 0.7, 
                                     strata = credit)
# 튜닝 스펙 설정
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

# 워크 플로우 설정
workflow <- workflow() %>%
  add_model(tune_spec) %>% 
  add_formula(credit ~ .)

# 모델 튜닝 with tune_grid()
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

# 튜닝결과 시각화
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

# ==== 튜닝된 모델 학습하기
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

# ===== 예측하기
result <- predict(rf_fit, test2, type = "prob")
result %>% head()
submission <- read_csv(file.path(file_path, "sample_submission.csv"))
sub_col <- names(submission)
submission <- bind_cols(submission$index, result)
names(submission) <- sub_col
write.csv(submission, row.names = FALSE,
          "C:/data/dacon_data/cradit/baseline_dacon_credit.csv")
























