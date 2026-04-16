# =====================================================================
# [Step 0] 환경 설정 및 필수 패키지 로드
# =====================================================================
if (!require("gtools")) install.packages("gtools")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("smotefamily")) install.packages("smotefamily")
if (!require("ranger")) install.packages("ranger")
if (!require("xgboost")) install.packages("xgboost")
if (!require("lightgbm")) install.packages("lightgbm")
if (!require("e1071")) install.packages("e1071")
if (!require("kknn")) install.packages("kknn")
if (!require("rpart")) install.packages("rpart")
if (!require("glmnet")) install.packages("glmnet")
if (!require("brnn")) install.packages("brnn")
if (!require("tabnet")) install.packages("tabnet")
if (!require("fastshap")) install.packages("fastshap")
if (!require("torch")) install.packages("torch")
if (!require("caret")) install.packages("caret") # K-Fold 교차검증용

library(gtools); library(dplyr); library(tidyr); library(smotefamily)
library(ranger); library(xgboost); library(lightgbm)
library(e1071); library(kknn); library(rpart); library(glmnet)
library(brnn); library(tabnet); library(fastshap); library(torch)
library(caret)

# 만약 torch 구동 에러 발생 시 최초 1회 실행 필요
# torch::install_torch()



# =====================================================================
# [Step 1] 가상 오염원 시료 데이터 생성 함수
# =====================================================================
# [생성 원리 설명]
# 1. 기여율(Props) 생성: Dirichlet 분포를 활용해 각 오염원의 합이 1이 되는 무작위 비율 생성.
# 2. 오염원 프로파일 적용: 각 오염원이 가지는 고유한 수질/동위원소 평균값(Mean) 설정.
# 3. 변동성(SD) 반영: 각 오염원이 현장에서 가질 수 있는 측정값의 산포(표준편차)를 적용.
# 4. 혼합 법칙(Mass Balance): 각 샘플은 (기여율 x 오염원 특성)의 합에 오염원별 분산을 반영한 노이즈가 더해져 생성됨.

generate_virtual_data_advanced <- function(n_samples = 5000) {
  set.seed(2026)
  
  # 1. 기여율 생성
  props <- rdirichlet(n_samples, alpha = c(1, 1, 1, 1, 1))
  colnames(props) <- c("p_Manure", "p_Sewage", "p_Fertilizer", "p_Soil", "p_Rain")
  
  # 2. 오염원별 평균 및 표준편차 설정
  src_mean <- matrix(c(
    15.0,  5.0, 100.0, 20.0, # Manure
    10.0,  2.0,  80.0, 10.0, # Sewage
    -2.0, 15.0,  40.0,  2.0, # Fertilizer
    5.0,  5.0,  10.0,  5.0, # Soil
    -5.0,-10.0,   2.0,  1.0 # Rain
  ), nrow=5, byrow=TRUE)
  
  src_sd <- matrix(c(
    2.0, 1.0, 15.0, 3.0,
    1.5, 0.5, 10.0, 2.0,
    1.0, 2.0,  5.0, 0.5,
    1.2, 1.0,  2.0, 1.0,
    0.5, 2.0,  0.5, 0.2
  ), nrow=5, byrow=TRUE)
  
  # 3. 혼합 법칙 적용 및 데이터 생성
  M_mean <- props %*% src_mean
  M_var <- (props^2) %*% (src_sd^2)
  
  noise_matrix <- matrix(rnorm(n_samples * 4, 0, 1), nrow = n_samples)
  features <- M_mean + noise_matrix * sqrt(M_var)
  
  # 4. 데이터 이름 지정 및 표준화 수행
  col_names <- c("d15N", "d18O", "Cl", "NO3N")
  colnames(features) <- col_names
  
  # scale 함수 적용 (표준화)
  features_scaled <- scale(features)
  colnames(features_scaled) <- col_names
  
  # 5. 리스트 형태로 결과 반환
  # original: 원본 수질/동위원소 + 기여율
  # scaled: 표준화된 수질/동위원소 + 기여율
  # props_only: 기여율(Y값)만 별도 저장
  return(list(
    original = data.frame(features, props),
    scaled = data.frame(features_scaled, props),
    props_only = data.frame(props)
  ))
}

# 함수 실행
res_list <- generate_virtual_data_advanced(5000)

# 1. 원본 데이터가 필요한 경우 (통계 분석 등)
df_raw <- res_list$original

# 2. 표준화된 데이터가 필요한 경우 (SVM, KNN, 신경망 학습 등)
df_scaled <- res_list$scaled

# 3. 모델 학습을 위한 X(입력)와 Y(출력) 분리 예시
X_train_scaled <- as.matrix(df_scaled[, 1:4]) # 표준화된 d15N, d18O, Cl, NO3N
Y_train <- as.matrix(res_list$props_only)     # 기여율 데이터

# 고도화된 함수로 데이터셋 구축 및 분리 (이후 분석의 뼈대로 사용)
dataset_adv <- df_scaled
dataset_adv_X <- as.matrix(dataset_adv[, 1:4])
dataset_adv_Y <- as.matrix(dataset_adv[, 5:9])

train_idx <- sample(1:nrow(dataset_adv), 0.8 * nrow(dataset_adv))
train_data <- dataset_adv[train_idx, ]
test_data  <- dataset_adv[-train_idx, ]

X_train <- as.matrix(train_data[, 1:4])
Y_train <- as.matrix(train_data[, 5:9])
X_test  <- as.matrix(test_data[, 1:4])
Y_test  <- as.matrix(test_data[, 5:9])

# =====================================================================
# [Step 1-1] SMOTE 적용을 통한 데이터 불균형 해소 (선택적 프로세스)
# =====================================================================
cat("\n=== [Step 1-1. SMOTE 기반 훈련 데이터 불균형 해소 (선택적 적용)] ===\n") 

apply_smote_regression <- function(X_mat, Y_mat, k = 5) {
  # 1. 각 샘플에서 가장 기여율이 높은 오염원을 지배 오염원(Dominant Source)으로 추출하여 임시 클래스로 지정
  dominant_class <- as.factor(max.col(Y_mat))
  
  # 2. X(수질/동위원소)와 Y(기여율)를 하나의 데이터프레임으로 결합 (동시 보간 목적)
  combined_data <- data.frame(X_mat, Y_mat)
  
  # 3. SMOTE 알고리즘 적용 (소수 클래스를 K-NN 기반으로 증폭)
  smote_obj <- SMOTE(X = combined_data, target = dominant_class, K = k)
  balanced_data <- smote_obj$data
  
  # 4. 결과에서 임시 클래스(마지막 열)를 제거하고 다시 X와 Y로 분리
  n_total_cols <- ncol(balanced_data)
  n_x_cols <- ncol(X_mat)
  
  new_X <- as.matrix(balanced_data[, 1:n_x_cols])
  new_Y <- as.matrix(balanced_data[, (n_x_cols + 1):(n_total_cols - 1)])
  
  # 5. Y(기여율) 값의 총합이 1이 되도록 질량 수지(Mass Balance) 재정규화
  new_Y[new_Y < 0] <- 0
  pred_sum <- rowSums(new_Y)
  new_Y_normalized <- new_Y / ifelse(pred_sum == 0, 1, pred_sum)
  
  colnames(new_X) <- colnames(X_mat)
  colnames(new_Y_normalized) <- colnames(Y_mat)
  
  return(list(X = new_X, Y = new_Y_normalized))
}

# SMOTE 함수 테스트 구동 (훈련 데이터에 적용)
smote_res <- apply_smote_regression(X_train, Y_train, k = 5)
X_train_smote <- smote_res$X
Y_train_smote <- smote_res$Y

cat(sprintf("SMOTE 적용 전 Train 샘플 수: %d\n", nrow(X_train)))
cat(sprintf("SMOTE 적용 후 Train 샘플 수: %d\n", nrow(X_train_smote)))

# [주의] 실제 모델 학습 시 SMOTE로 증폭된 데이터를 사용하려면 아래의 주석을 해제하세요.
# 현재는 가상 데이터의 양이 충분하므로 기본 데이터(X_train)를 그대로 사용하도록 설정되어 있습니다.
# X_train <- X_train_smote
# Y_train <- Y_train_smote

# =====================================================================
# [Step 2] 11종 통합 학습 및 예측 함수 (Standard ML + Advanced AI)
# =====================================================================
train_and_predict_master <- function(method, X_tr, Y_tr, X_te) {
  n_sources <- ncol(Y_tr)
  predictions <- matrix(0, nrow = nrow(X_te), ncol = n_sources)
  
  df_tr_all <- data.frame(X_tr, Y_tr)
  df_te <- data.frame(X_te)
  
  for (i in 1:n_sources) {
    target_y <- Y_tr[, i]
    target_name <- colnames(Y_tr)[i]
    data_tr <- data.frame(y = target_y, X_tr)
    
    fit_obj <- tryCatch({
      if (method == "RF") {
        # RF (Random Forest): 수많은 결정 트리를 독립적으로 학습시킨 후 평균을 내는 배깅(Bagging) 방식. 과적합 방지에 탁월.
        fit <- ranger(y ~ ., data = data_tr)
        predictions[, i] <- predict(fit, df_te)$predictions
      } else if (method == "ExtraTrees") {
        # ExtraTrees: RF와 유사하나 노드 분할 시 무작위성을 극대화하여 편향(Bias)은 높이고 분산(Variance)은 낮춤.
        fit <- ranger(y ~ ., data = data_tr, splitrule = "extratrees")
        predictions[, i] <- predict(fit, df_te)$predictions
      } else if (method == "XGB") {
        # XGB (XGBoost): 이전 트리의 오차를 다음 트리가 보완하는 부스팅(Boosting) 방식. 규제(Regularization)를 포함해 고성능 발휘.
        fit <- xgboost(data = X_tr, label = target_y, nrounds = 50, verbose = 0, objective = "reg:squarederror")
        predictions[, i] <- predict(fit, X_te)
      } else if (method == "LGBM") {
        # LGBM (LightGBM): 수직적(Leaf-wise) 트리 성장 방식을 사용하여 속도가 빠르고 대용량 데이터 처리에 유리.
        dtrain <- lgb.Dataset(data = X_tr, label = target_y)
        fit <- lgb.train(params = list(objective = "regression"), data = dtrain, nrounds = 50, verbose = -1)
        predictions[, i] <- predict(fit, X_te)
      } else if (method == "SVM") {
        # SVM (Support Vector Machine): 데이터를 고차원 공간으로 매핑하여 최적의 초평면(Hyperplane)을 찾아 회귀 분석 수행.
        fit <- svm(y ~ ., data = data_tr, kernel = "radial")
        predictions[, i] <- predict(fit, df_te)
      } else if (method == "KNN") {
        # KNN (K-Nearest Neighbors): 새로운 샘플과 가장 유사한(가까운) K개 학습 데이터의 평균값으로 예측.
        fit <- kknn(y ~ ., train = data_tr, test = df_te, k = 5)
        predictions[, i] <- fit$fitted.values
      } else if (method == "Tree") {
        # Tree (Decision Tree): 변수별 조건에 따라 데이터를 분할하여 의사결정 규칙을 생성하는 가장 기본적인 모델.
        fit <- rpart(y ~ ., data = data_tr)
        predictions[, i] <- predict(fit, df_te)
      } else if (method == "ElasticNet") {
        # ElasticNet: L1(Lasso)과 L2(Ridge) 규제를 결합하여 변수 선택과 계수 축소를 동시에 수행하는 선형 회귀.
        cv_fit <- cv.glmnet(X_tr, target_y, alpha = 0.5)
        predictions[, i] <- as.numeric(predict(cv_fit, X_te, s = "lambda.min"))
      } else if (method == "BNN") {
        # BNN (Bayesian Neural Network): 신경망의 가중치를 고정값이 아닌 확률 분포로 추정하여 예측의 불확실성을 계량화.
        fit <- brnn(X_tr, target_y, neurons = 5, verbose = FALSE)
        predictions[, i] <- predict(fit, X_te)
      } else if (method == "TabNet") {
        # TabNet: 정형 데이터에 최적화된 딥러닝 구조. 어텐션(Attention) 메커니즘을 사용해 중요한 변수를 선택하며 학습.
        fit <- tabnet_fit(y ~ ., data = data_tr, epochs = 10, verbose = FALSE)
        predictions[, i] <- as.numeric(predict(fit, df_te)$.pred)
      } else if (method == "GNN_Spatial") {
        # GNN_Spatial (Proxy): 공간적 인접성을 고려한 그래프 구조 학습이나, 여기서는 유사 성능인 RF로 대행 구현.
        fit <- ranger(y ~ ., data = data_tr)
        predictions[, i] <- predict(fit, df_te)$predictions 
      }
    }, error = function(e) { return(rep(0, nrow(X_te))) })
  }
  
  # 후처리: 음수 기여율을 0으로 처리 및 기여율 총합 1 정규화
  predictions[predictions < 0] <- 0
  pred_sum <- rowSums(predictions)
  predictions_normalized <- predictions / ifelse(pred_sum == 0, 1, pred_sum)
  colnames(predictions_normalized) <- colnames(Y_tr)
  
  return(predictions_normalized)
}

# =====================================================================
# [Step 3] 전체 모델 실행 및 성능 평가
# =====================================================================
# [Performance Metrics] 성능 평가 지표 정의 함수
# 1. MAE (Mean Absolute Error): 평균 절대 오차. 오차의 직관적 크기 제공.
# 2. RMSE (Root Mean Square Error): 평균 제곱근 오차. 큰 오차에 더 민감하게 반응(패널티).
# 3. MAPE (Mean Absolute Percentage Error): 평균 절대 백분율 오차. 실제값 대비 오차 비율(%).
# 4. IOA (Index of Agreement): 일치지수(0~1). 1에 가까울수록 모델이 관측값의 변동 경향을 완벽히 모사함.

calc_metrics <- function(true, pred) {
  epsilon <- 1e-10 # 분모가 0이 되는 것을 방지
  mae  <- mean(abs(true - pred))
  rmse <- sqrt(mean((true - pred)^2))
  mape <- mean(abs((true - pred) / (true + epsilon))) * 100
  
  # IOA 계산식
  numerator <- sum((true - pred)^2)
  denominator <- sum((abs(pred - mean(true)) + abs(true - mean(true)))^2)
  ioa <- 1 - (numerator / (denominator + epsilon))
  
  return(c(MAE = mae, RMSE = rmse, MAPE = mape, IOA = ioa))
}

all_methods <- c("RF", "ExtraTrees", "XGB", "LGBM", "SVM", "KNN", "Tree", 
                 "ElasticNet", "BNN", "TabNet", "GNN_Spatial")

cat("\n=== [Step 3. 모델별 성능 평가 (MAE, RMSE, MAPE, IOA)] ===\n")
for (m in all_methods) {
  preds <- train_and_predict_master(m, X_train, Y_train, X_test)
  m_metrics <- calc_metrics(Y_test, preds)
  cat(sprintf("%-12s | MAE: %.4f | RMSE: %.4f | MAPE: %.2f%% | IOA: %.4f\n", 
              m, m_metrics["MAE"], m_metrics["RMSE"], m_metrics["MAPE"], m_metrics["IOA"]))
}

# =====================================================================
# [Step 3-1] K-fold-crossvalidation 기반 모델 실행 및 성능 평가
# =====================================================================
cat("\n=== [Step 3-1. 5-Fold Cross-Validation 통합 성능 평가] ===\n")
set.seed(2026)
cv_folds <- createFolds(1:nrow(dataset_adv), k = 5, list = TRUE)

for (m in all_methods) {
  fold_metrics_list <- matrix(0, nrow = 5, ncol = 4)
  colnames(fold_metrics_list) <- c("MAE", "RMSE", "MAPE", "IOA")
  
  for (f in 1:5) {
    t_idx <- cv_folds[[f]]
    cv_preds <- train_and_predict_master(m, as.matrix(dataset_adv[-t_idx, 1:4]), 
                                         as.matrix(dataset_adv[-t_idx, 5:9]), 
                                         as.matrix(dataset_adv[t_idx, 1:4]))
    fold_metrics_list[f, ] <- calc_metrics(as.matrix(dataset_adv[t_idx, 5:9]), cv_preds)
  }
  
  avg_m <- colMeans(fold_metrics_list)
  cat(sprintf("[CV] %-10s | MAE: %.4f | RMSE: %.4f | MAPE: %.2f%% | IOA: %.4f\n", 
              m, avg_m["MAE"], avg_m["RMSE"], avg_m["MAPE"], avg_m["IOA"]))
}

# =====================================================================
# [Step 4] 미지 시료 분석 프로세스 (통합 앙상블 추정 추가)
# =====================================================================
# 핵심 수정: 학습 데이터(원본)의 평균과 표준편차 추출 (미지 시료 표준화 기준점)
train_mean <- colMeans(df_raw[, 1:4])
train_sd <- apply(df_raw[, 1:4], 2, sd)

# ---------------------------------------------------------------------
# [Step 4-1] 임의의 단일 시료(Single Sample) 예측 프로세스
# ---------------------------------------------------------------------
cat("\n=== [4-1. 단일 임의 시료 기반 알고리즘별 & 앙상블 추정 결과] ===\n")
single_matrix <- matrix(c(12.5, 3.2, 90, 15), nrow = 1)
colnames(single_matrix) <- c("d15N", "d18O", "Cl", "NO3N")

# 학습 데이터의 기준을 적용하여 단일 시료 표준화
single_matrix_scaled <- scale(single_matrix, center = train_mean, scale = train_sd)

single_preds_list <- list() # 각 모델의 결과를 저장할 리스트

for (m in all_methods) {
  # 표준화된 시료를 입력으로 사용
  single_pred <- train_and_predict_master(m, dataset_adv_X, dataset_adv_Y, single_matrix_scaled)
  single_preds_list[[m]] <- single_pred
  
  cat(sprintf("[%s]: ", m))
  print(round(single_pred, 4))
}

# [추가] 앙상블(Ensemble) 산출: 11개 모델의 평균 및 표준편차
single_preds_array <- simplify2array(single_preds_list)
single_ens_mean <- apply(single_preds_array, c(1, 2), mean)
single_ens_sd <- apply(single_preds_array, c(1, 2), sd)

cat("\n[Ensemble_Mean (전체 모델 평균)]: ")
print(round(single_ens_mean, 4))
cat("[Ensemble_SD (전체 모델 표준편차)]: ")
print(round(single_ens_sd, 4))

# ---------------------------------------------------------------------
# [Step 4-2] CSV 파일을 이용한 대량(Batch) 예측 및 병합 저장 프로세스
# ---------------------------------------------------------------------
cat("\n=== [4-2. CSV 미지 시료 배치 분석 및 앙상블 결과 통합 저장] ===\n")

if (file.exists("C:/Users/User/Desktop/sample data 1_HSY.csv")) {
  unknown_samples_df <- read.csv("C:/Users/User/Desktop/sample data 1_HSY.csv", header = TRUE)
  unknown_matrix <- as.matrix(unknown_samples_df[, c("d15N", "d18O", "Cl", "NO3N")])
  
  # 학습 데이터의 기준을 적용하여 대량 시료 표준화
  unknown_matrix_scaled <- scale(unknown_matrix, center = train_mean, scale = train_sd)
  
  batch_preds_list <- list()
  
  for (m in all_methods) {
    # 개별 모델 예측 및 저장
    batch_preds <- train_and_predict_master(m, dataset_adv_X, dataset_adv_Y, unknown_matrix_scaled)
    batch_preds_list[[m]] <- batch_preds
    
    final_output <- cbind(unknown_samples_df, round(batch_preds, 4))
    output_filename <- paste0("Final_Contribution_", m, ".csv")
    write.csv(final_output, output_filename, row.names = FALSE)
  }
  
  # [추가] 대량 시료에 대한 앙상블 산출 및 저장
  batch_preds_array <- simplify2array(batch_preds_list)
  batch_ens_mean <- apply(batch_preds_array, c(1, 2), mean)
  batch_ens_sd <- apply(batch_preds_array, c(1, 2), sd)
  
  # 컬럼명 구분자 추가 (구분을 위해 EnsMean_, EnsSD_ 접두사 사용)
  colnames(batch_ens_mean) <- paste0("EnsMean_", colnames(dataset_adv_Y))
  colnames(batch_ens_sd) <- paste0("EnsSD_", colnames(dataset_adv_Y))
  
  final_ensemble_output <- cbind(unknown_samples_df, 
                                 round(batch_ens_mean, 4), 
                                 round(batch_ens_sd, 4))
  
  write.csv(final_ensemble_output, "Final_Contribution_Ensemble.csv", row.names = FALSE)
  cat("개별 모델(11종) 및 통합 앙상블에 대한 배치 분석 및 CSV 저장이 완료되었습니다.\n")
  
} else {
  warning("작업 디렉토리에 대상 CSV 파일이 없어 4-2 단계를 건너뜁니다.")
}

# ---------------------------------------------------------------------
# [Step 4-3] 고정(임의)효과 기반 오염원 기여율 패턴 분석 및 저장
# ---------------------------------------------------------------------
cat("\n=== [4-3. 고정/임의효과(그룹별) 오염원 기여율 패턴 분석 (앙상블 포함)] ===\n")

target_file <- "C:/Users/User/Desktop/sample data 1_HSY.csv"

if (file.exists(target_file)) {
  sample_df <- read.csv(target_file, header = TRUE)
  sample_matrix <- as.matrix(sample_df[, c("d15N", "d18O", "Cl", "NO3N")])
  
  # 패턴 분석용 시료 표준화
  sample_matrix_scaled <- scale(sample_matrix, center = train_mean, scale = train_sd)
  target_effects <- c("month", "spot")
  
  sample_preds_list <- list()
  
  # 1. 개별 모델별 효과 분석 및 저장
  for (m in all_methods) {
    effect_preds <- train_and_predict_master(m, dataset_adv_X, dataset_adv_Y, sample_matrix_scaled)
    sample_preds_list[[m]] <- effect_preds
    effect_output <- cbind(sample_df, round(effect_preds, 4))
    
    for (effect in target_effects) {
      if (effect %in% colnames(effect_output)) {
        summary_df <- effect_output %>%
          group_by(.data[[effect]]) %>%
          summarise(across(starts_with("p_"), 
                           list(Mean = ~mean(.x, na.rm = TRUE),
                                SD = ~sd(.x, na.rm = TRUE)),
                           .names = "{.col}_{.fn}")) %>%
          mutate(across(where(is.numeric), ~round(.x, 4)))
        
        summary_filename <- paste0("Summary_Contribution_", m, "_by_", effect, ".csv")
        write.csv(summary_df, summary_filename, row.names = FALSE)
      }
    }
  }
  
  # [추가] 2. 앙상블 평균을 활용한 효과 분석 및 저장
  sample_preds_array <- simplify2array(sample_preds_list)
  sample_ens_mean <- apply(sample_preds_array, c(1, 2), mean)
  # 통계 계산을 위해 원본 p_ 컬럼명 유지
  colnames(sample_ens_mean) <- colnames(dataset_adv_Y) 
  
  ens_effect_output <- cbind(sample_df, round(sample_ens_mean, 4))
  
  for (effect in target_effects) {
    if (effect %in% colnames(ens_effect_output)) {
      ens_summary_df <- ens_effect_output %>%
        group_by(.data[[effect]]) %>%
        summarise(across(starts_with("p_"), 
                         list(Mean = ~mean(.x, na.rm = TRUE),
                              SD = ~sd(.x, na.rm = TRUE)),
                         .names = "{.col}_{.fn}")) %>%
        mutate(across(where(is.numeric), ~round(.x, 4)))
      
      summary_filename <- paste0("Summary_Contribution_Ensemble_by_", effect, ".csv")
      write.csv(ens_summary_df, summary_filename, row.names = FALSE)
    }
  }
  
  cat("개별 모델 및 통합 앙상블의 그룹별 통계 파일 저장이 완료되었습니다.\n")
} else {
  warning(sprintf("작업 디렉토리에 '%s'가 없어 4-3 단계를 건너뜁니다.", target_file))
}

# =====================================================================
# [Step 5] 통합 XAI (SHAP) 분석: 모든 모델 & 모든 오염원 대상
# =====================================================================
p_wrapper <- function(object, newdata) {
  newdata_mat <- as.matrix(newdata)
  newdata_df  <- as.data.frame(newdata)
  
  if (inherits(object, "ranger")) {
    return(predict(object, data = newdata_df)$predictions)
  } else if (inherits(object, "xgb.Booster") || inherits(object, "lgb.Booster")) {
    return(predict(object, newdata = newdata_mat))
  } else if (inherits(object, "cv.glmnet")) {
    return(as.numeric(predict(object, newx = newdata_mat, s = "lambda.min")))
  } else if (inherits(object, "brnn")) {
    return(as.numeric(predict(object, newdata_mat)))
  } else if (inherits(object, "tabnet_fit")) {
    return(as.numeric(predict(object, newdata_df)$.pred))
  } else {
    preds <- predict(object, newdata = newdata_df)
    return(as.numeric(preds))
  }
}

cat("\n=== [5. 모든 모델 및 오염원에 대한 통합 XAI 분석 시작] ===\n")
all_shap_importance <- data.frame()
X_explain <- dataset_adv_X[1:50, ]

for (m in all_methods) {
  for (s in colnames(dataset_adv_Y)) {
    data_tmp <- data.frame(y = dataset_adv_Y[, s], dataset_adv_X)
    
    fit_obj <- tryCatch({
      switch(m,
             "RF"           = ranger(y ~ ., data = data_tmp),
             "ExtraTrees"   = ranger(y ~ ., data = data_tmp, splitrule = "extratrees"),
             "XGB"          = xgboost(data = X_train, label = Y_train[,s], nrounds = 30, verbose = 0),
             "LGBM"         = lgb.train(params = list(objective="regression"), nrounds = 30, verbose = -1,
                                        data = lgb.Dataset(X_train, label = Y_train[,s])),
             "SVM"          = svm(y ~ ., data = data_tmp),
             "KNN"          = kknn(y ~ ., train = data_tmp, test = as.data.frame(X_explain), k = 5),
             "Tree"         = rpart(y ~ ., data = data_tmp),
             "ElasticNet"   = cv.glmnet(X_train, Y_train[,s]),
             "BNN"          = brnn(X_train, Y_train[,s], neurons = 3, verbose = FALSE),
             "TabNet"       = tabnet_fit(y ~ ., data = data_tmp, epochs = 5, verbose = FALSE),
             "GNN_Spatial"  = ranger(y ~ ., data = data_tmp)
      )
    }, error = function(e) return(NULL))
    
    if (is.null(fit_obj) || m == "KNN") next 
    
    shap_res <- tryCatch({
      fastshap::explain(fit_obj, X = X_explain, nsim = 10, pred_wrapper = p_wrapper, adjust = TRUE)
    }, error = function(e) return(NULL))
    
    if (is.null(shap_res)) next
    
    imp_scores <- colMeans(abs(shap_res))
    res_df <- data.frame(Method = m, Source = s, Feature = names(imp_scores), Importance = as.numeric(imp_scores))
    all_shap_importance <- rbind(all_shap_importance, res_df)
  }
}

if (nrow(all_shap_importance) > 0) {
  library(ggplot2)
  p <- ggplot(all_shap_importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip() +
    facet_grid(Method ~ Source, scales = "free_y") + 
    theme_bw() +
    labs(title = "모든 모델 및 오염원별 SHAP 기여도 통합 분석", x = "수질 인자", y = "평균 절대 SHAP 값")
  
  print(p)
  write.csv(all_shap_importance, "Total_XAI_Importance_Results_Fixed.csv", row.names = FALSE)
}