# =====================================================================
# [Step 0] 환경 설정 및 필수 패키지 로드
# =====================================================================
# 1) 기본 데이터 처리 및 다중 회귀 ML 패키지
if (!require("gtools")) install.packages("gtools")
if (!require("dplyr")) install.packages("dplyr")
if (!require("smotefamily")) install.packages("smotefamily")
if (!require("ranger")) install.packages("ranger")
if (!require("xgboost")) install.packages("xgboost")
if (!require("lightgbm")) install.packages("lightgbm")
if (!require("e1071")) install.packages("e1071")
if (!require("kknn")) install.packages("kknn")
if (!require("rpart")) install.packages("rpart")
if (!require("glmnet")) install.packages("glmnet")

# 2) 고도화 AI 및 XAI 패키지
if (!require("brnn")) install.packages("brnn")
if (!require("tabnet")) install.packages("tabnet")
if (!require("fastshap")) install.packages("fastshap")
if (!require("torch")) install.packages("torch")

library(gtools); library(dplyr); library(smotefamily)
library(ranger); library(xgboost); library(lightgbm)
library(e1071); library(kknn); library(rpart); library(glmnet)
library(brnn); library(tabnet); library(fastshap); library(torch)

# =====================================================================
# [Step 1] 가상 혼합 데이터 생성 및 분리
# =====================================================================
# 1. 가상 데이터 생성 함수 (5개 오염원: Manure, Sewage, Fertilizer, Soil, Rain)
generate_virtual_data <- function(n_samples = 5000) {
  set.seed(2026)
  # 기여율 생성 (알파값이 1이면 모든 비율이 균등하게 생성됨)
  props <- rdirichlet(n_samples, alpha = c(1, 1, 1, 1, 1))
  colnames(props) <- c("p_Manure", "p_Sewage", "p_Fertilizer", "p_Soil", "p_Rain")
  
  # 가상의 오염원별 평균값 설정 (d15N, d18O, Cl, TOC 등 4개 특성 가정)
  # 실제 연구 시에는 이 부분에 문헌값이나 채취한 Source DB의 평균/분산을 적용
  src_mean <- matrix(c(
    15.0, 5.0, 100, 20,  # Manure
    10.0, 2.0, 80,  10,  # Sewage
    -2.0, 15.0, 40,  2,  # Fertilizer
    5.0,  5.0, 10,  5,   # Soil
    -5.0, -10.0, 2,  1   # Rain
  ), nrow=5, byrow=TRUE)
  
  # 혼합 시료 데이터 계산 (Linear Mixing + 가우시안 노이즈)
  features <- props %*% src_mean + matrix(rnorm(n_samples * 4, 0, 1), ncol=4)
  colnames(features) <- c("d15N", "d18O", "Cl", "TOC")
  
  return(data.frame(features, props))
}

# 데이터셋 구축
dataset <- generate_virtual_data(5000)
train_idx <- sample(1:nrow(dataset), 0.8 * nrow(dataset))
train_data <- dataset[train_idx, ]
test_data  <- dataset[-train_idx, ]

# X(피처)와 Y(기여율 정답) 분리
X_train <- as.matrix(train_data[, 1:4])
Y_train <- as.matrix(train_data[, 5:9])
X_test  <- as.matrix(test_data[, 1:4])
Y_test  <- as.matrix(test_data[, 5:9])

# =====================================================================
# [Step 1-1] SMOTE 데이터 불균형 처리 (선택적 활용)
# =====================================================================
prep_smote_data <- dataset %>%
  mutate(target = ifelse(p_Rain >= 0.5, "Minority", "Majority")) %>%
  select(d15N, d18O, Cl, TOC, target)

smote_result <- SMOTE(X = prep_smote_data[, 1:4], 
                      target = prep_smote_data$target, 
                      K = 5, dup_size = 0)

dataset_balanced <- smote_result$data %>% rename(target = class)
X_balanced <- as.matrix(dataset_balanced[, 1:4]) 

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
    
    # -----------------------------------------------------------------
    # Base ML Models (전통적 기계학습 모델)
    # -----------------------------------------------------------------
    if (method == "RF") {
      # [1] Random Forest (랜덤 포레스트)
      # 원리: 수많은 의사결정 트리를 만들고(Bagging), 그 결과를 평균 내어 예측
      # 특징: 이상치(Outlier)와 노이즈에 강하며, 과적합을 방지함. 환경 데이터에서 가장 범용적으로 쓰임.
      fit <- ranger(y ~ ., data = data_tr)
      predictions[, i] <- predict(fit, df_te)$predictions
      
    } else if (method == "ExtraTrees") {
      # [2] Extra Trees (엑스트라 트리)
      # 원리: RF와 유사하나, 트리를 분할할 때 최적의 기준이 아닌 '무작위(Random)' 기준을 사용하여 분할
      # 특징: RF보다 연산 속도가 빠르고 분산(Variance)을 더 낮춰 일반화 성능이 좋을 때가 많음.
      fit <- ranger(y ~ ., data = data_tr, splitrule = "extratrees")
      predictions[, i] <- predict(fit, df_te)$predictions
      
    } else if (method == "XGB") {
      # [3] XGBoost (익스트림 그래디언트 부스팅)
      # 원리: 이전 트리의 오차(Residual)를 다음 트리가 순차적으로 보완해 나가는 부스팅(Boosting) 기법
      # 특징: 정형(표) 데이터 분석에서 가장 높은 예측 정확도를 자랑함. 하이퍼파라미터 튜닝이 중요함.
      fit <- xgboost(data = X_tr, label = target_y, nrounds = 50, verbose = 0, objective = "reg:squarederror")
      predictions[, i] <- predict(fit, X_te)
      
    } else if (method == "LGBM") {
      # [4] LightGBM (라이트 그래디언트 부스팅 머신)
      # 원리: XGBoost와 같으나, 트리를 수평이 아닌 수직(Leaf-wise)으로 비대칭적으로 확장함
      # 특징: XGBoost보다 학습 속도가 훨씬 빠르고 메모리 사용량이 적어 대용량 수질 데이터 처리에 유리함.
      dtrain <- lgb.Dataset(data = X_tr, label = target_y)
      fit <- lgb.train(params = list(objective = "regression"), data = dtrain, nrounds = 50, verbose = -1)
      predictions[, i] <- predict(fit, X_te)
      
    } else if (method == "SVM") {
      # [5] SVM (Support Vector Machine)
      # 원리: 데이터를 고차원 공간으로 보내어, 오류를 최소화하는 최적의 초평면(Hyperplane)을 찾음
      # 특징: 데이터 갯수가 적을 때도 우수한 성능을 보이며, 비선형 관계 매핑(RBF 커널)에 탁월함.
      fit <- svm(y ~ ., data = data_tr, kernel = "radial")
      predictions[, i] <- predict(fit, df_te)
      
    } else if (method == "KNN") {
      # [6] KNN (K-Nearest Neighbors)
      # 원리: 예측하려는 미지 시료와 가장 물리적 특성이 비슷한 K개의 훈련 데이터를 찾아 평균을 냄
      # 특징: 학습 과정이 없고 직관적이나, 변수 간 스케일 차이에 매우 민감함(스케일링 필수).
      fit <- kknn(y ~ ., train = data_tr, test = df_te, k = 5)
      predictions[, i] <- fit$fitted.values
      
    } else if (method == "Tree") {
      # [7] Decision Tree (의사결정 트리)
      # 원리: 스무고개처럼 질문을 던져 데이터를 분할하며 예측
      # 특징: 결과 해석이 가장 쉽고 직관적이나, 정확도가 떨어지고 과적합되기 쉬움(비교군으로 주로 사용).
      fit <- rpart(y ~ ., data = data_tr)
      predictions[, i] <- predict(fit, df_te)
      
    } else if (method == "ElasticNet") {
      # [8] Elastic Net (엘라스틱 넷)
      # 원리: 선형 회귀에 페널티(L1, L2)를 부여하여 의미 없는 변수의 영향력을 0으로 만듦
      # 특징: 변수 간 상관관계(다중공선성)가 높은 동위원소나 이온 데이터 분석 시 모델을 안정화시킴.
      cv_fit <- cv.glmnet(X_tr, target_y, alpha = 0.5)
      predictions[, i] <- as.numeric(predict(cv_fit, X_te, s = "lambda.min"))
      
      # -----------------------------------------------------------------
      # Advanced AI Models (고도화 인공지능 모델)
      # -----------------------------------------------------------------
    } else if (method == "BNN") {
      # [9] BNN (Bayesian Neural Network / 베이지안 신경망)
      # 원리: 신경망의 가중치(Weight)를 고정된 값이 아닌 확률 분포로 추정 (Bayesian 추론 결합)
      # 특징: 관측 데이터의 노이즈와 불확실성을 반영하므로, 이상치에 강건하며 결과의 신뢰 구간 산출이 가능함.
      fit <- brnn(X_tr, target_y, neurons = 5, verbose = FALSE)
      predictions[, i] <- predict(fit, X_te)
      
    } else if (method == "TabNet") {
      # [10] TabNet (표 데이터 특화 어텐션 네트워크)
      # 원리: 순차적 어텐션(Sequential Attention) 메커니즘을 통해 각 예측 단계마다 중요한 변수를 스스로 선택함
      # 특징: 딥러닝이면서도 설명 가능성(어떤 변수가 중요했는지)을 제공하며, 비선형 화학 반응 추적에 강력함.
      fit <- tabnet_fit(y ~ ., data = data_tr, epochs = 10, verbose = FALSE)
      predictions[, i] <- as.numeric(predict(fit, df_te)$.pred)
      
    } else if (method == "GNN_Spatial") {
      # [11] GNN_Spatial (공간 그래프 신경망 개념의 근사)
      # 원리: 각 측정 지점을 노드(Node)로, 하천의 흐름을 엣지(Edge)로 구성하여 상/하류 공간 정보를 학습
      # 특징: 본 코드에서는 RF로 근사하였으나, 실제 적용 시 인접 행렬(Adjacency Matrix)을 구축하여 
      # 공간적 확산 및 희석 기작을 모델에 직접 반영할 수 있음.
      fit <- ranger(y ~ ., data = data_tr)
      predictions[, i] <- predict(fit, df_te)$predictions 
    }
  }
  
  # 후처리: 물리적 제약 조건 반영 (음수 기여율을 0으로 처리 및 기여율 총합을 1로 정규화)
  predictions[predictions < 0] <- 0
  pred_sum <- rowSums(predictions)
  predictions_normalized <- predictions / ifelse(pred_sum == 0, 1, pred_sum)
  colnames(predictions_normalized) <- colnames(Y_tr)
  
  return(predictions_normalized)
}

# =====================================================================
# [Step 3] 전체 모델 실행 및 성능 평가
# =====================================================================
# 실행할 알고리즘 전체 목록
all_methods <- c("RF", "ExtraTrees", "XGB", "LGBM", "SVM", "KNN", "Tree", 
                 "ElasticNet", "BNN", "TabNet", "GNN_Spatial")

results_list <- list()

cat("\n=== [알고리즘 학습 시작] ===\n")
for (m in all_methods) {
  message(paste0("학습 진행 중: ", m))
  results_list[[m]] <- train_and_predict_master(m, X_train, Y_train, X_test)
}

# MAE 평가 함수
calculate_mae <- function(true_Y, pred_Y) { mean(abs(true_Y - pred_Y)) }
calculate_rmse <- function(true_Y, pred_Y) { sqrt(mean((true_Y - pred_Y)^2)) }
calculate_mape <- function(true_Y, pred_Y) { 100*mean(abs((true_Y - pred_Y)/true_Y)) }

cat("\n=== [모델별 성능 평가 (MAE)] ===\n")
for (m in all_methods) {
  mae_val <- calculate_mae(Y_test, results_list[[m]])
  cat(sprintf("%-15s MAE: %.6f\n", m, mae_val))
}

cat("\n=== [모델별 성능 평가 (RMSE)] ===\n")
for (m in all_methods) {
  rmse_val <- calculate_rmse(Y_test, results_list[[m]])
  cat(sprintf("%-15s RMSE: %.6f\n", m, rmse_val))
}

cat("\n=== [모델별 성능 평가 (MAPE)] ===\n")
for (m in all_methods) {
  mape_val <- calculate_mape(Y_test, results_list[[m]])
  cat(sprintf("%-15s MAPE: %.6f\n", m, mape_val))
}

# =====================================================================
# [Step 4] 미지 시료 오염원 기여율 추정 및 결과 저장
# =====================================================================

# 분석에 사용할 11가지 전체 모델 리스트 정의
all_methods <- c("RF", "ExtraTrees", "XGB", "LGBM", "SVM", "KNN", 
                 "Tree", "ElasticNet", "BNN", "TabNet", "GNN_Spatial")

# ---------------------------------------------------------------------
# 4-1. 임의의 단일 시료(Single Sample) 예측 프로세스
# ---------------------------------------------------------------------
cat("\n=== [4-1. 단일 임의 시료 기반 알고리즘별 추정 결과] ===\n")

# 임의 시료 생성 (d15N, d18O, Cl, TOC)
single_matrix <- matrix(c(12.5, 3.2, 90, 15), nrow = 1)
colnames(single_matrix) <- c("d15N", "d18O", "Cl", "TOC")

# 모든 모델에 대해 반복 예측 수행 및 출력
for (m in all_methods) {
  cat(sprintf("[%s] 모델 추정 중... ", m))
  
  # 기존에 정의된 train_and_predict_master 함수 호출
  single_pred <- train_and_predict_master(m, X_train, Y_train, single_matrix)
  
  cat("완료\n")
  print(round(single_pred, 4))
  cat("--------------------------------------------------\n")
}


# ---------------------------------------------------------------------
# 4-2. CSV 파일을 이용한 대량(Batch) 예측 및 병합 저장 프로세스
# ---------------------------------------------------------------------
cat("\n=== [4-2. CSV 미지 시료 배치 분석 및 모델별 결과 저장] ===\n")

if (file.exists("unknown_data.csv")) {
  # 1) 미지 시료 데이터 로드
  unknown_samples_df <- read.csv("unknown_data.csv", header = TRUE)
  
  # 2) 모델 입력용 행렬 변환 (컬럼명 확인 필수)
  # 파일 내 컬럼명이 d15N, d18O, Cl, TOC로 되어 있다고 가정합니다.
  unknown_matrix <- as.matrix(unknown_samples_df[, c("d15N", "d18O", "Cl", "TOC")])
  
  # 3) 11개 모델 순회하며 예측 및 파일 저장
  for (m in all_methods) {
    cat(sprintf(">>>> [%s] 모델 배치 분석 진행 중... ", m))
    
    # 모델 예측 수행
    batch_preds <- train_and_predict_master(m, X_train, Y_train, unknown_matrix)
    
    # 원 데이터(unknown_samples_df)와 예측 결과(batch_preds) 병합
    # 소수점 4자리까지 반올림하여 가독성 확보
    final_output <- cbind(unknown_samples_df, round(batch_preds, 4))
    
    # 모델명을 포함한 파일명 생성 및 저장
    output_filename <- paste0("Final_Contribution_", m, ".csv")
    write.csv(final_output, output_filename, row.names = FALSE)
    
    cat(sprintf("저장 완료: %s\n", output_filename))
  }
  
  cat("\n모든 모델에 대한 분석 및 CSV 저장이 성공적으로 완료되었습니다.\n")
  
} else {
  warning("파일 오류: 'unknown_data.csv'가 작업 디렉토리에 존재하지 않습니다. 4-2 단계를 건너뜁니다.")
}

# =====================================================================
# [Step 5] XAI (SHAP) 의사결정 시각화
# =====================================================================
# 원리: 게임 이론(Shapley Values)을 바탕으로, 최종 기여율 산출에 특정 수질 인자가 얼마나 공헌했는지 수치화함
# 특징: 블랙박스 모델(AI)의 판단 근거를 투명하게 공개하여 분석 결과의 과학적 객관성 및 정책적 설득력을 확보함
message("\n=== [SHAP 변수 기여도 시각화 준비 완료] ===")
rf_model <- ranger(p_Manure ~ ., data = data.frame(p_Manure = Y_train[,1], X_train))
shap_values <- explain(rf_model, X = as.data.frame(X_train), nsim = 10, 
                       pred_wrapper = function(m, x) predict(m, x)$predictions)
autoplot(shap_values)

# =====================================================================
# [Step 5-1] 통합 XAI (SHAP) 분석: 모든 모델 & 모든 오염원 대상 (오류 수정본)
# =====================================================================
library(ggplot2)
library(fastshap)
library(tidyr)

# 1. SHAP 계산을 위한 통합 예측 래퍼 함수 (입력 데이터 형식 오류 해결)
p_wrapper <- function(object, newdata) {
  # 기본적으로 행렬과 데이터 프레임 두 가지 버전을 준비
  newdata_mat <- as.matrix(newdata)
  newdata_df  <- as.data.frame(newdata)
  
  if (inherits(object, "ranger")) {
    # [RF, ExtraTrees, GNN_Spatial]
    return(predict(object, data = newdata_df)$predictions)
    
  } else if (inherits(object, "xgb.Booster") || inherits(object, "lgb.Booster")) {
    # [XGBoost, LightGBM] - 행렬 입력 필수
    return(predict(object, newdata = newdata_mat))
    
  } else if (inherits(object, "cv.glmnet")) {
    # [ElasticNet] - cv.glmnet은 반드시 matrix 형식을 newx 인자로 받아야 함
    # s = "lambda.min"을 명시하여 최적의 예측치 추출
    return(as.numeric(predict(object, newx = newdata_mat, s = "lambda.min")))
    
  } else if (inherits(object, "brnn")) {
    # [BNN] - 행렬 형식 선호
    return(as.numeric(predict(object, newdata_mat)))
    
  } else if (inherits(object, "tabnet_fit")) {
    # [TabNet]
    return(as.numeric(predict(object, newdata_df)$.pred))
    
  } else {
    # [SVM, Tree 등 기타 모델] - 데이터 프레임 기반 예측
    preds <- predict(object, newdata = newdata_df)
    return(as.numeric(preds))
  }
}

cat("\n=== [5. 모든 모델 및 오염원에 대한 통합 XAI 분석 시작] ===\n")

all_shap_importance <- data.frame()
target_methods <- c("RF", "ExtraTrees", "XGB", "LGBM", "SVM", "KNN", 
                    "Tree", "ElasticNet", "BNN", "TabNet", "GNN_Spatial")

# SHAP 계산용 배경 데이터 (속도와 안정성을 위해 50개 샘플 행렬화)
X_explain <- X_train[1:50, ]

for (m in target_methods) {
  message(sprintf(">>>> 알고리즘 분석 중: [%s]", m))
  
  for (s in colnames(Y_train)) {
    # 각 조합별 모델 임시 학습 (객체 확보)
    data_tmp <- data.frame(y = Y_train[, s], X_train)
    
    # 모델 학습 로직 (cv.glmnet 등 매개변수 최적화 포함)
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
    
    # KNN 또는 학습 실패 시 건너뛰기
    if (is.null(fit_obj) || m == "KNN") next 
    
    # SHAP 값 계산 (오류 발생 시 해당 조합만 건너뛰도록 설정)
    shap_res <- tryCatch({
      fastshap::explain(
        fit_obj, 
        X = X_explain, 
        nsim = 10, 
        pred_wrapper = p_wrapper,
        adjust = TRUE
      )
    }, error = function(e) {
      message(sprintf("Warning: [%s-%s] 조합에서 SHAP 계산 실패", m, s))
      return(NULL)
    })
    
    if (is.null(shap_res)) next
    
    imp_scores <- colMeans(abs(shap_res))
    res_df <- data.frame(
      Method = m,
      Source = s,
      Feature = names(imp_scores),
      Importance = as.numeric(imp_scores)
    )
    all_shap_importance <- rbind(all_shap_importance, res_df)
  }
}

# 2. 통합 시각화 및 데이터 저장
if (nrow(all_shap_importance) > 0) {
  p <- ggplot(all_shap_importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip() +
    facet_grid(Method ~ Source, scales = "free_y") + 
    theme_bw() +
    labs(title = "모든 모델 및 오염원별 SHAP 기여도 통합 분석",
         x = "수질 인자", y = "평균 절대 SHAP 값")
  
  print(p)
  write.csv(all_shap_importance, "Total_XAI_Importance_Results_Fixed.csv", row.names = FALSE)
} else {
  message("분석된 결과 데이터가 없습니다.")
}
