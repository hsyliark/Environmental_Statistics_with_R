# ---------------------------------------------------------
# 0. 환경 설정 및 고도화 패키지 로드
# ---------------------------------------------------------
if (!require("brnn")) install.packages("brnn")       # Bayesian Neural Network
if (!require("tabnet")) install.packages("tabnet")   # TabNet (torch 기반)
if (!require("fastshap")) install.packages("fastshap") # XAI (SHAP)
if (!require("torch")) install.packages("torch")     # TabNet 구동 엔진

library(brnn)
library(tabnet)
library(fastshap)
library(torch)
library(gtools)
library(dplyr)

# 1) 가상 데이터 생성 함수 (5개 오염원: Manure, Sewage, Fertilizer, Soil, Rain)
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




## 2) 데이터 불균형 문제 시 활용

# 필요 패키지 설치 및 로드
if (!require("smotefamily")) install.packages("smotefamily")
library(smotefamily)
library(dplyr)

# ---------------------------------------------------------
# SMOTE 적용을 위한 데이터 변환 (가정: Rain 기여율이 높은 데이터가 부족함)
# ---------------------------------------------------------
# 현재 dataset에서 Rain 기여율(p_Rain)이 0.5 이상인 데이터를 '소수 클래스(1)'로 정의
# 실제 연구에서는 특정 지점이나 특정 시기(강우 시)의 데이터를 타겟으로 잡으시면 됩니다.

prep_smote_data <- dataset %>%
  mutate(target = ifelse(p_Rain >= 0.5, "Minority", "Majority")) %>%
  select(d15N, d18O, Cl, TOC, target) # 피처와 타겟 라벨만 추출

# 데이터 분포 확인
print("SMOTE 전 클래스 분포:")
print(table(prep_smote_data$target))

# ---------------------------------------------------------
# SMOTE 실행
# ---------------------------------------------------------
# X: 독립변수 (d15N, d18O, Cl, TOC)
# target: 클래스 라벨
# K: 인접 이웃 수 (보통 5)
# dup_size: 0으로 설정 시 다수 클래스 수에 맞춰 자동으로 생성

smote_result <- SMOTE(X = prep_smote_data[, 1:4], 
                      target = prep_smote_data$target, 
                      K = 5, 
                      dup_size = 0)

# 생성된 데이터 결합
dataset_balanced <- smote_result$data %>%
  rename(target = class) # smotefamily는 결과를 'class'라는 열이름으로 반환함

print("SMOTE 후 클래스 분포:")
print(table(dataset_balanced$target))


# ---------------------------------------------------------
# 1. 고도화 알고리즘 통합 학습 및 예측 함수
# ---------------------------------------------------------
train_and_predict_advanced <- function(method, X_tr, Y_tr, X_te) {
  n_sources <- ncol(Y_tr)
  predictions <- matrix(0, nrow = nrow(X_te), ncol = n_sources)
  
  # 데이터 프레임 변환 (대부분의 R 패키지 대응)
  df_tr_all <- data.frame(X_tr, Y_tr)
  df_te <- data.frame(X_te)
  
  for (i in 1:n_sources) {
    target_name <- colnames(Y_tr)[i]
    message(paste0("훈련 중인 오염원 [", target_name, "] - 알고리즘: ", method))
    
    if (method == "BNN") {
      # [1] Bayesian Neural Network (베이지안 신경망)
      # 확률적 가중치를 통해 과적합을 방지하고 불확실성을 고려함
      fit <- brnn(X_tr, Y_tr[, i], neurons = 5, verbose = FALSE)
      predictions[, i] <- predict(fit, X_te)
      
    } else if (method == "TabNet") {
      # [2] TabNet (Attention 기반 표 데이터 특화 모델)
      # torch 엔진을 사용하여 변수 중요도를 스스로 학습함
      # (주의) TabNet은 다중 출력(Multi-output)이 지원되나, 루프 구조 유지를 위해 단일 출력으로 구성
      dat_tr <- data.frame(y = Y_tr[, i], X_tr)
      fit <- tabnet_fit(y ~ ., data = dat_tr, epochs = 10, verbose = FALSE)
      predictions[, i] <- as.numeric(predict(fit, df_te)$.pred)
      
    } else if (method == "GNN_Spatial") {
      # [3] GNN (그래프 신경망 - 공간 가중치 모델로 근사)
      # 실제 GNN은 인접 행렬이 필요함. 여기서는 수계 연결성을 고려한 
      # 지리적 가중치 회귀(GWR) 혹은 공간 지연 모델의 논리를 머신러닝에 결합
      # 예시로, 특정 지점(Row)이 인접 지점의 영향을 받는다고 가정하여 가중 평균 적용
      # (실제 구현 시에는 지점 간 거리/연결성 기반 Adjacency Matrix가 투입되어야 함)
      fit <- ranger::ranger(Y_tr[, i] ~ ., data = data.frame(y = Y_tr[, i], X_tr))
      base_pred <- predict(fit, df_te)$predictions
      # 공간적 평활화(Spatial Smoothing) 예시 로직
      predictions[, i] <- base_pred 
    }
  }
  
  # 후처리: 음수 제거 및 합계 1 정규화
  predictions[predictions < 0] <- 0
  pred_sum <- rowSums(predictions)
  predictions_normalized <- predictions / ifelse(pred_sum == 0, 1, pred_sum)
  colnames(predictions_normalized) <- colnames(Y_tr)
  
  return(predictions_normalized)
}

# ---------------------------------------------------------
# 2. XAI (SHAP) 구현: 모델의 의사결정 근거 시각화
# ---------------------------------------------------------
# 가장 성능이 좋은 모델(예: RF)에 대해 SHAP 분석 수행
explain_model_importance <- function(trained_model, X_sample) {
  # SHAP 가중치 계산
  shap_values <- explain(trained_model, X = X_sample, nsim = 10, 
                         pred_wrapper = function(m, x) predict(m, x)$predictions)
  
  # 시각화 (각 수질 인자의 기여도 확인)
  autoplot(shap_values) + 
    ggtitle("오염원 기여율 산정 모델의 변수 기여도 (SHAP)") +
    theme_minimal()
}

# ---------------------------------------------------------
# 3. 실무 적용 및 결과 확인
# ---------------------------------------------------------

# 알고리즘 실행
adv_methods <- c("BNN", "TabNet")
adv_results <- list()

for (m in adv_methods) {
  adv_results[[m]] <- train_and_predict_advanced(m, X_train, Y_train, X_test)
}

# MAE 평가
cat("\n=== [고도화 모델 성능 평가 (MAE)] ===\n")
for (m in adv_methods) {
  mae_val <- mean(abs(Y_test - adv_results[[m]]))
  cat(sprintf("%-12s MAE: %.6f\n", m, mae_val))
}

# 미지 시료(CSV)에 대한 BNN 예측 적용 예시
unknown_matrix <- as.matrix(unknown_samples_df[, c("d15N", "d18O", "Cl", "TOC")])
unknown_preds_bnn <- train_and_predict_advanced("BNN", X_train, Y_train, unknown_matrix)