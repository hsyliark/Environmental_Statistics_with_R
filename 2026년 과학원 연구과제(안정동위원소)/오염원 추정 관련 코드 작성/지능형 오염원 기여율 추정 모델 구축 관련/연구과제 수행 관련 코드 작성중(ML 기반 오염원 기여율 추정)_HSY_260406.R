## Step 1. 가상 혼합 데이터 생성 (Data Generation)

# 필요 패키지 로드
library(gtools)
library(dplyr)

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




## Step 1-1. 데이터 불균형 문제 시 활용

# 1. 필요 패키지 설치 및 로드
if (!require("smotefamily")) install.packages("smotefamily")
library(smotefamily)
library(dplyr)

# ---------------------------------------------------------
# 2. SMOTE 적용을 위한 데이터 변환 (가정: Rain 기여율이 높은 데이터가 부족함)
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
# 3. SMOTE 실행
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
# 4. 결과 활용 (학습용 데이터셋 재구성)
# ---------------------------------------------------------
# SMOTE로 생성된 데이터(target == "Minority")들은 기존 데이터들 사이의 
# 값을 보간하여 만들어진 새로운 가상 시료들입니다.

# 수치형 변수만 다시 추출하여 분석 모델에 투입
X_balanced <- as.matrix(dataset_balanced[, 1:4])
# 이후 이 데이터를 이전에 작성해드린 train_and_predict_ml 함수에 넣어 
# 오염원 기여율 모델의 학습 데이터로 활용하시면 됩니다.




## Step 2. 기계학습(ML) 알고리즘 적용 (다중 회귀 + 정규화)

# 필요 패키지 로드
library(ranger)      # RF, Extra Trees
library(xgboost)     # XGBoost
library(lightgbm)    # LightGBM
library(e1071)       # SVM
library(kknn)        # KNN
library(rpart)       # Decision Tree
library(glmnet)      # Elastic Net

# ---------------------------------------------------------
# 8종 기계학습 모델 학습 및 예측 통합 함수
# ---------------------------------------------------------
train_and_predict_ml <- function(method, X_tr, Y_tr, X_te) {
  n_sources <- ncol(Y_tr)
  predictions <- matrix(0, nrow = nrow(X_te), ncol = n_sources)
  
  # 각 오염원(End-member) 기여율별로 반복 학습
  for (i in 1:n_sources) {
    target_y <- Y_tr[, i]
    
    # 데이터 프레임 형태 준비 (일부 모델용)
    data_tr <- data.frame(y = target_y, X_tr)
    data_te <- data.frame(X_te)
    
    if (method == "RF") {
      # 1. Random Forest
      fit <- ranger(y ~ ., data = data_tr)
      predictions[, i] <- predict(fit, data_te)$predictions
      
    } else if (method == "ExtraTrees") {
      # 2. Extra Trees (Extremely Randomized Trees)
      fit <- ranger(y ~ ., data = data_tr, splitrule = "extratrees")
      predictions[, i] <- predict(fit, data_te)$predictions
      
    } else if (method == "XGB") {
      # 3. XGBoost
      fit <- xgboost(data = X_tr, label = target_y, nrounds = 50, verbose = 0, objective = "reg:squarederror")
      predictions[, i] <- predict(fit, X_te)
      
    } else if (method == "LGBM") {
      # 4. LightGBM
      dtrain <- lgb.Dataset(data = X_tr, label = target_y)
      fit <- lgb.train(params = list(objective = "regression"), data = dtrain, nrounds = 50, verbose = -1)
      predictions[, i] <- predict(fit, X_te)
      
    } else if (method == "SVM") {
      # 5. SVM (Support Vector Machine)
      fit <- svm(y ~ ., data = data_tr, kernel = "radial")
      predictions[, i] <- predict(fit, data_te)
      
    } else if (method == "KNN") {
      # 6. KNN (K-Nearest Neighbors)
      fit <- kknn(y ~ ., train = data_tr, test = data_te, k = 5)
      predictions[, i] <- fit$fitted.values
      
    } else if (method == "Tree") {
      # 7. Decision Tree
      fit <- rpart(y ~ ., data = data_tr)
      predictions[, i] <- predict(fit, data_te)
      
    } else if (method == "ElasticNet") {
      # 8. Elastic Net (Lasso + Ridge 절충형)
      # alpha = 0.5 설정, 내부 교차검증(cv)을 통해 최적 람다 선정
      cv_fit <- cv.glmnet(X_tr, target_y, alpha = 0.5)
      predictions[, i] <- as.numeric(predict(cv_fit, X_te, s = "lambda.min"))
    }
  }
  
  # --- 후처리 프로세스 ---
  # 1. 음수 값 제거 (물리적 기여율은 0 미만일 수 없음)
  predictions[predictions < 0] <- 0
  
  # 2. 합계가 1(100%)이 되도록 정규화
  pred_sum <- rowSums(predictions)
  # 합이 0인 경우를 대비해 나눗셈 처리 (0으로 나누기 방지)
  predictions_normalized <- predictions / ifelse(pred_sum == 0, 1, pred_sum)
  
  colnames(predictions_normalized) <- colnames(Y_tr)
  return(predictions_normalized)
}

# ---------------------------------------------------------
# 활용 예시
# ---------------------------------------------------------
# 모델 리스트 정의
methods <- c("RF", "ExtraTrees", "XGB", "LGBM", "SVM", "KNN", "Tree", "ElasticNet")

# 반복문을 통한 모델별 결과 저장 (예시)
results_list <- list()
for (m in methods) {
  message(paste0("현재 학습 중인 모델: ", m))
  results_list[[m]] <- train_and_predict_ml(m, X_train, Y_train, X_test)
}




## Step 3. 평가 및 미지 시료(Unknown) 적용

# 1. 평가 함수 (MAE 계산)
calculate_mae <- function(true_Y, pred_Y) {
  mean(abs(true_Y - pred_Y))
}

# 2. 반복문을 통해 모든 ML 모델의 MAE 출력
cat("\n=== [모델별 성능 평가 (MAE)] ===\n")
for (m in methods) {
  # results_list에서 각 모델의 예측값을 가져옴
  current_preds <- results_list[[m]]
  
  # MAE 계산 및 출력
  mae_val <- calculate_mae(Y_test, current_preds)
  cat(sprintf("%-12s MAE: %.6f\n", m, mae_val))
}

# 3. 미지 시료 (국가 수질측정망 데이터라고 가정) 예측
# matrix 생성 시 컬럼명을 X_train과 동일하게 맞춰줍니다.
unknown_sample <- matrix(c(12.5, 3.2, 90, 15), nrow=1)
colnames(unknown_sample) <- colnames(X_train)

# 미지 시료를 XGBoost로 예측 (이미 학습된 결과를 쓰는 게 아니라 함수를 재호출)
unknown_preds_xgb <- train_and_predict_ml("XGB", X_train, Y_train, unknown_sample)

cat("\n=== [미지 시료 오염원 기여율 추정 (XGBoost)] ===\n")
print(round(unknown_preds_xgb, 4))




# ---------------------------------------------------------
# 3-1. 실제 미지 시료(CSV 파일) 불러오기 및 예측
# ---------------------------------------------------------
# (주의) 워킹 디렉토리에 'unknown_data.csv' 파일이 있다고 가정합니다.
# 파일 경로는 실제 파일이 있는 위치로 지정해주세요. (예: "C:/data/unknown_data.csv")

# CSV 파일 불러오기
unknown_samples_df <- read.csv("unknown_data.csv", header = TRUE)

# 분석에 필요한 독립변수(Feature)만 추출하여 행렬(matrix)로 변환
# ★중요: 학습 데이터(X_train)에 사용된 변수명 및 순서와 정확히 일치해야 합니다.
unknown_matrix <- as.matrix(unknown_samples_df[, c("d15N", "d18O", "Cl", "TOC")])

# 불러온 미지 시료 전체를 대상으로 모델(예: XGBoost) 예측 수행
unknown_preds_xgb <- train_and_predict_ml("XGB", X_train, Y_train, unknown_matrix)

cat("\n=== [실제 미지 시료 오염원 기여율 추정 결과 (XGBoost)] ===\n")
# 가독성을 위해 소수점 4자리까지 반올림
final_results <- round(unknown_preds_xgb, 4)

# 원본 데이터에 예측된 기여율 결과를 새로운 열(Column)로 병합
# 이렇게 하면 각 시료별(지점별/날짜별) 특성과 기여율을 한눈에 확인할 수 있습니다.
final_output_df <- cbind(unknown_samples_df, final_results)

# 결과 확인 (상위 6개 행만 출력)
print(head(final_output_df))

# 필요하다면 최종 결과를 다시 CSV 파일로 내보내어 엑셀에서 확인 가능
write.csv(final_output_df, "final_prediction_results.csv", row.names = FALSE)
