# ==============================================================================
# 오염원 기여율 평가 및 교차 검증 종합 함수 (보완판)
# 주요 특징:
#   1) 단일 시료(1D) 및 다중 시료/전체 결합 벡터(Flattened Vector) 모두 직접 입력 가능
#   2) 행렬/데이터프레임 형태도 자동 벡터 변환하여 교차 검증 지표 일괄 산출
#   3) 기본 평가지표(TVD, MAE, RMSE, PBIAS, Aitchison) + 교차 검증 지표(Lin's CCC, R^2, Slope) 산출
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. 하위 보조 함수: Lin's Concordance Correlation Coefficient (CCC)
# ------------------------------------------------------------------------------
calc_lin_ccc <- function(x, y) {
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  var_x  <- var(x, na.rm = TRUE)
  var_y  <- var(y, na.rm = TRUE)
  cov_xy <- cov(x, y, use = "complete.obs")
  
  if ((var_x + var_y + (mean_x - mean_y)^2) == 0) return(1.0)
  ccc <- (2 * cov_xy) / (var_x + var_y + (mean_x - mean_y)^2)
  return(ccc)
}

# ------------------------------------------------------------------------------
# 1. 벡터/행렬 입력 기반 종합 평가지표 산출 함수
# ------------------------------------------------------------------------------
calc_contribution_metrics <- function(f_meas, f_theo, k_sources = NULL, eps = 1e-9) {
  # (1) 입력 데이터 벡터 변환 및 길이 검증
  vec_m <- as.numeric(as.matrix(f_meas))
  vec_t <- as.numeric(as.matrix(f_theo))
  
  if (length(vec_m) != length(vec_t)) {
    stop("실측값(모델1)과 이론값(모델2) 벡터의 길이가 일치해야 합니다.")
  }
  
  # (2) 백분율(0~100%) 입력 시 0~1 비율 단위 자동 정규화
  if (sum(vec_m) > 1.5 && is.null(k_sources)) vec_m <- vec_m / sum(vec_m)
  if (sum(vec_t) > 1.5 && is.null(k_sources)) vec_t <- vec_t / sum(vec_t)
  
  N <- length(vec_m) # 전체 데이터 포인트 수
  
  # (3) TVD (Total Variation Distance) 산출
  # - 단일 시료 또는 오염원 수(k_sources) 지정 여부에 따른 적절한 TVD 계산
  if (!is.null(k_sources) && (N %% k_sources == 0)) {
    n_samples <- N / k_sources
    mat_m <- matrix(vec_m, nrow = n_samples, byrow = TRUE)
    mat_t <- matrix(vec_t, nrow = n_samples, byrow = TRUE)
    tvd_vec <- 0.5 * rowSums(abs(mat_m - mat_t))
    tvd <- mean(tvd_vec) # 전체 시료 평균 TVD
  } else {
    tvd <- 0.5 * sum(abs(vec_m - vec_t)) # 단일 시료 TVD
  }
  
  # (4) MAE & RMSE 산출
  mae  <- mean(abs(vec_m - vec_t))
  rmse <- sqrt(mean((vec_m - vec_t)^2))
  
  # (5) Percent Bias (PBIAS, %)
  pbias_vec <- ((vec_m - vec_t) / ifelse(vec_t == 0, eps, vec_t)) * 100
  mean_abs_pbias <- mean(abs(pbias_vec))
  
  # (6) Aitchison Distance (Compositional Data Analysis)
  vec_m_safe <- ifelse(vec_m <= 0, eps, vec_m)
  vec_t_safe <- ifelse(vec_t <= 0, eps, vec_t)
  
  gm_m <- exp(mean(log(vec_m_safe)))
  gm_t <- exp(mean(log(vec_t_safe)))
  
  clr_m <- log(vec_m_safe / gm_m)
  clr_t <- log(vec_t_safe / gm_t)
  
  aitchison <- sqrt(sum((clr_m - clr_t)^2))
  
  # (7) 교차 검증 지표: Lin's CCC, 1:1 회귀분석 (R^2, Slope, Intercept)
  ccc_val <- calc_lin_ccc(vec_m, vec_t)
  
  fit <- lm(vec_m ~ vec_t)
  r_squared <- summary(fit)$r.squared
  slope     <- unname(coef(fit)[2])
  intercept <- unname(coef(fit)[1])
  
  # (8) 결과를 수치형 벡터 및 리스트 구조로 동시 반환
  summary_metrics_vec <- c(
    TVD = tvd,
    MAE = mae,
    RMSE = rmse,
    Mean_Abs_PBIAS_pct = mean_abs_pbias,
    Aitchison_Distance = aitchison,
    Lins_CCC = ccc_val,
    R_Squared = r_squared,
    Slope = slope,
    Intercept = intercept
  )
  
  return(list(
    Metrics_Vector = summary_metrics_vec, # 주요 지표 요약 벡터
    PBIAS_by_Item_pct = pbias_vec         # 개별 요소별 PBIAS 벡터
  ))
}

# ------------------------------------------------------------------------------
# 2. 다중 시료 배치 처리 및 통합 결과 산출 함수
# ------------------------------------------------------------------------------
calc_contribution_batch <- function(mat_meas, mat_theo, eps = 1e-9) {
  mat_m <- as.matrix(mat_meas)
  mat_t <- as.matrix(mat_theo)
  
  if (!all(dim(mat_m) == dim(mat_t))) {
    stop("실측값과 이론값 데이터의 행/열 차원이 동일해야 합니다.")
  }
  
  n_samples <- nrow(mat_m)
  k_sources <- ncol(mat_m)
  
  # 시료별 지표 산출
  res_df <- data.frame(
    Sample_ID = 1:n_samples,
    TVD = numeric(n_samples),
    MAE = numeric(n_samples),
    RMSE = numeric(n_samples),
    Mean_Abs_PBIAS_pct = numeric(n_samples),
    Aitchison_Dist = numeric(n_samples)
  )
  
  for (i in 1:n_samples) {
    res <- calc_contribution_metrics(mat_m[i, ], mat_t[i, ], eps = eps)
    res_df$TVD[i]                <- res$Metrics_Vector["TVD"]
    res_df$MAE[i]                <- res$Metrics_Vector["MAE"]
    res_df$RMSE[i]               <- res$Metrics_Vector["RMSE"]
    res_df$Mean_Abs_PBIAS_pct[i] <- res$Metrics_Vector["Mean_Abs_PBIAS_pct"]
    res_df$Aitchison_Dist[i]     <- res$Metrics_Vector["Aitchison_Distance"]
  }
  
  # 전체 결합 벡터 기반 교차 검증 지표 추가 산출
  overall_res <- calc_contribution_metrics(mat_m, mat_t, k_sources = k_sources, eps = eps)
  
  return(list(
    Sample_Batch_Results = res_df,
    Overall_Metrics_Vector = overall_res$Metrics_Vector
  ))
}

# ==============================================================================
# 사용 예시 1: 단일 시료 벡터 입력 (3종 및 4종 오염원)
# ==============================================================================
cat("\n--- [예시 1] 4종 오염원 단일 시료 평가 ---\n")
f_meas_4d <- c(14, 4, 73, 9)
f_theo_4d <- c(10.0, 3.5, 76.1, 10.4)

res_4d <- calc_contribution_metrics(f_meas_4d, f_theo_4d)
print(res_4d$Metrics_Vector)

# ==============================================================================
# 사용 예시 2: EMMTE vs MixSIAR 교차 검증 (벡터 형태 직접 입력)
# ==============================================================================
cat("\n--- [예시 2] EMMTE vs MixSIAR 전체 결과 벡터 직접 교차 검증 ---\n")

# 여러 시료 및 오염원에서 산출된 기여율 데이터 (1차원 벡터 형태)
vec_emmte   <- c(0.80, 0.15, 0.05,  0.40, 0.40, 0.20,  0.10, 0.70, 0.20)
vec_mixsiar <- c(0.78, 0.16, 0.06,  0.42, 0.38, 0.20,  0.11, 0.68, 0.21)

# k_sources = 3 (3종 혼합 조건 예시)
cross_val_res <- calc_contribution_metrics(vec_emmte, vec_mixsiar, k_sources = 3)

# 결과 수치 벡터 출력
print(round(cross_val_res$Metrics_Vector, 4))

# ==============================================================================
# 사용 예시 3: 데이터프레임/행률 일괄 배치 처리 및 종합 검증
# ==============================================================================
cat("\n--- [예시 3] 다중 시료 데이터프레임 일괄 평가 및 요약 ---\n")
df_emmte   <- data.frame(S1 = c(0.8, 0.5, 0.2), S2 = c(0.15, 0.3, 0.5), S3 = c(0.05, 0.2, 0.3))
df_mixsiar <- data.frame(S1 = c(0.78, 0.52, 0.21), S2 = c(0.16, 0.28, 0.49), S3 = c(0.06, 0.20, 0.30))

batch_res <- calc_contribution_batch(df_emmte, df_mixsiar)

cat("\n1. 시료별 평가 결과 테이블:\n")
print(batch_res$Sample_Batch_Results)

cat("\n2. 전체 통합 평가 벡터 지표 (Lin's CCC 및 회귀분석 포함):\n")
print(round(batch_res$Overall_Metrics_Vector, 4))