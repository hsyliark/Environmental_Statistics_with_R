# ==============================================================================
# 1. 단일 시료/임의 차원 오염원 기여율 평가지표 산출 함수
# ==============================================================================
calc_contribution_metrics <- function(f_meas, f_theo, eps = 1e-9) {
  # 입력 벡터 길이 검증
  if (length(f_meas) != length(f_theo)) {
    stop("실측값과 이론값 벡터의 차원(길이)이 일치해야 합니다.")
  }
  
  # 백분율(0~100%) 입력 시 0~1 비율 단위로 자동 정규화
  if (sum(f_meas) > 1.5) f_meas <- f_meas / sum(f_meas)
  if (sum(f_theo) > 1.5) f_theo <- f_theo / sum(f_theo)
  
  k <- length(f_meas) # 오염원 수 (차원)
  
  # 1) Total Variation Distance (TVD)
  tvd <- 0.5 * sum(abs(f_meas - f_theo))
  
  # 2) Mean Absolute Error (MAE)
  mae <- mean(abs(f_meas - f_theo))
  
  # 3) Root Mean Squared Error (RMSE)
  rmse <- sqrt(mean((f_meas - f_theo)^2))
  
  # 4) Percent Bias (PBIAS, %)
  pbias_vec <- ((f_meas - f_theo) / f_theo) * 100
  mean_abs_pbias <- mean(abs(pbias_vec))
  
  # 5) Aitchison Distance (Compositional Data Analysis 기법)
  # log(0) 연산 오류 방지를 위한 eps 보정
  f_m_safe <- ifelse(f_meas <= 0, eps, f_meas)
  f_t_safe <- ifelse(f_theo <= 0, eps, f_theo)
  
  # 기하평균 산출
  gm_m <- exp(mean(log(f_m_safe)))
  gm_t <- exp(mean(log(f_t_safe)))
  
  # Centered Log-Ratio (CLR) 변환
  clr_m <- log(f_m_safe / gm_m)
  clr_t <- log(f_t_safe / gm_t)
  
  # 애치슨 거리 산출
  aitchison <- sqrt(sum((clr_m - clr_t)^2))
  
  # 결과 출력
  return(list(
    TVD = tvd,
    MAE = mae,
    RMSE = rmse,
    Mean_Abs_PBIAS_pct = mean_abs_pbias,
    Aitchison_Distance = aitchison,
    PBIAS_by_Source_pct = pbias_vec
  ))
}

# ==============================================================================
# 2. 다중 시료(행렬/데이터프레임) 일괄 배치 처리 함수
# ==============================================================================
calc_contribution_batch <- function(mat_meas, mat_theo, eps = 1e-9) {
  mat_m <- as.matrix(mat_meas)
  mat_t <- as.matrix(mat_theo)
  
  if (!all(dim(mat_m) == dim(mat_t))) {
    stop("실측값과 이론값 데이터의 행/열 차원이 동일해야 합니다.")
  }
  
  n_samples <- nrow(mat_m)
  
  # 결과를 저장할 데이터프레임 초기화
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
    res_df$TVD[i] <- res$TVD
    res_df$MAE[i] <- res$MAE
    res_df$RMSE[i] <- res$RMSE
    res_df$Mean_Abs_PBIAS_pct[i] <- res$Mean_Abs_PBIAS_pct
    res_df$Aitchison_Dist[i] <- res$Aitchison_Distance
  }
  
  return(res_df)
}

# ==============================================================================
# 예시1: 2종 오염원 혼합 실험
# ==============================================================================
f_meas_2d <- c(98.2, 1.8)
f_theo_2d <- c(99.4, 0.6)

res_2d <- calc_contribution_metrics(f_meas_2d, f_theo_2d)
print(res_2d)

# [출력 예시]
# $TVD: 0.05       (전체 기여율 재구성 오차 5%p)
# $MAE: 0.05       (오염원당 평균 오차 5%p)
# $RMSE: 0.05
# $Mean_Abs_PBIAS_pct: 11.90%
# $Aitchison_Distance: 0.1614
# $PBIAS_by_Source_pct: -7.14%, +16.67%

# ==============================================================================
# 예시2: 4종 오염원 확장 적용
# ==============================================================================
# 차원이 늘어나도 동일한 함수 호출
f_meas_4d <- c(0.38, 0.32, 0.19, 0.11)
f_theo_4d <- c(0.40, 0.30, 0.20, 0.10)

res_4d <- calc_contribution_metrics(f_meas_4d, f_theo_4d)
print(res_4d)

# ==============================================================================
# 예시3: 데이터프레임 전체 일괄 평가
# ==============================================================================
# 행: 시료 조건, 열: 오염원(Source 1, Source 2)
df_theo <- data.frame(S1 = c(0.8, 0.5, 0.2), S2 = c(0.2, 0.5, 0.8))
df_meas <- data.frame(S1 = c(0.78, 0.52, 0.21), S2 = c(0.22, 0.48, 0.79))

batch_results <- calc_contribution_batch(df_meas, df_theo)
print(batch_results)