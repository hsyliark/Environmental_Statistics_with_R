# ==============================================================================
# [과제명] 안정동위원소 결합 오염평가 체계 고도화를 위한 국가 수질측정망 적용 연구
# [목  적] 오염원별 대표 시료 선정을 위한 2차 정제 4단계 표준 워크플로우 수행 R 스크립트
# [작성일] 2026년 8월
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. 환경 설정 및 필수 패키지 로드
# ------------------------------------------------------------------------------

# R 기본 내장 패키지인 'MASS' 사용 (로버스트 MCD 다변량 거리 계산용)
# MASS 패키지가 없을 경우 자동 설치 및 로드
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)

# 데이터 파일 경로 설정 (파일명 변경 시 이 부분만 수정하시면 됩니다)
file_path <- "C:/Users/User/Desktop/오염원 데이터 선별/(2026년) 오염원 데이터 선별_선행연구.csv"

# 인코딩 오류 방지용 안전 데이터 불러오기 함수 정의
# Windows(CP949/EUC-KR) 및 Mac/Linux(UTF-8) 환경 모두 호환
load_data_safely <- function(path) {
  # 1차 시도: CP949 (한글 윈도우 기본 인코딩)
  df <- tryCatch({
    read.csv(path, fileEncoding = "CP949", stringsAsFactors = FALSE)
  }, error = function(e) NULL)
  
  # 2차 시도: UTF-8 (Mac/Linux/최신 RStudio 인코딩)
  if (is.null(df) || ncol(df) <= 1) {
    df <- tryCatch({
      read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  }
  
  # 3차 시도: 시스템 기본 read.csv
  if (is.null(df) || ncol(df) <= 1) {
    df <- read.csv(path, stringsAsFactors = FALSE)
  }
  
  return(df)
}

# 데이터 읽어오기
raw_data <- load_data_safely(file_path)

# 데이터 로드 확인 및 예외 처리
if (is.null(raw_data) || nrow(raw_data) == 0) {
  stop("오류: 데이터 파일을 읽을 수 없습니다. 파일 경로 및 파일명을 확인해 주세요.")
}

cat("=== 0단계: 원본 데이터 로드 완료 ===\n")
cat("총 시료 수:", nrow(raw_data), "개 / 컬럼 목록:", paste(colnames(raw_data), collapse = ", "), "\n\n")


# ------------------------------------------------------------------------------
# 1단계: 도메인 스크리닝 (Domain Screening)
# ------------------------------------------------------------------------------
# 질산염 안정동위원소(d15N, d18O)의 화학적/수질학적 물리적 한계 범위를 적용합니다.
# - d15N 일반 범위: -30‰ ~ +40‰
# - d18O 일반 범위: -20‰ ~ +80‰

cat("=== 1단계: 도메인 스크리닝 (Domain Screening) 시작 ===\n")

# 도메인 기준 적용 (통과 시 TRUE, 이탈 시 FALSE)
domain_mask <- (raw_data$d15N >= -30 & raw_data$d15N <= 40) &
  (raw_data$d18O >= -20 & raw_data$d18O <= 80) &
  !is.na(raw_data$d15N) & !is.na(raw_data$d18O)

# 결과 열 추가
data_step1 <- raw_data
data_step1$Step1_Pass <- domain_mask

cat(" 도메인 스크리닝 통과 시료 수:", sum(domain_mask), "/", nrow(raw_data), "개\n\n")


# ------------------------------------------------------------------------------
# 2단계: 단변량 1D 정제 (Univariate Robust Outlier Removal)
# ------------------------------------------------------------------------------
# 각 오염원 그룹별(구분)로 중위수(Median)와 MAD(Median Absolute Deviation)를 이용한
# 수정 Z-점수(Modified Z-Score)를 계산하여 극단치를 제거합니다.
# - 기준: |Modified Z-Score| > 3.5 일 경우 비대표 시료로 판정

cat("=== 2단계: 단변량 로버스트 정제 (Univariate Robust Screening) 시작 ===\n")

# 수정 Z-점수 계산 함수 정의
calc_modified_z <- function(x) {
  med <- median(x, na.rm = TRUE)
  # R의 mad() 함수는 기본적으로 1.4826 곱셈 상수가 적용되어 있어
  # |x - median| / mad(x) 값이 표준 Modified Z-score와 정확히 일치합니다.
  mad_val <- mad(x, na.rm = TRUE)
  
  # MAD가 0인 경우(동일 값이 연속될 때) 0으로 나누는 오류 방지
  if (mad_val == 0) {
    return(rep(0, length(x)))
  }
  
  return(abs(x - med) / mad_val)
}

# 그룹별(구분)로 Modified Z-score 계산 수행
data_step2 <- data_step1
data_step2$ModZ_d15N <- NA
data_step2$ModZ_d18O <- NA
data_step2$Step2_Pass <- FALSE

# 오염원 그룹 고유값 추출 (토양계, 강우계, 생활계, 축산계, 농업계 등)
groups <- unique(data_step2$구분)

for (grp in groups) {
  # 해당 그룹 내 1단계 통과 시료 인덱스 추출
  idx <- which(data_step2$구분 == grp & data_step2$Step1_Pass == TRUE)
  
  if (length(idx) > 0) {
    # d15N 및 d18O 수정 Z-점수 계산
    modz_15N <- calc_modified_z(data_step2$d15N[idx])
    modz_18O <- calc_modified_z(data_step2$d18O[idx])
    
    data_step2$ModZ_d15N[idx] <- modz_15N
    data_step2$ModZ_d18O[idx] <- modz_18O
    
    # 두 항목 모두 수정 Z-점수가 3.5 이하인 시료만 최종 통과
    data_step2$Step2_Pass[idx] <- (modz_15N <= 3.5) & (modz_18O <= 3.5)
  }
}

cat(" 단변량 정제 통과 시료 수:", sum(data_step2$Step2_Pass), "/", sum(data_step1$Step1_Pass), "개\n\n")


# ------------------------------------------------------------------------------
# 3단계: 다변량 2D 정제 (Multivariate Robust Mahalanobis Distance)
# ------------------------------------------------------------------------------
# d15N과 d18O의 2차원 공간 상 상관관계를 고려한 MCD(Minimum Covariance Determinant)
# 로버스트 마할라노비스 거리를 계산합니다.
# - 기준: 자유도 2인 카이제곱 분포의 95% 임계값(Chi-square critical value = 5.991) 이내 시료 선별

cat("=== 3단계: 다변량 로버스트 정제 (Multivariate MCD Mahalanobis) 시작 ===\n")

# 자유도 2, 신뢰수준 95% 카이제곱 임계값 (약 5.991)
chi2_cutoff <- qchisq(0.95, df = 2)

data_step3 <- data_step2
data_step3$MCD_Distance <- NA
data_step3$Step3_Pass <- FALSE

for (grp in groups) {
  # 2단계까지 통과한 해당 그룹 시료 인덱스 추출
  idx <- which(data_step3$구분 == grp & data_step3$Step2_Pass == TRUE)
  
  # MCD 계산을 위해 최소 5개 이상의 표본이 필요함
  if (length(idx) >= 5) {
    mat <- as.matrix(data_step3[idx, c("d15N", "d18O")])
    
    # MASS::cov.mcd를 이용한 로버스트 중심 및 공분산 행렬 추정
    mcd_fit <- tryCatch({
      MASS::cov.mcd(mat)
    }, error = function(e) NULL)
    
    if (!is.null(mcd_fit)) {
      # 추정된 로버스트 중심과 공분산을 기반으로 마할라노비스 거리 계산
      dists <- mahalanobis(mat, center = mcd_fit$center, cov = mcd_fit$cov)
      data_step3$MCD_Distance[idx] <- dists
      data_step3$Step3_Pass[idx] <- (dists <= chi2_cutoff)
    } else {
      # MCD 수렴 실패 시 보수적으로 기존 통과 상태 유지
      data_step3$Step3_Pass[idx] <- TRUE
    }
  } else if (length(idx) > 0) {
    # 표본 수가 5개 미만인 소규모 그룹은 다변량 이상치 검정 생략 후 유지
    data_step3$Step3_Pass[idx] <- TRUE
  }
}

cat(" 다변량 정제 최종 통과(대표 시료) 수:", sum(data_step3$Step3_Pass), "/", sum(data_step2$Step2_Pass), "개\n\n")


# ------------------------------------------------------------------------------
# 4단계: 대표값 산출 및 정제 전/후 민감도 평가 (Summary Statistics)
# ------------------------------------------------------------------------------
# 오염원 그룹별 정제 전(Raw)과 정제 후(Final) 대표 시료의 통계량 비교표를 생성합니다.

cat("=== 4단계: 오염원별 대표값 산출 및 정제 결과 요약 ===\n")

summary_results <- data.frame(
  오염원구분 = character(),
  원자료_수 = integer(),
  최종시료_수 = integer(),
  제거율_퍼센트 = numeric(),
  Raw_d15N_평균 = numeric(),
  Raw_d15N_표준편차 = numeric(),
  Final_d15N_평균 = numeric(),
  Final_d15N_표준편차 = numeric(),
  Raw_d18O_평균 = numeric(),
  Raw_d18O_표준편차 = numeric(),
  Final_d18O_평균 = numeric(),
  Final_d18O_표준편차 = numeric(),
  stringsAsFactors = FALSE
)

for (grp in groups) {
  # 정제 전 그룹 데이터
  raw_sub <- data_step3[data_step3$구분 == grp, ]
  # 최종 2차 정제 완료 그룹 데이터 (Step3_Pass == TRUE)
  final_sub <- data_step3[data_step3$구분 == grp & data_step3$Step3_Pass == TRUE, ]
  
  n_raw <- nrow(raw_sub)
  n_final <- nrow(final_sub)
  removal_rate <- round((1 - (n_final / n_raw)) * 100, 1)
  
  summary_results[nrow(summary_results) + 1, ] <- list(
    오염원구분 = grp,
    원자료_수 = n_raw,
    최종시료_수 = n_final,
    제거율_퍼센트 = removal_rate,
    Raw_d15N_평균 = round(mean(raw_sub$d15N, na.rm = TRUE), 2),
    Raw_d15N_표준편차 = round(sd(raw_sub$d15N, na.rm = TRUE), 2),
    Final_d15N_평균 = round(mean(final_sub$d15N, na.rm = TRUE), 2),
    Final_d15N_표준편차 = round(sd(final_sub$d15N, na.rm = TRUE), 2),
    Raw_d18O_평균 = round(mean(raw_sub$d18O, na.rm = TRUE), 2),
    Raw_d18O_표준편차 = round(sd(raw_sub$d18O, na.rm = TRUE), 2),
    Final_d18O_평균 = round(mean(final_sub$d18O, na.rm = TRUE), 2),
    Final_d18O_표준편차 = round(sd(final_sub$d18O, na.rm = TRUE), 2)
  )
}

# 콘솔에 정제 결과 요약표 출력
print(summary_results)


# ------------------------------------------------------------------------------
# 5. 결과 데이터 파일 저장 (CSV Export)
# ------------------------------------------------------------------------------

# 1) 대표 시료 선별 플래그가 포함된 전체 결과 파일 저장
write.csv(data_step3, "C:/Users/User/Desktop/오염원 데이터 선별/오염원_데이터_2차정제_전체결과.csv", row.names = FALSE, fileEncoding = "CP949")

# 2) 최종 선별된 대표 시료만 추출하여 저장 (End-member 모델 입력용)
final_representative_samples <- data_step3[data_step3$Step3_Pass == TRUE, c("구분", "오염원", "d15N", "d18O")]
write.csv(final_representative_samples, "C:/Users/User/Desktop/오염원 데이터 선별/오염원별_최종_대표시료.csv", row.names = FALSE, fileEncoding = "CP949")

# 3) 정제 전/후 통계 비교 요약표 저장
write.csv(summary_results, "C:/Users/User/Desktop/오염원 데이터 선별/2차정제_통계_요약표.csv", row.names = FALSE, fileEncoding = "CP949")

cat("\n[완료] 결과 파일 3건이 성공적으로 저장되었습니다.\n")
cat(" 1. 오염원_데이터_2차정제_전체결과.csv (각 단계별 통과 플래그 포함)\n")
cat(" 2. 오염원별_최종_대표시료.csv (End-member 산출용 대표 시료)\n")
cat(" 3. 2차정제_통계_요약표.csv (보고서용 평균/표준편차 비교표)\n")
