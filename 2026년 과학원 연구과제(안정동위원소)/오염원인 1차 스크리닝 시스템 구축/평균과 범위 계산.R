# 필요 패키지 로드
library(dplyr)
library(readr)
library(tidyr)

# 1. 데이터 불러오기 (한글 인코딩 방어)
file_path <- "C:/Users/User/Desktop/연구지점 데이터 정리/월별 통계량 산출.csv"
water_data <- read_csv(file_path, locale = locale(encoding = "EUC-KR"))

# 2. 분석 대상 10개 항목 정의 (CSV 컬럼명 표준 매칭)
target_params <- c("TN", "TP", "NH3N", "NO3N", "EC", "TOC", "Chla", "Cl", "d15N", "d18O")

# 3. 통계량 산출 및 소수점 자리수 규칙 적용
report_summary <- water_data %>%
  # 필요한 10개 변수와 월 데이터만 선택
  select(month, all_of(target_params)) %>%
  # 가로형 데이터를 세로형 변환
  pivot_longer(cols = -month, names_to = "Parameter", values_to = "Value") %>%
  # 월별, 수질 항목별 그룹화
  group_by(month, Parameter) %>%
  # 기초 통계량 산출 (전부 결측치인 경우 NA 처리 예외 적용)
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Min  = if(all(is.na(Value))) NA_real_ else min(Value, na.rm = TRUE),
    Max  = if(all(is.na(Value))) NA_real_ else max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 지시사항에 따른 소수점 규격 강제 포맷팅 (결측치는 "-" 표기)
  mutate(
    Formatted_Mean = case_when(
      is.na(Mean) ~ "-",
      Parameter %in% c("TN", "TP", "NH3N", "NO3N") ~ sprintf("%.3f", Mean),
      Parameter == "EC" ~ sprintf("%.0f", Mean),
      Parameter %in% c("TOC", "Chla", "Cl", "d15N", "d18O") ~ sprintf("%.1f", Mean)
    ),
    Formatted_Min = case_when(
      is.na(Min) ~ "-",
      Parameter %in% c("TN", "TP", "NH3N", "NO3N") ~ sprintf("%.3f", Min),
      Parameter == "EC" ~ sprintf("%.0f", Min),
      Parameter %in% c("TOC", "Chla", "Cl", "d15N", "d18O") ~ sprintf("%.1f", Min)
    ),
    Formatted_Max = case_when(
      is.na(Max) ~ "-",
      Parameter %in% c("TN", "TP", "NH3N", "NO3N") ~ sprintf("%.3f", Max),
      Parameter == "EC" ~ sprintf("%.0f", Max),
      Parameter %in% c("TOC", "Chla", "Cl", "d15N", "d18O") ~ sprintf("%.1f", Max)
    )
  ) %>%
  # 보고서 맞춤형 범위(Min ~ Max) 텍스트 컬럼 생성
  mutate(
    Range = if_else(Formatted_Min == "-", "-", paste0(Formatted_Min, " ~ ", Formatted_Max))
  ) %>%
  # 최종 보고용 컬럼만 필터링
  select(month, Parameter, Mean = Formatted_Mean, Range) %>%
  # 3월->4월->5월 및 요청하신 항목 순서대로 정렬 보정
  arrange(
    match(month, c("3월", "4월", "5월")),
    match(Parameter, target_params)
  )

# 4. 콘솔 확인 및 파일 저장
print(report_summary, n = Inf)
write_csv(report_summary, "보고서용_월별_10대항목_통계량.csv")
