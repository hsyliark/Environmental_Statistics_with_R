# ==============================================================================
# 0. 패키지 로드 및 데이터 준비
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, EnvStats, writexl)

# 데이터 로드 (경로 및 인코딩 확인 필수)
df <- read_csv("C:/Users/User/Desktop/data_30point.csv", locale = locale(encoding = "EUC-KR"))

# 지점명 오타 방지 (공백 제거)
df <- df %>% mutate(site = str_trim(site))

# ==============================================================================
# 1. 목표수질 기준 설정 (3개 항목: BOD, T-P, TOC) - T-N 제외
# ==============================================================================
limit_table <- tribble(
  ~Grade, ~Limit_BOD, ~Limit_TP, ~Limit_TOC,
  "Ia",   1,          0.02,      2,
  "Ib",   2,          0.04,      3,
  "II",   3,          0.10,      4,
  "III",  5,          0.20,      5,
  "IV",   8,          0.30,      6,
  "V",    10,         0.50,      8,
  "-",    Inf,        Inf,       Inf
)

site_targets <- tribble(
  ~site,       ~Target_Grade,
  "담양", "-", "우치", "III", "광주1", "-", "광주천1", "-", "광주3", "III",
  "황룡강2", "-", "황룡강5", "II", "지석천4", "II", "광산", "III", "나주", "III",
  "죽산", "III", "고막원천4", "II", "무안1", "Ib", "영암천1", "Ib", "무안2", "Ib",
  "운암", "Ib", "동계", "Ib", "오수천2", "Ib", "적성", "-", "남원", "Ib",
  "요천3", "Ib", "곡성", "Ib", "보성강4", "Ia", "주암댐", "Ia", "보성강7", "Ia",
  "구례", "-", "하동", "-", "진월", "Ib", "탐진강5", "Ib", "주진천2", "III"
) %>% mutate(site = str_trim(site))

# 데이터 결합 및 미달성 여부 판정
data_analysis <- df %>%
  mutate(date_obj = as.Date(date, format = "%Y.%m.%d"),
         year = year(date_obj), month = month(date_obj)) %>%
  left_join(site_targets, by = "site") %>%
  left_join(limit_table, by = c("Target_Grade" = "Grade")) %>%
  mutate(
    Is_Fail_BOD = if_else(BOD > Limit_BOD, 1, 0),
    Is_Fail_TP  = if_else(T_P > Limit_TP,  1, 0),
    Is_Fail_TOC = if_else(TOC > Limit_TOC, 1, 0),
    Is_Fail_Any = Is_Fail_BOD + Is_Fail_TP + Is_Fail_TOC
  )

# ==============================================================================
# 2. [분석 1] 목표달성 현황 및 미달성 순위
# ==============================================================================
# 2-1. 연도별 미달성 횟수
cross_year <- data_analysis %>%
  group_by(site, year) %>%
  summarise(Yearly_Fail = sum(Is_Fail_Any, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = Yearly_Fail, names_prefix = "연도_")

# 2-2. 항목별 미달성 횟수 및 순위 (Total Failures)
cross_param <- data_analysis %>%
  group_by(site) %>%
  summarise(
    미달성_BOD = sum(Is_Fail_BOD, na.rm = TRUE),
    미달성_TP  = sum(Is_Fail_TP, na.rm = TRUE),
    미달성_TOC = sum(Is_Fail_TOC, na.rm = TRUE),
    총_미달성_횟수 = sum(Is_Fail_Any, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(미달성_순위 = min_rank(desc(총_미달성_횟수)))

# ==============================================================================
# 3. [분석 2] 시계열 추세 분석 (Sen's Slope 추가)
# ==============================================================================
calc_smk_trend <- function(d, p) {
  d_clean <- d %>% filter(!is.na(.data[[p]]))
  # 데이터 부족 시 (24개 미만)
  if(nrow(d_clean) < 24) return(tibble(Trend = "데이터부족", Slope = NA, Tau = NA, P_val = NA))
  
  tryCatch({
    # Seasonal Kendall Test 수행
    res <- kendallSeasonalTrendTest(as.formula(paste(p, "~ month + year")), data = d_clean)
    
    p_val <- res$p.value["z (Trend)"]
    slope_val <- res$estimate["slope"] # Sen's Slope Estimator
    tau_val <- res$estimate["tau"]     # Kendall's Tau
    
    # 추세 판정 (P < 0.05 유의수준)
    trend_val <- if_else(p_val < 0.05, 
                         if_else(slope_val > 0, "악화", "개선"), 
                         "-")
    
    return(tibble(Trend = trend_val, Slope = slope_val, Tau = tau_val, P_val = p_val))
  }, error = function(e) return(tibble(Trend = "오류", Slope = NA, Tau = NA, P_val = NA)))
}

params <- c("BOD", "T_N", "T_P", "TOC")

trend_summary <- map_dfr(unique(data_analysis$site), function(s) {
  s_data <- data_analysis %>% filter(site == s)
  res_row <- tibble(site = s)
  for(p in params) {
    res <- calc_smk_trend(s_data, p)
    # 결과 저장: 추세, Sen's Slope, Tau, P-value 순
    res_row[[paste0("추세_", p)]] <- res$Trend
    res_row[[paste0("Sen_Slope_", p)]] <- round(res$Slope, 6) # 변화량 (기울기)
    res_row[[paste0("Tau_", p)]]  <- round(res$Tau, 3)        # 경향성 강도
    res_row[[paste0("P값_", p)]]  <- round(res$P_val, 4)      # 유의확률
  }
  return(res_row)
})

# ==============================================================================
# 4. [분석 3] Z-Score 및 종합 순위 (90백분위수 기준)
# ==============================================================================
zscore_ranking <- data_analysis %>%
  group_by(site) %>%
  # 90백분위수 산출
  summarise(across(c(BOD, T_N, T_P, TOC), 
                   ~quantile(., probs = 0.9, na.rm = TRUE), 
                   .names = "P90_{.col}"), .groups = 'drop') %>% 
  # Z-Score 변환
  mutate(across(starts_with("P90_"), 
                ~as.numeric(scale(.)), 
                .names = "Z_{.col}")) %>%
  # 종합 지수 (4개 항목 Z-score 평균)
  mutate(종합_지수 = (Z_P90_BOD + Z_P90_T_N + Z_P90_T_P + Z_P90_TOC) / 4) %>%
  # 종합 순위 (지수가 높을수록 오염도 상위)
  mutate(종합_순위 = min_rank(desc(종합_지수))) %>%
  select(site, 종합_지수, 종합_순위, starts_with("P90_"))

# ==============================================================================
# 5. [분석 4] 오염원 특성 진단
# ==============================================================================
source_diagnosis <- data_analysis %>%
  group_by(site) %>%
  summarise(
    NP_Ratio = mean(T_N / T_P, na.rm = TRUE),
    TOC_TN_Cor = cor(TOC, T_N, use = "complete.obs"),
    .groups = 'drop'
  ) %>%
  mutate(예비_진단 = case_when(
    NP_Ratio > 25 & TOC_TN_Cor > 0.6 ~ "축산/농업(복합)",
    NP_Ratio > 30 ~ "농업계(비료)",
    NP_Ratio < 15 ~ "생활계(하수)",
    TRUE ~ "복합/기타"
  ))

# ==============================================================================
# 6. 최종 통합 및 엑셀 저장 (보고서용 컬럼 정렬)
# ==============================================================================
final_table <- site_targets %>%
  left_join(zscore_ranking, by = "site") %>%    # 1. 종합 오염도 평가
  left_join(cross_param, by = "site") %>%       # 2. 목표 달성 여부 (총괄)
  left_join(cross_year, by = "site") %>%        # 3. 연도별 달성 현황
  left_join(trend_summary, by = "site") %>%     # 4. 시계열 추세 (Sen's Slope 포함)
  left_join(source_diagnosis, by = "site") %>%  # 5. 오염원 진단
  select(
    지점명 = site, 목표등급 = Target_Grade,
    
    # [종합 평가]
    종합_순위, 종합_지수,
    BOD_P90 = P90_BOD, TN_P90 = P90_T_N, TP_P90 = P90_T_P, TOC_P90 = P90_TOC,
    
    # [목표 달성 현황]
    미달성_순위, 총_미달성_횟수,
    미달성_BOD, 미달성_TP, 미달성_TOC,
    starts_with("연도_"),
    
    # [시계열 추세 - 항목별 4개 지표 확인 가능]
    matches("추세_|Sen_Slope_|Tau_|P값_"),
    
    # [오염원 진단]
    NP_Ratio, TOC_TN_Cor, 예비_진단
  ) %>%
  arrange(종합_순위)

# 엑셀 파일 생성
write_xlsx(final_table, "C:/Users/User/Desktop/수질분석_최종결과_SenSlope포함.xlsx")

# 확인
print(head(final_table))











#### 5*2 배열 그림 정렬
# ==============================================================================
# 0. 패키지 로드 및 데이터 준비
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, scales)

# 1. 데이터 로드 (경로 확인 필수)
df <- read_csv("C:/Users/User/Desktop/data_30point.csv", locale = locale(encoding = "EUC-KR"))

# 2. [핵심] 사용자가 요청한 지점 순서 정의 (30개 지점)
site_order <- c(
  "담양", "우치", "광주1", "광주천1", "광주3", 
  "황룡강2", "황룡강5", "지석천4", "광산", "나주", 
  "죽산", "고막원천4", "무안1", "영암천1", "무안2", 
  "운암", "동계", "오수천2", "적성", "남원", 
  "요천3", "곡성", "보성강4", "주암댐", "보성강7", 
  "구례", "하동", "진월", "탐진강5", "주진천2"
)

# 3. 데이터 전처리 및 순서 고정 (Factor 설정)
plot_df <- df %>%
  mutate(
    site = str_trim(site),
    date_obj = as.Date(date, format = "%Y.%m.%d"),
    NP_Ratio = T_N / T_P
  ) %>%
  # 지정한 순서대로 지점을 정렬하기 위해 Factor로 변환합니다.
  filter(site %in% site_order) %>% 
  mutate(site = factor(site, levels = site_order))

# ==============================================================================
# 그래프 생성 함수 (기존 로직 유지 + 순서 보장)
# ==============================================================================
save_ordered_plots <- function(data, y_var, y_lab, file_prefix, is_ratio = FALSE) {
  
  num_groups <- 3 # 30개 지점 / 10개씩 = 3개 파일
  
  for (i in 1:num_groups) {
    # 그룹별 지점 추출
    start_idx <- (i - 1) * 10 + 1
    end_idx <- i * 10
    current_sites <- site_order[start_idx:end_idx]
    
    subset_data <- data %>% filter(site %in% current_sites)
    
    # 그래프 생성
    p <- ggplot(subset_data, aes(x = date_obj, y = .data[[y_var]])) +
      geom_line(color = ifelse(is_ratio, "darkgreen", "gray40"), alpha = 0.6) +
      geom_point(size = 1.2, color = ifelse(is_ratio, "darkgreen", "black")) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed", size = 0.6) +
      facet_wrap(~site, ncol = 2, scales = "free_y", nrow = 5) +
      scale_x_date(date_labels = "%y.%m", date_breaks = "12 months") + 
      theme_bw() +
      labs(x = "Date", y = y_lab, title = paste0(file_prefix, " Trends (Group ", i, ")")) +
      theme(
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    # N/P Ratio일 경우 기준선 추가
    if(is_ratio) {
      p <- p + 
        geom_hline(yintercept = 15, color = "red", linetype = "dotted") +
        geom_hline(yintercept = 30, color = "orange", linetype = "dotted")
    }
    
    # 저장
    file_name <- paste0("C:/Users/User/Desktop/Plot_", file_prefix, "_Group", i, ".png")
    ggsave(file_name, p, width = 210, height = 297, units = "mm", dpi = 300)
  }
}

# ==============================================================================
# 실행: 항목별 3개 파일씩 자동 생성
# ==============================================================================
# 1. 수질 항목별 (BOD, T-N, T-P, TOC)
params <- list(c("BOD", "BOD (mg/L)"), c("T_N", "T-N (mg/L)"), 
               c("T_P", "T-P (mg/L)"), c("TOC", "TOC (mg/L)"))

for (p in params) {
  save_ordered_plots(plot_df, p[1], p[2], p[1])
}

# 2. N/P Ratio 별도 실행
save_ordered_plots(plot_df, "NP_Ratio", "T-N/T-P Ratio", "NP_Ratio", is_ratio = TRUE)

print("모든 시계열 그래프가 지정된 순서대로 저장되었습니다.")





#### 10*3 배열 그림 정렬
# ==============================================================================
# 0. 패키지 로드 및 데이터 준비
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, scales)

# 1. 데이터 로드
df <- read_csv("C:/Users/User/Desktop/data_30point.csv", locale = locale(encoding = "EUC-KR"))

# 2. 지점 순서 정의 (수계 및 상하류 순서 고정)
site_order <- c(
  "담양", "우치", "광주1", "광주천1", "광주3", 
  "황룡강2", "황룡강5", "지석천4", "광산", "나주", 
  "죽산", "고막원천4", "무안1", "영암천1", "무안2", 
  "운암", "동계", "오수천2", "적성", "남원", 
  "요천3", "곡성", "보성강4", "주암댐", "보성강7", 
  "구례", "하동", "진월", "탐진강5", "주진천2"
)

# 3. 데이터 전처리
plot_df <- df %>%
  mutate(
    site = str_trim(site),
    date_obj = as.Date(date, format = "%Y.%m.%d"),
    NP_Ratio = T_N / T_P
  ) %>%
  filter(site %in% site_order) %>% 
  mutate(site = factor(site, levels = site_order))

# ==============================================================================
# 10행 3열 통합 그래프 생성 함수 (추세선 색상 가변형)
# ==============================================================================
save_final_plots <- function(data, y_var, y_lab, file_prefix, line_color, is_ratio = FALSE) {
  
  p <- ggplot(data, aes(x = date_obj, y = .data[[y_var]])) +
    # 데이터 포인트 및 실선 (회색으로 고정하여 추세선 강조)
    geom_line(color = "gray60", alpha = 0.4, size = 0.4) +
    geom_point(size = 0.7, color = "gray30", alpha = 0.6) +
    
    # [핵심] 항목별로 지정된 색상의 추세선 적용
    geom_smooth(method = "lm", se = FALSE, color = line_color, linetype = "solid", size = 0.8) +
    
    # 10행 3열 배열
    facet_wrap(~site, ncol = 3, nrow = 10, scales = "free_y") +
    
    # X축 설정
    scale_x_date(date_labels = "%y", date_breaks = "1 year") + 
    
    theme_bw(base_size = 11) +
    labs(x = "Year", y = y_lab, 
         title = paste0("[Trend] ", file_prefix, " Time Series Plot")) +
    
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "gray95"),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "darkblue")
    )
  
  # N/P Ratio 전용 기준선 추가 
  if(is_ratio) {
    p <- p + 
      geom_hline(yintercept = 15, color = "red", linetype = "dotted", size = 0.6) +
      geom_hline(yintercept = 30, color = "blue", linetype = "dotted", size = 0.6)
  }
  
  # 파일 저장 (10행 수용을 위해 세로 500mm 유지)
  file_name <- paste0("C:/Users/User/Desktop/Final_Plot_", file_prefix, ".png")
  ggsave(file_name, p, width = 210, height = 500, units = "mm", dpi = 300)
  
  message(paste0(">>> ", file_prefix, " 그래프 저장 완료 (색상: ", line_color, ")"))
}

# ==============================================================================
# 4. 항목별 실행 (각 항목에 최적화된 색상 전달)
# ==============================================================================

# 1) BOD: 로얄 블루
save_final_plots(plot_df, "BOD", "BOD (mg/L)", "BOD", "royalblue")

# 2) T-N: 진한 초록색
save_final_plots(plot_df, "T_N", "T-N (mg/L)", "T_N", "darkgreen")

# 3) T-P: 빨간색
save_final_plots(plot_df, "T_P", "T-P (mg/L)", "T_P", "red2")

# 4) TOC: 보라색
save_final_plots(plot_df, "TOC", "TOC (mg/L)", "TOC", "purple")

# 5) N/P Ratio: 진한 주황색
save_final_plots(plot_df, "NP_Ratio", "T-N/T-P Ratio", "NP_Ratio", "darkorange", is_ratio = TRUE)
