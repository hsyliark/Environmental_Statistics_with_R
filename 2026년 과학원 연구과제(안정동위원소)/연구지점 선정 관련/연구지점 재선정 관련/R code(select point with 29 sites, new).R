# ==============================================================================
# 0. 패키지 로드 및 데이터 준비
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, EnvStats, writexl, scales)

# 1. 새로운 데이터 로드 (파일명 변경 반영)
df <- read_csv("C:/Users/User/Desktop/(260413) data_29point.csv", locale = locale(encoding = "EUC-KR"))

# 지점명 오타 및 공백 방지
df <- df %>% mutate(site = str_trim(site))

# [중요] 29개 지점의 상하류 순서 정의
# ※ CSV에 존재하는 29개 지점명을 추출합니다. 보고서의 수계/상하류 배치에 맞게 
# 아래 문자열 벡터의 순서를 직접 수정하여 고정하실 수 있습니다.
site_order <- unique(df$site)

# ==============================================================================
# 1. 목표수질 기준 설정 및 타겟 매핑
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

# 2. 새로운 후보 명단(PDF) 기반 목표 수질 (29개 지점)
# ※ 일부 지점의 등급은 임의 할당되었으니, PDF 명단을 보고 필요시 수정해주세요.
site_targets <- tribble(
  ~site,       ~Target_Grade,
  "우치", "III", "풍영정천2", "-", "광주천2", "-", "황룡강5", "II",
  "지석천4", "II", "평동천", "-", "광산", "III", "장성천2", "-",
  "나주", "III", "영산천", "-", "만봉천", "-", "문평천", "-",
  "죽산", "III", "영산포2", "-", "고막원천4", "II", "함평천3", "-",
  "함평", "-", "삼포천2", "-", "영암천2", "-", "무안2", "Ib",
  "관촌", "-", "임실", "-", "오수천2", "Ib", "요천3", "Ib",
  "동복천2", "-", "주암댐", "Ia", "계산", "-", "탐진강4", "-",
  "탐진강5", "Ib"
) %>% mutate(site = str_trim(site))

# 데이터 결합 및 미달성 여부 판정
data_analysis <- df %>%
  mutate(date_obj = as.Date(date, format = "%Y.%m.%d"),
         year = year(date_obj), month = month(date_obj)) %>%
  left_join(site_targets, by = "site") %>%
  # [안전장치] site_targets에 누락된 새로운 지점이 있다면 목표등급을 "-"로 자동 부여
  mutate(Target_Grade = replace_na(Target_Grade, "-")) %>%
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
cross_year <- data_analysis %>%
  group_by(site, year) %>%
  summarise(Yearly_Fail = sum(Is_Fail_Any, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = Yearly_Fail, names_prefix = "연도_")

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
# 3. [분석 2] 시계열 추세 분석 (Sen's Slope)
# ==============================================================================
calc_smk_trend <- function(d, p) {
  d_clean <- d %>% filter(!is.na(.data[[p]]))
  if(nrow(d_clean) < 24) return(tibble(Trend = "데이터부족", Slope = NA, Tau = NA, P_val = NA))
  
  tryCatch({
    res <- kendallSeasonalTrendTest(as.formula(paste(p, "~ month + year")), data = d_clean)
    p_val <- res$p.value["z (Trend)"]
    slope_val <- res$estimate["slope"]
    tau_val <- res$estimate["tau"]
    trend_val <- if_else(p_val < 0.05, if_else(slope_val > 0, "악화", "개선"), "-")
    return(tibble(Trend = trend_val, Slope = slope_val, Tau = tau_val, P_val = p_val))
  }, error = function(e) return(tibble(Trend = "오류", Slope = NA, Tau = NA, P_val = NA)))
}

params <- c("BOD", "T_N", "T_P", "TOC")

trend_summary <- map_dfr(unique(data_analysis$site), function(s) {
  s_data <- data_analysis %>% filter(site == s)
  res_row <- tibble(site = s)
  for(p in params) {
    res <- calc_smk_trend(s_data, p)
    res_row[[paste0("추세_", p)]] <- res$Trend
    res_row[[paste0("Sen_Slope_", p)]] <- round(res$Slope, 6)
    res_row[[paste0("Tau_", p)]]  <- round(res$Tau, 3)
    res_row[[paste0("P값_", p)]]  <- round(res$P_val, 4)
  }
  return(res_row)
})

# ==============================================================================
# 4. [분석 3] Z-Score 및 종합 순위 (90백분위수 기준)
# ==============================================================================
zscore_ranking <- data_analysis %>%
  group_by(site) %>%
  summarise(across(c(BOD, T_N, T_P, TOC), 
                   ~quantile(., probs = 0.9, na.rm = TRUE), 
                   .names = "P90_{.col}"), .groups = 'drop') %>% 
  mutate(across(starts_with("P90_"), ~as.numeric(scale(.)), .names = "Z_{.col}")) %>%
  mutate(종합_지수 = (Z_P90_BOD + Z_P90_T_N + Z_P90_T_P + Z_P90_TOC) / 4) %>%
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
# 6. 최종 통합 및 엑셀 저장
# ==============================================================================
final_table <- tibble(site = site_order) %>% left_join(site_targets, by="site") %>%
  mutate(Target_Grade = replace_na(Target_Grade, "-")) %>%
  left_join(zscore_ranking, by = "site") %>%
  left_join(cross_param, by = "site") %>%
  left_join(cross_year, by = "site") %>%
  left_join(trend_summary, by = "site") %>%
  left_join(source_diagnosis, by = "site") %>%
  select(
    지점명 = site, 목표등급 = Target_Grade,
    종합_순위, 종합_지수,
    BOD_P90 = P90_BOD, TN_P90 = P90_T_N, TP_P90 = P90_T_P, TOC_P90 = P90_TOC,
    미달성_순위, 총_미달성_횟수, 미달성_BOD, 미달성_TP, 미달성_TOC,
    starts_with("연도_"),
    matches("추세_|Sen_Slope_|Tau_|P값_"),
    NP_Ratio, TOC_TN_Cor, 예비_진단
  ) %>%
  arrange(종합_순위)

write_xlsx(final_table, "C:/Users/User/Desktop/수질분석_최종결과_29point.xlsx")
cat(">>> 엑셀 마스터 테이블 생성 완료\n")


# ==============================================================================
# 7. 그래프 시각화 (5*2 배열 및 10*3 배열)
# ==============================================================================
plot_df <- data_analysis %>%
  mutate(
    NP_Ratio = T_N / T_P,
    site = factor(site, levels = site_order)
  )

# ---------------------------------------------------------
# [그림 A] 5행 2열 배열 출력 (29개 지점 대응)
# ---------------------------------------------------------
save_ordered_plots <- function(data, y_var, y_lab, file_prefix, is_ratio = FALSE) {
  num_groups <- ceiling(length(site_order) / 10) # 29 / 10 = 3그룹
  
  for (i in 1:num_groups) {
    start_idx <- (i - 1) * 10 + 1
    # [수정포인트] 지점이 30개가 안 될 경우를 대비하여 min() 함수 적용
    end_idx <- min(i * 10, length(site_order)) 
    current_sites <- site_order[start_idx:end_idx]
    
    subset_data <- data %>% filter(site %in% current_sites)
    
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
    
    if(is_ratio) {
      p <- p + geom_hline(yintercept = 15, color = "red", linetype = "dotted") +
        geom_hline(yintercept = 30, color = "orange", linetype = "dotted")
    }
    file_name <- paste0("C:/Users/User/Desktop/Plot_5x2_", file_prefix, "_Group", i, ".png")
    ggsave(file_name, p, width = 210, height = 297, units = "mm", dpi = 300)
  }
}

# ---------------------------------------------------------
# [그림 B] 10행 3열 배열 출력 (색상 적용)
# ---------------------------------------------------------
save_final_plots <- function(data, y_var, y_lab, file_prefix, line_color, is_ratio = FALSE) {
  p <- ggplot(data, aes(x = date_obj, y = .data[[y_var]])) +
    geom_line(color = "gray60", alpha = 0.4, size = 0.4) +
    geom_point(size = 0.7, color = "gray30", alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = line_color, linetype = "solid", size = 0.8) +
    # 29개 지점이므로 마지막 1칸(우측 하단)은 자연스럽게 빈칸으로 출력됩니다.
    facet_wrap(~site, ncol = 3, nrow = 10, scales = "free_y") +
    scale_x_date(date_labels = "%y", date_breaks = "1 year") + 
    theme_bw(base_size = 11) +
    labs(x = "Year", y = y_lab, title = paste0("[Trend] ", file_prefix, " Time Series Plot")) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "gray95"),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
    )
  
  if(is_ratio) {
    p <- p + geom_hline(yintercept = 15, color = "red", linetype = "dotted", size = 0.6) +
      geom_hline(yintercept = 30, color = "blue", linetype = "dotted", size = 0.6)
  }
  file_name <- paste0("C:/Users/User/Desktop/Plot_10x3_", file_prefix, ".png")
  ggsave(file_name, p, width = 210, height = 500, units = "mm", dpi = 300)
}

# ---------------------------------------------------------
# 그래프 실행
# ---------------------------------------------------------
plot_params <- list(
  c("BOD", "BOD (mg/L)", "royalblue"), 
  c("T_N", "T-N (mg/L)", "darkgreen"), 
  c("T_P", "T-P (mg/L)", "red2"), 
  c("TOC", "TOC (mg/L)", "purple")
)

for (p in plot_params) {
  save_ordered_plots(plot_df, p[1], p[2], p[1])
  save_final_plots(plot_df, p[1], p[2], p[1], p[3])
}
save_ordered_plots(plot_df, "NP_Ratio", "T-N/T-P Ratio", "NP_Ratio", is_ratio = TRUE)
save_final_plots(plot_df, "NP_Ratio", "T-N/T-P Ratio", "NP_Ratio", "darkorange", is_ratio = TRUE)

cat(">>> 모든 분석 및 시각화 처리가 완료되었습니다.\n")
