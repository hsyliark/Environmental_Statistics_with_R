# ==============================================================================
# [SCI 논문용] 지점별/Family별 Best Model 강조형 시계열 그래프
# 수정일: 2026.02.05
# ==============================================================================

# 1. 라이브러리 로드
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# 2. 파일 경로 설정 (사용자 환경에 맞게 수정)
files <- list(
  SC = "C:/Users/User/Desktop/df_SC_res_2025.csv",
  JS = "C:/Users/User/Desktop/df_JS_res_2025.csv"
)

# 3. 모델 Family 그룹 정의
model_groups <- list(
  DNN = c("DNN", "DNN_SMOTE", "DNN_ADASYN", "DNN_SMOTE_ENN", "DNN_SMOTE_Tomek"),
  CNN = c("CNN", "CNN_SMOTE", "CNN_ADASYN", "CNN_SMOTE_ENN", "CNN_SMOTE_Tomek"),
  TimeSeries = c("RNN", "LSTM")
)

# 4. 루프 실행: 지점별 -> Family별
for (site_key in names(files)) {
  
  # 데이터 로드 및 날짜 형식 수정 (점 구분을 ymd로 해결)
  df <- read_csv(files[[site_key]], show_col_types = FALSE)
  df$date <- ymd(df$date) 
  
  site_full_name <- ifelse(site_key == "SC", "Seungchon Weir (SC)", "Juksan Weir (JS)")
  
  for (fam_name in names(model_groups)) {
    
    # --------------------------------------------------------------------------
    # [핵심 수정] 요청하신 지점별/Family별 Best Model 동적 할당
    # --------------------------------------------------------------------------
    best_model <- ""
    if (site_key == "SC") {
      best_model <- switch(fam_name, 
                           "DNN" = "DNN_SMOTE", 
                           "CNN" = "CNN_SMOTE_Tomek", 
                           "TimeSeries" = "RNN")
    } else { # JS
      best_model <- switch(fam_name, 
                           "DNN" = "DNN_SMOTE", 
                           "CNN" = "CNN_SMOTE", 
                           "TimeSeries" = "RNN")
    }
    
    current_models <- model_groups[[fam_name]]
    target_cols <- c("date", "Chla", current_models)
    
    # 데이터 변환 (Long Format)
    df_long <- df %>%
      select(all_of(target_cols)) %>%
      pivot_longer(cols = -date, names_to = "Model", values_to = "Value")
    
    # 그래프 스타일 지정을 위한 그룹화
    df_long <- df_long %>%
      mutate(
        Type = case_when(
          Model == "Chla" ~ "Observed",
          Model == best_model ~ "Best",
          TRUE ~ "Others"
        ),
        # 범례 순서: Observed -> Best -> 나머지 모델들
        Model = factor(Model, levels = c("Chla", best_model, setdiff(current_models, best_model)))
      )
    
    # --------------------------------------------------------------------------
    # 그래프 생성
    # --------------------------------------------------------------------------
    p <- ggplot(df_long, aes(x = date, y = Value, color = Model, size = Type, linetype = Type)) +
      
      # 경향성을 보여주는 부드러운 곡선 (요청하신 span=0.3 적용)
      geom_smooth(method = "loess", span = 0.3, se = FALSE) +
      
      # [SCI 강조 스타일링]
      # 1. 선 굵기: 관측치와 Best 모델은 굵게, 나머지는 얇게
      scale_size_manual(values = c("Observed" = 1.3, "Best" = 1.1, "Others" = 0.5), guide = "none") +
      
      # 2. 선 타입: 관측치와 Best는 실선, 나머지는 점선(가독성 향상)
      scale_linetype_manual(values = c("Observed" = "solid", "Best" = "solid", "Others" = "dashed"), guide = "none") +
      
      # 3. 색상: 관측치는 검정색, 나머지는 컬러 팔레트
      scale_color_manual(values = c(
        "Chla" = "#000000", # Black
        setNames(hue_pal()(length(current_models)), current_models)
      )) +
      
      # 축 및 레이블
      labs(
        title = paste0(site_full_name, ": ", fam_name, " Family Comparison"),
        subtitle = paste0("Highlighting: Observed vs. Best Model (", best_model, ")"),
        x = "Date",
        y = expression(paste("Chlorophyll-a (mg/", m^3, ")")),
        color = "Legend"
      ) +
      
      # 시계열 축 설정 (2개월 단위)
      scale_x_date(date_labels = "%Y-%m", date_breaks = "2 month") +
      
      # 테마 설정 (SCI 논문 표준)
      theme_bw() +
      theme(
        text = element_text(family = "serif", size = 14), # 논문용 타임즈 폰트 계열
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.minor = element_blank()
      )
    
    # --------------------------------------------------------------------------
    # 고해상도 저장 (TIFF, 300DPI)
    # --------------------------------------------------------------------------
    file_name <- paste0("Fig_Final_", site_key, "_", fam_name, ".tiff")
    ggsave(file_name, p, width = 11, height = 6, dpi = 300, compression = "lzw")
    
    message(paste0("Success: ", file_name, " (Best Model: ", best_model, ")"))
  }
}
