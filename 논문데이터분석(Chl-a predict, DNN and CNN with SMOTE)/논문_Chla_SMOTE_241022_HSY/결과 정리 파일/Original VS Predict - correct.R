# ==============================================================================
# [SCI 논문용] Best Model 강조형 시계열 그래프 그리기
# 작성일: 2026.02.02
# ==============================================================================

# 1. 라이브러리 로드
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# 2. 파일 경로 설정 (사용자 환경에 맞게 수정 필요)
# csv 파일이 있는 경로를 정확히 입력해주세요.
files <- list(
  SC = "C:/Users/User/Desktop/df_SC_res_ver2.csv", # 승촌보 파일 경로
  JS = "C:/Users/User/Desktop/df_JS_res_ver2.csv"  # 죽산보 파일 경로
)

# 3. 모델 그룹 정의
models_dnn <- c("DNN", "DNN_SMOTE", "DNN_ADASYN", "DNN_SMOTE_ENN", "DNN_SMOTE_Tomek")
models_cnn <- c("CNN", "CNN_SMOTE", "CNN_ADASYN", "CNN_SMOTE_ENN", "CNN_SMOTE_Tomek")
models_ts  <- c("RNN", "LSTM")

# 4. 루프 실행 (지점별 x 모델 Family별 그래프 생성)
for (site_key in names(files)) {
  
  # 데이터 로드
  df <- read_csv(files[[site_key]], show_col_types = FALSE)
  
  # 날짜 형식 변환
  df$date <- as.Date(df$date)
  
  # 지점명 설정 (그래프 제목용)
  site_name <- ifelse(site_key == "SC", "Seungchon Weir", "Juksan Weir")
  
  # 3가지 Family에 대해 반복
  families <- list(DNN = models_dnn, CNN = models_cnn, TimeSeries = models_ts)
  
  for (fam_name in names(families)) {
    
    # 현재 Family에 해당하는 모델 리스트
    current_models <- families[[fam_name]]
    
    # --------------------------------------------------------------------------
    # [핵심 수정 1] 지점 및 Family별 Best Model 지정 (요청하신 사항 반영)
    # --------------------------------------------------------------------------
    best_model <- ""
    
    if (site_key == "SC") { # 승촌보
      if (fam_name == "DNN") best_model <- "DNN_ADASYN"
      else if (fam_name == "CNN") best_model <- "CNN_SMOTE"
      else if (fam_name == "TimeSeries") best_model <- "RNN"
    } else { # 죽산보
      if (fam_name == "DNN") best_model <- "DNN_ADASYN"
      else if (fam_name == "CNN") best_model <- "CNN_SMOTE"
      else if (fam_name == "TimeSeries") best_model <- "RNN"
    }
    
    # --------------------------------------------------------------------------
    # 데이터 전처리 (Long Format 변환)
    # --------------------------------------------------------------------------
    # 실제값(Chla)과 현재 Family 모델들만 선택
    select_cols <- c("date", "Chla", current_models)
    
    df_long <- df %>%
      select(all_of(select_cols)) %>%
      pivot_longer(
        cols = -date,
        names_to = "Model",
        values_to = "Value"
      )
    
    # --------------------------------------------------------------------------
    # [핵심 수정 2] 시각화 그룹(Line Type) 설정
    # Group: "Observed" (실제값), "Best" (최우수모델), "Others" (기타모델)
    # --------------------------------------------------------------------------
    df_long <- df_long %>%
      mutate(
        Highlight_Group = case_when(
          Model == "Chla" ~ "Observed",
          Model == best_model ~ "Best",
          TRUE ~ "Others"
        )
      )
    
    # --------------------------------------------------------------------------
    # [핵심 수정 3] 그리는 순서(Layer) 조정
    # Others를 먼저 그리고, Best와 Observed를 나중에 그려야 덮어써서 잘 보임
    # --------------------------------------------------------------------------
    df_long$Highlight_Group <- factor(df_long$Highlight_Group, levels = c("Others", "Best", "Observed"))
    df_long <- df_long %>% arrange(Highlight_Group) 
    
    # --------------------------------------------------------------------------
    # 그래프 그리기
    # --------------------------------------------------------------------------
    p <- ggplot(df_long, aes(x = date, y = Value, color = Model, size = Highlight_Group, alpha = Highlight_Group)) +
      
      geom_line() +
      
      # [스타일 설정]
      # 1. 굵기(Size): 실제값과 Best 모델은 1.2로 굵게, 나머지는 0.5로 얇게
      scale_size_manual(values = c("Observed" = 1.2, "Best" = 1.2, "Others" = 0.5)) +
      
      # 2. 투명도(Alpha): 실제값과 Best 모델은 불투명(1), 나머지는 약간 흐리게(0.6)
      scale_alpha_manual(values = c("Observed" = 1.0, "Best" = 1.0, "Others" = 0.6)) +
      
      # 3. 색상(Color): 실제값은 검정, 나머지는 ggplot 기본 색상 또는 커스텀
      scale_color_manual(values = c(
        "Chla" = "black", 
        # 필요시 나머지 모델 색상도 지정 가능, 여기선 자동 할당에 맡김
        setNames(rainbow(length(current_models)), current_models) 
      )) +
      
      # 축 및 라벨 설정
      labs(
        title = paste0("Prediction Comparison - ", site_name, " (", fam_name, " Family)"),
        subtitle = paste0("Highlighting Best Model: ", best_model),
        x = "Date",
        y = expression(paste("Chlorophyll-a (mg/", m^3, ")")),
        color = "Models"
      ) +
      
      scale_x_date(date_labels = "%Y-%m", date_breaks = "2 month") +
      
      # 테마 설정
      theme_bw() +
      theme(
        text = element_text(family = "serif", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      
      # 범례 가이드 정리 (Size/Alpha 범례는 숨기고 Color 범례만 표시)
      guides(size = "none", alpha = "none")
    
    # 파일 저장 (고해상도 TIFF)
    file_name <- paste0("Plot_Highlight_", site_name, "_", fam_name, ".tiff")
    ggsave(file_name, p, width = 12, height = 6, dpi = 300, compression = "lzw")
    
    print(paste("Saved:", file_name))
  }
}
