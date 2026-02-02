# ==============================================================================
# [SCI 논문용] Best Model 강조 & 곡선형 시계열 그래프 (최종 수정반)
# 작성일: 2026.02.02
# ==============================================================================

# 1. 라이브러리 로드
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# 2. 파일 경로 설정 (사용자 환경에 맞게 수정해주세요)
files <- list(
  SC = "C:/Users/User/Desktop/df_SC_res_ver2.csv", # 승촌보
  JS = "C:/Users/User/Desktop/df_JS_res_ver2.csv"  # 죽산보
)

# 3. 모델 그룹 정의
# (1) 강조할 핵심 모델 리스트 (요청하신 3개 + 실제값)
focus_models <- c("Chla", "DNN_ADASYN", "CNN_SMOTE", "RNN")

# (2) 전체 모델 리스트 (데이터 처리를 위해 필요)
all_models <- c("Chla", "DNN", "DNN_SMOTE", "DNN_ADASYN", "DNN_SMOTE_ENN", "DNN_SMOTE_Tomek",
                "CNN", "CNN_SMOTE", "CNN_ADASYN", "CNN_SMOTE_ENN", "CNN_SMOTE_Tomek",
                "RNN", "LSTM")

# 4. 루프 실행 (지점별 그래프 생성)
for (site_key in names(files)) {
  
  # 데이터 로드
  df <- read_csv(files[[site_key]], show_col_types = FALSE)
  df$date <- as.Date(df$date)
  
  # 지점명 설정
  site_name <- ifelse(site_key == "SC", "Seungchon Weir", "Juksan Weir")
  
  # --------------------------------------------------------------------------
  # 데이터 전처리 (Long Format)
  # --------------------------------------------------------------------------
  df_long <- df %>%
    select(date, all_of(all_models)) %>%
    pivot_longer(
      cols = -date,
      names_to = "Model",
      values_to = "Value"
    )
  
  # --------------------------------------------------------------------------
  # [핵심 로직] 그룹핑 (Focus vs Others)
  # --------------------------------------------------------------------------
  df_long <- df_long %>%
    mutate(
      # 모델이 Focus 그룹에 속하면 해당 모델명을, 아니면 "Others"로 분류
      Model_Group = ifelse(Model %in% focus_models, Model, "Others"),
      
      # 시각화 속성 설정을 위한 분류
      Line_Type = ifelse(Model %in% focus_models, "Solid", "Dotted"),
      Line_Size = ifelse(Model %in% focus_models, "Thick", "Thin"),
      Line_Alpha = ifelse(Model %in% focus_models, "Opaque", "Transparent")
    )
  
  # --------------------------------------------------------------------------
  # [핵심 로직] 레이어 순서 조정 (Z-Order)
  # Others(회색 점선)를 먼저 그리고, Focus 모델을 나중에 그려야 덮어씌워짐
  # --------------------------------------------------------------------------
  # 순서: Others -> RNN -> CNN -> DNN -> Chla (실제값이 가장 위에 오도록)
  df_long$Model <- factor(df_long$Model, 
                          levels = c(setdiff(all_models, focus_models), 
                                     "RNN", "CNN_SMOTE", "DNN_ADASYN", "Chla"))
  
  df_long <- df_long %>% arrange(Model)
  
  # --------------------------------------------------------------------------
  # 그래프 그리기
  # --------------------------------------------------------------------------
  p <- ggplot(df_long, aes(x = date, y = Value, group = Model, color = Model, linetype = Model, size = Model, alpha = Model)) +
    
    # [곡선 처리] geom_smooth 사용
    # span: 곡선의 부드러운 정도 (0.1~0.2가 데이터 왜곡 없이 부드럽게 연결됨)
    # se = FALSE: 신뢰구간 그림자 제거
    geom_smooth(method = "loess", span = 0.3, se = FALSE) +
    
    # [색상 설정]
    # Focus 모델은 고유색, 나머지는 전부 연한 회색("grey70")으로 통일
    scale_color_manual(values = c(
      "Chla" = "black",            # 실제값: 검정
      "DNN_ADASYN" = "#D55E00",    # DNN: 진한 주황/빨강 계열 (눈에 잘 띔)
      "CNN_SMOTE" = "#0072B2",     # CNN: 파랑 계열
      "RNN" = "#009E73",           # RNN: 초록 계열
      
      # 나머지 모델들은 모두 회색 처리 (리스트에 없는 모델은 자동 매칭되므로 안심)
      "DNN" = "grey70", "DNN_SMOTE" = "grey70", "DNN_SMOTE_ENN" = "grey70", "DNN_SMOTE_Tomek" = "grey70",
      "CNN" = "grey70", "CNN_ADASYN" = "grey70", "CNN_SMOTE_ENN" = "grey70", "CNN_SMOTE_Tomek" = "grey70",
      "LSTM" = "grey70"
    )) +
    
    # [선 종류 설정] Focus는 실선, 나머지는 점선
    scale_linetype_manual(values = c(
      "Chla" = "solid", "DNN_ADASYN" = "solid", "CNN_SMOTE" = "solid", "RNN" = "solid",
      "DNN" = "dotted", "DNN_SMOTE" = "dotted", "DNN_SMOTE_ENN" = "dotted", "DNN_SMOTE_Tomek" = "dotted",
      "CNN" = "dotted", "CNN_ADASYN" = "dotted", "CNN_SMOTE_ENN" = "dotted", "CNN_SMOTE_Tomek" = "dotted",
      "LSTM" = "dotted"
    )) +
    
    # [선 굵기 설정] Focus는 1.2, 나머지는 0.5
    scale_size_manual(values = c(
      "Chla" = 1.2, "DNN_ADASYN" = 1.2, "CNN_SMOTE" = 1.2, "RNN" = 1.2,
      "DNN" = 0.6, "DNN_SMOTE" = 0.6, "DNN_SMOTE_ENN" = 0.6, "DNN_SMOTE_Tomek" = 0.6,
      "CNN" = 0.6, "CNN_ADASYN" = 0.6, "CNN_SMOTE_ENN" = 0.6, "CNN_SMOTE_Tomek" = 0.6,
      "LSTM" = 0.6
    )) +
    
    # [투명도 설정] Focus는 선명하게(1), 나머지는 흐릿하게(0.5)
    scale_alpha_manual(values = c(
      "Chla" = 1.0, "DNN_ADASYN" = 0.9, "CNN_SMOTE" = 0.9, "RNN" = 0.9,
      "DNN" = 0.5, "DNN_SMOTE" = 0.5, "DNN_SMOTE_ENN" = 0.5, "DNN_SMOTE_Tomek" = 0.5,
      "CNN" = 0.5, "CNN_ADASYN" = 0.5, "CNN_SMOTE_ENN" = 0.5, "CNN_SMOTE_Tomek" = 0.5,
      "LSTM" = 0.5
    )) +
    
    # 축 및 라벨 설정
    labs(
      title = paste0("Model Prediction Comparison (Smoothed) - ", site_name),
      subtitle = "Highlighting Best Models (DNN-ADASYN, CNN-SMOTE, RNN) vs Others",
      x = "Date",
      y = expression(paste("Chlorophyll-a (mg/", m^3, ")")),
      color = "Models" # 범례 제목
    ) +
    
    scale_x_date(date_labels = "%Y-%m", date_breaks = "2 month") +
    
    # 테마 설정
    theme_bw() +
    theme(
      text = element_text(family = "serif", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"), # 범례 선 길이 늘림 (점선/실선 구분을 위해)
      panel.grid.minor = element_blank()
    ) +
    
    # 범례 정리: 색상 범례만 남기고 나머지는 통합하거나 숨김
    guides(
      linetype = "none", 
      size = "none", 
      alpha = "none",
      color = guide_legend(override.aes = list(
        linetype = c("solid", "dotted", "dotted", "solid", "dotted", "dotted", "solid", "dotted", "solid", "dotted", "dotted", "dotted", "dotted"),
        size = 1.2 # 범례 아이콘 굵기
      ))
    )
  
  # 파일 저장
  file_name <- paste0("Plot_Smooth_Highlight_", site_name, ".tiff")
  ggsave(file_name, p, width = 12, height = 6, dpi = 300, compression = "lzw")
  
  print(paste("Saved:", file_name))
}







# ==============================================================================
# [SCI 논문용] 지점별/Family별 모델 비교 시계열 그래프 (가독성 최적화)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# 1. 파일 경로 설정
files <- list(
  SC = "C:/Users/User/Desktop/df_SC_res_ver2.csv",
  JS = "C:/Users/User/Desktop/df_JS_res_ver2.csv"
)

# 2. Family 정의 및 Best Model 설정
families <- list(
  DNN = list(models = c("DNN", "DNN_SMOTE", "DNN_ADASYN", "DNN_SMOTE_ENN", "DNN_SMOTE_Tomek"), 
             best = "DNN_ADASYN"),
  CNN = list(models = c("CNN", "CNN_SMOTE", "CNN_ADASYN", "CNN_SMOTE_ENN", "CNN_SMOTE_Tomek"), 
             best = "CNN_SMOTE"),
  TimeSeries = list(models = c("RNN", "LSTM"), 
                    best = "RNN")
)

# 3. 루프 실행: 지점별 -> Family별
for (site_key in names(files)) {
  site_name <- ifelse(site_key == "SC", "Seungchon Weir", "Juksan Weir")
  df <- read_csv(files[[site_key]], show_col_types = FALSE)
  df$date <- as.Date(df$date)
  
  for (fam_name in names(families)) {
    current_fam <- families[[fam_name]]
    target_models <- c("Chla", current_fam$models)
    best_model <- current_fam$best
    
    # 데이터 변환 (Wide -> Long)
    df_long <- df %>%
      select(date, all_of(target_models)) %>%
      pivot_longer(cols = -date, names_to = "Model", values_to = "Value")
    
    # 그래프 스타일 설정 (색상, 굵기, 선타입)
    df_long <- df_long %>%
      mutate(
        Type = case_when(
          Model == "Chla" ~ "Observed",
          Model == best_model ~ "Best",
          TRUE ~ "Others"
        ),
        # 범례 순서 고정 (실제값, Best, 나머지 순)
        Model = factor(Model, levels = c("Chla", best_model, setdiff(current_fam$models, best_model)))
      )
    
    # 그래프 생성
    p <- ggplot(df_long, aes(x = date, y = Value, color = Model, size = Type, linetype = Type)) +
      # 곡선 부드럽게 (기존 요청하신 span=0.3 유지)
      geom_smooth(method = "loess", span = 0.3, se = FALSE) +
      
      # 강조 스타일링
      scale_size_manual(values = c("Observed" = 1.2, "Best" = 1.1, "Others" = 0.6), guide = "none") +
      scale_linetype_manual(values = c("Observed" = "solid", "Best" = "solid", "Others" = "dashed"), guide = "none") +
      scale_color_manual(values = c(
        "Chla" = "black", 
        setNames(hue_pal()(length(current_fam$models)), current_fam$models)
      )) +
      
      labs(
        title = paste0(site_name, " - ", fam_name, " Family Comparison"),
        subtitle = paste0("Highlighted: Observed vs Best Model (", best_model, ")"),
        x = "Date",
        y = expression(paste("Chlorophyll-a (mg/", m^3, ")")),
        color = "Model List"
      ) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "2 month") +
      theme_bw() +
      theme(
        text = element_text(family = "serif", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
    
    # 저장
    file_name <- paste0("Plot_", site_key, "_", fam_name, "_Final.tiff")
    ggsave(file_name, p, width = 10, height = 6, dpi = 300, compression = "lzw")
    
    message(paste("Saved:", file_name))
  }
}
