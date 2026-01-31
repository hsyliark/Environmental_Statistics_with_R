# ==============================================================================
# [SCI 논문용] Chl-a 예측 모델 비교 시계열 그래프 그리기
# ==============================================================================

# 1. 라이브러리 로드 (설치되어 있지 않다면 install.packages("패키지명") 실행)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales) # 날짜 축 포맷팅용

# 2. 작업 경로 및 파일 설정 (파일이 있는 경로로 설정해주세요)
# setwd("C:/Users/사용자/Desktop/논문작업폴더") 

# 파일명 정의
files <- list(
  SC = "C:/Users/User/Desktop/df_SC_res_ver2.csv", # 승촌보
  JS = "C:/Users/User/Desktop/df_JS_res_ver2.csv"  # 죽산보
)

# 3. 모델 그룹 정의
# 실제값(Chla)은 비교를 위해 모든 그룹에 포함됩니다.
# 데이터의 컬럼명과 정확히 일치해야 합니다.

models_dnn <- c("Chla", "DNN", "DNN_SMOTE", "DNN_ADASYN", "DNN_SMOTE_ENN", "DNN_SMOTE_Tomek")
models_cnn <- c("Chla", "CNN", "CNN_SMOTE", "CNN_ADASYN", "CNN_SMOTE_ENN", "CNN_SMOTE_Tomek")
models_ts  <- c("Chla", "RNN", "LSTM")

# 그룹 정보를 리스트로 묶음
model_groups <- list(
  "DNN_Family" = models_dnn,
  "CNN_Family" = models_cnn,
  "TimeSeries_Family" = models_ts
)

# 4. 그래프 생성 함수 정의
create_and_save_plot <- function(site_name, file_path, group_name, model_list) {
  
  # 데이터 로드
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # 날짜 형식 변환 (혹시 문자로 인식될 경우를 대비)
  data$date <- as.Date(data$date)
  
  # 데이터 필터링 및 Long Format 변환 (ggplot2용)
  plot_data <- data %>%
    select(date, all_of(model_list)) %>%  # 해당 그룹의 모델만 선택
    pivot_longer(
      cols = -date, 
      names_to = "Model", 
      values_to = "Value"
    )
  
  # 'Chla' (실제값)이 범례의 가장 위에 오고, 그래프에서 가장 잘 보이도록 순서 조정
  plot_data$Model <- factor(plot_data$Model, levels = model_list)
  
  # 색상 팔레트 지정 (Chla는 검정색, 나머지는 자동 색상 또는 지정 색상)
  # 모델 개수에 맞춰 색상을 동적으로 생성하되, Chla는 항상 Black 고정
  n_colors <- length(model_list) - 1
  my_colors <- c("black", scales::hue_pal()(n_colors))
  names(my_colors) <- model_list
  
  # 선 굵기 지정 (Chla는 조금 더 굵게 1.2, 나머지는 0.8)
  my_sizes <- c(1.2, rep(0.7, n_colors))
  names(my_sizes) <- model_list
  
  # 선 타입 지정 (Chla는 실선, 나머지는 실선 혹은 점선 등 필요시 변경 가능)
  # 여기서는 모두 실선으로 하되 색상으로 구분
  
  # 그래프 그리기
  p <- ggplot(plot_data, aes(x = date, y = Value, color = Model, size = Model)) +
    geom_line(alpha = 0.9) + # 투명도 약간 주어 겹침 방지
    
    # 커스텀 스타일 적용
    scale_color_manual(values = my_colors) +
    scale_size_manual(values = my_sizes) +
    
    # 축 및 라벨 설정
    labs(
      title = paste0("Chlorophyll-a Prediction Comparison - ", site_name, " (", group_name, ")"),
      x = "Date",
      y = expression(paste("Chlorophyll-a (mg/", m^3, ")")), # 단위에 위첨자 적용
      color = "Models"
    ) +
    
    # 날짜 축 설정 (예: 1개월 단위, 필요시 "3 month" 등으로 변경)
    scale_x_date(date_labels = "%Y-%m", date_breaks = "2 month") +
    
    # 테마 설정 (논문용 깔끔한 테마)
    theme_bw() +
    theme(
      text = element_text(family = "serif", size = 14), # 폰트 설정 (Times New Roman 스타일)
      axis.text.x = element_text(angle = 45, hjust = 1), # x축 라벨 회전
      legend.position = "bottom", # 범례 하단 배치
      legend.title = element_blank(), # 범례 제목 제거 (깔끔함 유지)
      panel.grid.minor = element_blank() # 보조 눈금선 제거
    ) +
    guides(size = "none") # 사이즈 범례는 보여주지 않음 (색상 범례만 표시)
  
  # 파일 저장 (300dpi 고해상도 TIFF - 논문 제출용 표준)
  file_out_name <- paste0("Plot_", site_name, "_", group_name, ".tiff")
  ggsave(filename = file_out_name, plot = p, width = 10, height = 6, dpi = 300, compression = "lzw")
  
  print(paste("Saved:", file_out_name))
}

# 5. 실행 (루프를 돌며 그래프 생성)
# 두 지점(SC, JS)에 대해 각각 3개의 그룹 그래프를 생성 (총 6개)

for (site in names(files)) {
  for (group in names(model_groups)) {
    create_and_save_plot(
      site_name = ifelse(site == "SC", "Seungchon Weir", "Juksan Weir"), # 그래프 제목용 이름
      file_path = files[[site]],
      group_name = group,
      model_list = model_groups[[group]]
    )
  }
}
