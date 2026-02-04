# 1. 라이브러리 로드
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork) # 그래프 배치를 위해 사용
library(viridis)   # 세련된 컬러맵 사용

# 2. 데이터 불러오기 (파일 경로에 맞게 수정 필요)
df_sc <- read_csv("C:/Users/User/Desktop/seungchon_2012-2025.csv", 
                  show_col_types = FALSE, locale = locale(encoding = "EUC-KR"))
df_js <- read_csv("C:/Users/User/Desktop/juksan_2012-2025.csv", 
                  show_col_types = FALSE, locale = locale(encoding = "EUC-KR"))

# 3. 데이터 전처리 통합 함수
preprocess_data <- function(df, site_name_eng) {
  df %>%
    mutate(
      Date_obj = as.Date(Date, format = "%Y.%m.%d"), # 날짜 변환
      Year = as.numeric(format(Date_obj, "%Y")),
      Month = as.numeric(format(Date_obj, "%m")),
      Site = site_name_eng # 영문 지점명 추가
    ) %>%
    filter(Year >= 2012 & Year <= 2024) # 2012~2024년 데이터만 추출 (Training Period)
}

# 전처리 실행 및 데이터 병합
df_sc_clean <- preprocess_data(df_sc, "Seungchon Weir")
df_js_clean <- preprocess_data(df_js, "Juksan Weir")
df_total <- bind_rows(df_sc_clean, df_js_clean)

# [핵심 수정 부분] 지점 순서를 승촌보 -> 죽산보 순서로 고정
df_total$Site <- factor(df_total$Site, levels = c("Seungchon Weir", "Juksan Weir"))

# ==============================================================================
# [Figure 1] 월별 Chl-a 분포 (Seasonal Boxplot)
# ==============================================================================
p1 <- ggplot(df_total, aes(x = factor(Month), y = Chla, fill = Site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 1, outlier.size = 1) +
  scale_fill_manual(values = c("Seungchon Weir" = "#4E84C4", "Juksan Weir" = "#C4961A")) +
  labs(
    title = "(a) Monthly Distribution of Chlorophyll-a (2012-2024)",
    x = "Month",
    y = "Chlorophyll-a (mg/m³)"
  ) +
  theme_bw() +
  theme(legend.position = "top")

# ==============================================================================
# [Figure 2] 수온(WT)-총인(TP)-Chla 상호작용 (Interaction Scatter Plot)
# 설명: 수온이 높아도 TP가 낮으면 Chl-a가 낮고, 둘 다 높을 때 폭발함을 보여줌 -> DNN 당위성
# ==============================================================================
p2 <- ggplot(df_total, aes(x = WT, y = Chla, color = TP)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~Site) +
  scale_color_viridis(option = "D", name = "TP (mg/L)", direction = -1) +
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed", size=0.5) +
  labs(
    title = "(b) Interaction of WT and TP on Chlorophyll-a",
    x = "Water Temperature (°C)",
    y = "Chlorophyll-a (mg/m³)"
  ) +
  theme_bw() +
  theme(legend.position = "right")

# ==============================================================================
# [Figure 3] 데이터 불균형 확인 (Density Plot)
# 설명: 데이터가 왼쪽에 쏠려있고(Imbalance), 고농도 데이터가 적음을 보여줌 -> ADASYN 당위성
# ==============================================================================
p3 <- ggplot(df_total, aes(x = Chla, fill = Site)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 25, linetype="dashed", color="black") + # 예: 조류 경계 기준
  annotate("text", x=30, y=0.04, label="Threshold", hjust=0, size=3) +
  scale_fill_manual(values = c("Seungchon Weir" = "#4E84C4", "Juksan Weir" = "#C4961A")) +
  labs(
    title = "(c) Density Distribution of Chlorophyll-a (Data Imbalance)",
    x = "Chlorophyll-a (mg/m³)",
    y = "Density"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# ==============================================================================
# [종합] 그래프 합치기 및 저장 (Patchwork 라이브러리 활용)
# ==============================================================================
# 배치: (a)가 위에, (b)와 (c)가 아래에 배치
final_plot <- p1 / (p2 + p3) + 
  plot_annotation(tag_levels = 'a') # a, b, c 태그 자동 생성 방지 (타이틀에 넣었으므로)

# 화면 출력
print(final_plot)

# 파일 저장 (고해상도 TIFF 권장 for SCI)
ggsave("C:/Users/User/Desktop/EDA_Combined_Figure.tiff", final_plot, 
       width = 12, height = 10, dpi = 300, compression = "lzw")
ggsave("C:/Users/User/Desktop/EDA_Combined_Figure.png", final_plot, 
       width = 12, height = 10, dpi = 300)
