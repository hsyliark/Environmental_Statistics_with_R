# ==============================================================================
# [논문용] 승촌보 vs 죽산보 데이터 비교 분석 (Target: 2012-2024)
# 작성일: 2025-02 (Year filtering applied)
# ==============================================================================

# 1. 라이브러리 로드
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggpubr")) install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(ggpubr)

# 2. 데이터 불러오기 및 전처리 (인코딩 오류 방지 포함)
# 파일 경로는 실제 환경에 맞춰 수정하세요.
df_sc_raw <- read.csv("C:/Users/User/Desktop/seungchon_2012-2025.csv", 
                      header=TRUE, na.strings = c("", "NA", "-"), fileEncoding = "EUC-KR")
df_js_raw <- read.csv("C:/Users/User/Desktop/juksan_2012-2025.csv", 
                      header=TRUE, na.strings = c("", "NA", "-"), fileEncoding = "EUC-KR")

# [핵심 수정] 2012년부터 2024년 데이터만 필터링
df_sc <- df_sc_raw %>% filter(Year >= 2012 & Year <= 2024)
df_js <- df_js_raw %>% filter(Year >= 2012 & Year <= 2024)

# 지점 구분 컬럼 추가
df_sc$Site <- "Seungchon Weir"
df_js$Site <- "Juksan Weir"

# 데이터 병합 (EDA용 전체 데이터)
df_all <- rbind(df_sc, df_js)

# 데이터 타입 변환 (숫자형 보장)
cols_to_numeric <- c("Chla", "WL", "RF", "Year")
df_all[cols_to_numeric] <- lapply(df_all[cols_to_numeric], as.numeric)

# 결측치 제거 및 요인 순서 지정 (승촌보-죽산보 순서)
df_clean <- df_all %>% filter(!is.na(WL) & !is.na(Chla) & !is.na(RF))
df_clean$Site <- factor(df_clean$Site, levels = c("Seungchon Weir", "Juksan Weir"))

# ==============================================================================
# [Fig A] 수위(WL)와 Chl-a의 관계 (2012-2024)
# ==============================================================================
p1 <- ggplot(df_clean, aes(x=WL, y=Chla)) +
  geom_point(alpha=0.3, color="darkgreen", size=1.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color="red", size=1, fill="red", alpha=0.1) +
  facet_wrap(~Site, scales = "free", ncol=2) +
  theme_bw(base_size = 14) +
  labs(x = "Water Level (EL.m)", 
       y = expression(paste("Chlorophyll-a (mg/", m^3, ")"))) + 
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey90"),
        strip.text = element_text(face="bold", size=12))

# ==============================================================================
# [Fig B] 강우량(RF)과 Chl-a의 시차 상관분석 (CCF) 데이터 생성
# ==============================================================================
get_ccf_data <- function(sub_df, site_name) {
  # 2012-2024 데이터 한정 (안전을 위해 다시 필터링)
  valid_data <- sub_df %>% 
    filter(Year <= 2024) %>% 
    filter(!is.na(RF) & !is.na(Chla))
  
  ccf_res <- ccf(valid_data$RF, valid_data$Chla, lag.max=14, plot=FALSE)
  
  data.frame(
    Lag = ccf_res$lag,
    ACF = ccf_res$acf,
    Site = site_name
  )
}

# 각 지점별 CCF 계산 및 통합
ccf_sc <- get_ccf_data(df_sc, "Seungchon Weir")
ccf_js <- get_ccf_data(df_js, "Juksan Weir")
ccf_all <- rbind(ccf_sc, ccf_js)
ccf_all$Site <- factor(ccf_all$Site, levels = c("Seungchon Weir", "Juksan Weir"))

# 시각화
p2 <- ggplot(ccf_all, aes(x=Lag, y=ACF)) +
  geom_bar(stat="identity", fill="steelblue", width=0.7) +
  geom_hline(yintercept = c(0.1, -0.1), linetype="dashed", color="red", alpha=0.5) + # 유의선 가이드
  facet_wrap(~Site, ncol=2) +
  theme_bw(base_size = 14) +
  labs(x = "Lag (Days)", 
       y = "Cross-Correlation Coefficient") +
  scale_x_continuous(breaks = seq(-14, 14, 2)) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey90"),
        strip.text = element_text(face="bold", size=12))

# ==============================================================================
# 3. 고해상도 이미지 저장 (300 DPI)
# ==============================================================================
ggsave("Fig_EDA_WL_2012-2024.tiff", plot=p1, width=10, height=5, dpi=300, compression="lzw")
ggsave("Fig_EDA_Rain_Lag_2012-2024.tiff", plot=p2, width=10, height=5, dpi=300, compression="lzw")

print("2012-2024 기간에 대한 분석 결과 저장이 완료되었습니다.")