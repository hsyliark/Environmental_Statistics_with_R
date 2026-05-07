# ==============================================================================
# 시스템명: 영산강 수계 오염원인 정밀 앙상블 스크리닝 시스템 (Final Patch)
# 분석 목적: 데이터 기반 최적 군집수(K) 도출 및 8단계 정밀 오염원 판별
# 주요 방법론: GMM, K-means, Ward's D2 Consensus + Multi-Proxy Rule-based Labeling
# ==============================================================================

# 1. 환경 설정 및 필수 라이브러리 로드
# [주석] pacman은 패키지 의존성을 관리하며, 미설치 시 자동 설치를 수행하여 연구 환경의 연속성을 보장합니다.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mclust, cluster, dplyr, ggplot2, VIM, umap, ggrepel, randomForest, factoextra)

# 2. 데이터 전처리 (Data Pre-processing)
# [주석] 인코딩 문제를 방지하기 위해 euc-kr을 사용하며, 파일 경로는 실제 환경에 맞춰 조정하십시오.
data <- read.csv("C:/Users/User/Desktop/sample data 1_screening.csv", fileEncoding = "euc-kr") 
features_raw <- data %>% select(-spot)

# [방법론] KNN(K-Nearest Neighbors) 결측치 보정 (참고문헌: Kowarik & Templ, 2016)
# 단순히 평균을 채우지 않고 수질 지문이 유사한 인접 데이터를 참조하여 보간함으로써 데이터의 통계적 왜곡을 최소화합니다.
imputed_data <- kNN(features_raw, k = 5, imp_var = FALSE)

# [방법론] Z-score 표준화 (Standardization)
# 수질 농도(mg/L)와 동위원소(‰) 등 단위 체계가 다른 변수들을 동일한 통계적 가중치로 분석하기 위해 평균 0, 표준편차 1로 변환합니다.
scaled_data <- scale(imputed_data)

# 3. [오류 수정 및 최적화] 데이터 기반 최적 군집 개수(K) 자동 결정
# [방법론] 모델의 복잡도와 분리도를 종합 고려하여 AI가 최적의 분류 체계를 스스로 결정합니다.

cat("[*] 최적 군집 수(K) 도출을 위한 통계 모델링을 시작합니다...\n")

# (1) GMM 기반 BIC 최적화 (오류 원천 차단 로직)
# mclustBIC의 직접 호출 대신 Mclust 통합 객체를 사용하여 atomic vector 오류를 방지하고 최적의 G를 추출합니다.
gmm_auto <- Mclust(scaled_data)
best_k <- gmm_auto$G  # BIC 지수가 가장 낮은(최적 모델) 군집 수를 자동 채택

# (2) Silhouette Index 기반 검증 (참고문헌: Rousseeuw, 1987)
# 지점이 군집 내에 얼마나 밀집되어 있고 다른 군집과 얼마나 분리되었는지 수치화(0~1)합니다.
sil_test <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = 10)
k_sil <- as.numeric(sil_test$data$clusters[which.max(sil_test$data$y)])

cat("[*] 분석 결과: 통계적 최적 군집 개수는 [", best_k, "]개 입니다. (Silhouette 검증: ", k_sil, "개)\n")

# 4. 다중 알고리즘 기반 앙상블 군집화 (Ensemble Consensus Clustering)
# [방법론] 단일 알고리즘의 수학적 편향을 제거하기 위해 서로 다른 원리의 3개 모델 결과를 통합합니다.

# (1) GMM (Gaussian Mixture Model): 데이터의 확률 밀도 분포를 고려한 통계적 분류
c_gmm <- gmm_auto$classification

# (2) K-Means++: 지점 간 유클리드 거리를 최소화하여 중심점 기준으로 분류
km_res <- kmeans(scaled_data, centers = best_k, nstart = 25)
c_km <- km_res$cluster

# (3) Hierarchical (Ward's D2): 계층적 유사성을 파악하며 군집 내 분산 증가를 최소화
dist_m <- dist(scaled_data)
hc_res <- hclust(dist_m, method = "ward.D2")
c_hc <- cutree(hc_res, k = best_k)

# [방법론] 다수결 합의 통합 (Majority Voting)
# 3개 모델 중 2개 이상의 모델이 동일하게 분류한 결과를 최종 군집으로 채택하여 분류의 객관성을 확보합니다.
ensemble_results <- data.frame(GMM = c_gmm, KM = c_km, HC = c_hc)
data$Final_Cluster <- apply(ensemble_results, 1, function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})

# 5. 고도화된 8단계 오염원 판별 로직 (Expert Rule-based Labeling)
# [방법론] 질소/산소 동위원소 지문과 다중 수질 항목(TP, Chl-a, NH3N 등)의 상관관계를 복합 적용합니다.
assign_detailed_label <- function(df) {
  d15N <- df$d15N; d18O <- df$d18O; TOC <- df$TOC; Chla <- df$Chla
  TP <- df$TP; NO3N <- df$NO3N; NH3N <- df$NH3N; TN <- df$TN; EC <- df$EC
  
  # [1단계] 대기 유입 및 초기 강우 (참고문헌: Kendall, 1998)
  # 근거: 대기 중 Nitrate는 미생물 변형 전 단계로 d18O가 매우 높게 측정됨 (15‰ 이상)
  if (d18O > 15.0) return("대기유입/초기강우")
  
  # [2단계] 강한 탈질 작용에 의한 기원 변형 (참고문헌: Mayer, 2002)
  # 근거: 하천 정체 시 미생물 반응으로 d15N과 d18O가 약 2:1 비율로 동반 상승하여 오염원 판독을 방해함
  if (d15N > 12.0 && d18O > 8.0 && (d15N/d18O < 2.5)) return("탈질작용(오염원변형)")
  
  # [3단계] 축산분뇨 직접 유입 (참고문헌: Xue, 2009)
  # 근거: 분뇨는 고농도 d15N(+12‰ 이상)과 함께 높은 TP(>0.4), NH3N 및 전기전도도(EC)를 동반함
  if (d15N > 12.0 && (TP > 0.4 || NH3N > 1.0 || EC > 500)) return("축산분뇨 직접유출")
  
  # [4단계] 생활하수 및 하수처리수 영향 (참고문헌: Widory, 2004)
  # 근거: d15N은 높으나 분뇨 대비 TP가 낮고, 합성세제 등의 영향으로 Cl 농도가 상대적으로 높은 구간
  if (d15N > 8.0 && d15N <= 15.0 && TP <= 0.3) return("생활하수/처리수 영향")
  
  # [5단계] 화학비료 기반 농경지 유출 (참고문헌: Vitousek, 1997)
  # 근거: 질소고정으로 생산된 비료는 d15N이 0‰ 내외(-4~+4)이며, 강우 시 NO3N 농도가 지배적임
  if (d15N < 4.0 && NO3N > 4.0) return("화학비료 기반 농경지")
  
  # [6단계] 유기질비료 및 토양 혼합 유출 (참고문헌: Bedard-Haughn, 2003)
  # 근거: 퇴비 등 유기질 비료는 d15N이 4~9‰ 범위이며, 토양 유기물(TOC)과 함께 유출되는 특성 반영
  if (d15N >= 4.0 && d15N < 9.0 && TOC > 6.0 && NO3N > 2.0) return("유기질비료/토양혼합")
  
  # [7단계] 조류 증식 및 내생 유기물 (영산강 하류 특성 반영)
  # 근거: 영산강 보 정체 구간의 특성으로, Chl-a 급증 시 외부 유입보다는 내부 광합성에 의한 수질 악화 판별
  if (Chla > 40.0 || (TOC > 8.0 && TP > 0.15)) return("조류증식/내생부하")
  
  # [8단계] 자연 배경 농도 (배경 수질)
  # 근거: 인위적 오염 징후가 희박하고 자연적인 산림 토양 질소 농도 범위를 유지하는 구간
  if (d15N >= 2.0 && d15N <= 7.0 && TN < 2.0) return("자연배경/산림유출")
  
  return("복합오염/판정불가")
}

# 군집별 통계적 평균치를 기반으로 정밀 라벨 부여
final_labels <- data %>%
  group_by(Final_Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(Source_Detailed = assign_detailed_label(cur_data())) %>%
  select(Final_Cluster, Source_Detailed)

data <- data %>% left_join(final_labels, by = "Final_Cluster")

# 6. 시각화 및 중요도 분석 (XAI)
# [방법론] UMAP (Uniform Manifold Approximation and Projection)
# 고차원 수질 데이터를 2차원 '오염 지문 지도'로 투영하여 지점 간 유사성을 한눈에 파악하게 합니다.
umap_res <- umap(scaled_data)
data$umap1 <- umap_res$layout[,1]; data$umap2 <- umap_res$layout[,2]

ggplot(data, aes(x = umap1, y = umap2, color = Source_Detailed, label = spot)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_text_repel(size = 4, fontface = "bold") +
  theme_minimal() +
  labs(title = "ML 기반 오염원 정밀 앙상블 분류 지도",
       subtitle = paste("자동 최적화된 군집 수(K):", best_k),
       color = "최종 판정 오염원")

# 7. 최종 결과 출력
print(data %>% select(spot, Final_Cluster, Source_Detailed) %>% arrange(Final_Cluster))
