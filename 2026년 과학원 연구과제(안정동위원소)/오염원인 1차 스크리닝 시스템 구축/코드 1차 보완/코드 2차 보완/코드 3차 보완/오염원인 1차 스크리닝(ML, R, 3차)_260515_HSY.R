# ==============================================================================
# 시스템명: 국가 수계 통합 오염원인 정밀 앙상블 스크리닝 시스템 (R Ver.)
# 연구목적: 안정동위원소 및 수질 데이터를 활용한 오염원 판별 자동화 및 고도화
# ==============================================================================

# [패키지 로드]  
# 방법론: pacman을 통한 자동 환경 구축. 
# 구성: VIM(kNN 결측치), mclust(GMM), randomForest(변수 기여도), umap(차원축소), rpart(의사결정나무) 등 활용
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mclust, cluster, dplyr, ggplot2, VIM, umap, ggrepel, 
               randomForest, factoextra, rpart, rpart.plot, purrr)

# Windows 환경 한글 깨짐 방지 폰트 설정
if (.Platform$OS.type == "windows") {
  windowsFonts(malgun = windowsFont("Malgun Gothic"))
}

# [재현성 설정] 결과의 일관성 및 알고리즘 비교의 신뢰성을 위해 시드 고정
SEED <- 3606

# ------------------------------------------------------------------------------
# 1. 데이터 전처리 (Preprocessing)
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# 1. kNN (k-Nearest Neighbors) Imputation: 
#    단순 평균 대치법의 한계를 극복하기 위해 다차원 공간상에서 가장 유사한 k=5개의 
#    관측치 가중 평균으로 결측치를 보간합니다.
#    * SCI Ref: Kowarik, A., & Templ, M. (2016). Imputation with the R Package VIM. Journal of Statistical Software, 74(7), 1-16.
# 2. Z-score Normalization (Scaling):
#    수질 인자(mg/L, μg/L)와 동위원소(‰) 간의 단위 및 스케일 차이로 인해 
#    거리 기반 알고리즘(K-means 등)이 왜곡되는 것을 방지합니다.
# ------------------------------------------------------------------------------
data <- read.csv("C:/Users/User/Desktop/특이측정값(3~4월).csv", fileEncoding = "CP949") 
features_raw <- data %>% select(-spot)

set.seed(SEED)
imputed_data <- kNN(features_raw, k = 5, imp_var = FALSE) # 결측치 보정 (kNN)
scaled_data <- scale(imputed_data) # 표준화 진행 (평균 0, 분산 1)

# ------------------------------------------------------------------------------
# 2. 다중 알고리즘 앙상블 군집화 (Consensus Ensemble Clustering)
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# 수질 데이터는 선형/비선형적 특성이 혼재하므로 단일 군집화 알고리즘의 편향(Bias)을
# 제거하기 위해 5가지 알고리즘의 결과를 통합(Majority Voting)합니다.
# * SCI Ref: Monti, S., et al. (2003). Consensus clustering: a resampling-based method 
#            for class discovery and visualization of gene expression microarray data. Machine Learning, 52(1), 91-118.
# ------------------------------------------------------------------------------
set.seed(SEED)
n_rows <- nrow(scaled_data)
# 군집 탐색 범위 설정 (데이터 수가 적을 경우 동적으로 조정)
search_range <- if(n_rows < 9) 2:min(8, n_rows - 1) else 4:8

# 2.1 최적 군집 수 탐색 (k selection)
# [방법론] BIC, Silhouette, Gap Statistic 3가지 평가 지표의 다수결 적용
# * SCI Ref (Gap Stat): Tibshirani, R., et al. (2001). Estimating the number of clusters in a data set via the gap statistic. JRSS Series B, 63(2), 411-423.
gmm_auto <- Mclust(scaled_data, G = search_range)
best_k_bic <- ifelse(!is.null(gmm_auto$G), gmm_auto$G, 4)

if (n_rows > 4) {
  dynamic_k_max <- min(8, n_rows - 1)
  sil_test <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = dynamic_k_max)
  best_k_sil <- max(4, min(as.numeric(sil_test$data$clusters[which.max(sil_test$data$y)]), 8))
  
  gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = dynamic_k_max, B = 50)
  best_k_gap <- max(4, min(maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method="Tibs2001SEmax"), 8))
} else {
  best_k_sil <- 4; best_k_gap <- 4
}

# 다수결에 의한 최종 k 확정
k_votes <- na.omit(c(best_k_bic, best_k_sil, best_k_gap))
calculated_k <- as.numeric(names(sort(table(k_votes), decreasing = TRUE)[1]))
best_k <- max(4, min(calculated_k, 8))

# 2.2 앙상블 군집 수행
set.seed(SEED)
c_gmm <- Mclust(scaled_data, G = best_k)$classification             # 확률분포 기반
c_km  <- kmeans(scaled_data, centers = best_k, nstart = 25)$cluster # 거리 기반
c_hc  <- cutree(hclust(dist(scaled_data), method = "ward.D2"), k = best_k) # 계층 구조 분산 최소화
c_pam <- pam(scaled_data, k = best_k)$clustering                    # K-medoids (이상치에 강건)
c_diana <- cutree(diana(scaled_data), k = best_k)                   # 하향식 계층 분할

# 최빈값(Majority Voting) 기반 최종 군집 할당
ensemble_results <- data.frame(GMM = c_gmm, KM = c_km, HC = c_hc, PAM = c_pam, DIANA = c_diana)
data$Final_Cluster <- apply(ensemble_results, 1, function(x) {
  ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
})

# ------------------------------------------------------------------------------
# 3. 범용적 10단계 오염원 정밀 판별 엔진 (Rule-based Expert System)
# ------------------------------------------------------------------------------
# [분석과정 및 오염원 판별 기준]
# 질소/산소 안정동위원소 지문(Isotopic Signatures)과 수질 항목의 이온 비(Ion ratios)를
# 융합하여 기원을 역추적하는 결정 규칙 기반 알고리즘입니다.
# * SCI Ref 1 (Isotope Signatures): Kendall, C. (1998). Tracing nitrogen sources and cycling in catchments. Isotope Tracers in Catchment Hydrology, 519-576.
# * SCI Ref 2 (Source Apportionment): Xue, D., et al. (2009). Present limitations and future prospects of stable isotope methods for nitrate source identification. Water Research, 43(5), 1159-1170.
# ------------------------------------------------------------------------------
assign_source_universal <- function(row) {
  n15 <- row$d15N; o18 <- row$d18O; cl <- row$Cl
  tp <- row$TP; nh3 <- row$NH3N; no3 <- row$NO3N
  toc <- row$TOC; chl <- row$Chla; ec <- row$EC
  
  # [판별 로직 - SCI 문헌 임계치 적용]
  # 1. 대기 기원 (Atmospheric Deposition): d18O가 15‰ 이상으로 극도로 높은 특징 (Kendall, 1998)
  if (o18 > 15.0) return("대기유입/초기강우") 
  
  # 2. 탈질작용 (Denitrification): 혐기성 조건에서 d15N과 d18O가 1:2 ~ 1:1 비율로 동반 상승 (Xue et al., 2009)
  if (n15 > 6.0 && o18 > 3.0 && (n15/o18 >= 1.0 && n15/o18 <= 2.5)) return("탈질작용(기원변형)") 
  
  # 3. 축산분뇨 (Manure): d15N이 10~20‰ 이상으로 높고 휘발/질산화 과정에서 암모니아 및 염소 이온 농도 높음
  if (n15 > 12.0 && (cl > 60 || nh3 > 0.3)) return("축산분뇨 직접유출") 
  
  # 4. 생활하수 (Sewage): d15N이 8~12‰ 수준이며 하수 유래 염소이온 수반, 상대적으로 TP 낮거나 관리된 형태
  if (n15 > 8.0 && n15 <= 12.0 && (cl > 40 || tp <= 0.3)) return("생활하수/처리수 영향") 
  
  # 5. 산업폐수/인위적 점오염: d15N이 7‰ 이상이면서 비정상적으로 높은 전기전도도(EC)와 TOC 수반
  if (n15 > 7.0 && (ec > 500 || cl > 100) && toc > 3.0) return("산업/인위적 점오염")
  
  # 6. 화학비료 (Chemical Fertilizer): 공기 중 질소 고정으로 인해 d15N이 0~4‰ 범위 내외 유지 (Kendall, 1998)
  if (n15 < 4.0 && no3 > 2.0 && cl < 30) return("화학비료 기반 농경지") 
  
  # 7. 토양 유기물/유기질 비료 (Soil Nitrogen / Compost): d15N이 4~9‰ 수준이며, 염소 농도에 따라 퇴비와 자연토양 구분
  if (n15 >= 4.0 && n15 < 9.0 && toc > 4.0) {
    return(ifelse(cl > 35, "유기질비료(퇴비) 영향", "토양유기물/야생동물배설물"))
  }
  
  # 8. 조류 증식/내생 부하: 엽록소a 와 TOC, TP를 기준으로 수계 내부 발생 오염 추적
  if (chl > 40.0 && toc > 4.5) return("조류증식(현장발생)")
  if (toc > 6.5 && tp > 0.1) return("내생부하(정체수역 유기물 축적)")
  
  # 9. 복합 오염 및 배경농도
  if (tp > 0.1 || cl > 50) return("복합오염(하수/분뇨 기여 우세)")
  if (no3 > 2.0) return("복합오염(농경지 비점 우세)")
  return("자연배경/특이징후 없음")
}

# [보완] 개별 포인트 단위 오염원 판별 후, 앙상블 군집(Cluster)별 대표 Mode 선정
imputed_data_df <- as.data.frame(imputed_data)
imputed_data_df$Individual_Source <- apply(imputed_data_df, 1, function(r) assign_source_universal(as.list(r)))
imputed_data_df$Final_Cluster <- data$Final_Cluster

# 군집 내 최빈값 추출 함수
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cluster_mode <- imputed_data_df %>%
  group_by(Final_Cluster) %>%
  summarise(Source_Name = get_mode(Individual_Source), .groups = 'drop')

data <- data %>% left_join(cluster_mode, by = "Final_Cluster")

# ------------------------------------------------------------------------------
# 4. 시각화 및 설명 가능한 AI (XAI: Explainable AI)
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# 복잡한 다차원 결합 데이터를 현장 실무자가 쉽게 이해할 수 있도록 XAI 기법 적용
# ------------------------------------------------------------------------------
set.seed(SEED)

# (1) UMAP 시각화 (Uniform Manifold Approximation and Projection)
# [방법론] 위상수학(Topology)에 기반하여 데이터의 지역적, 전역적 구조를 모두 보존하며 2차원으로 차원 축소
# * SCI Ref: McInnes, L., Healy, J., & Melville, J. (2018). UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction. arXiv preprint arXiv:1802.03426.
n_neighbors_val <- ifelse(nrow(scaled_data) > 2, min(15, nrow(scaled_data) - 1), 2)
umap_res <- umap(scaled_data, n_neighbors = n_neighbors_val)
data$umap1 <- umap_res$layout[,1]; data$umap2 <- umap_res$layout[,2]

ggplot(data, aes(x = umap1, y = umap2, color = Source_Name, label = spot)) +
  geom_point(size = 5, alpha = 0.7) + geom_text_repel(family = "malgun") +
  labs(title = "UMAP 기반 오염원 분포 지도", subtitle = "안정동위원소-수질 융합 군집") +
  theme_minimal(base_family = "malgun")

# (2) 의사결정 트리 (Decision Logic Tree)
# [방법론] CART(Classification And Regression Trees) 알고리즘을 활용한 해석 가능한 규칙 도출. Gini Index를 통해 불순도를 최소화하는 노드를 분할합니다.
# * SCI Ref: Breiman, L., Friedman, J., Stone, C. J., & Olshen, R. A. (1984). Classification and Regression Trees. CRC press.
set.seed(SEED)
tree_model <- rpart(Source_Name ~ d15N + d18O + TP + NH3N + NO3N + TOC + EC + Chla + TN + Cl, 
                    data = data, method = "class",
                    control = rpart.control(cp = 0.001, minsplit = 2))
rpart.plot(tree_model, main = "오염원 판별 의사결정 로직 트리 (분류 근거 가시화)",
           type = 4, extra = 104, fallen.leaves = TRUE, box.palette = "RdYlGn",
           family = "malgun", cex = 0.7)

# (3) 변수 중요도 산출 (Random Forest 활용)
# [방법론] 다수의 의사결정나무를 앙상블하여 예측력을 높이고, 각 수질 변수가 군집 판별에 미치는 중요도(Mean Decrease Accuracy/Gini)를 산출합니다.
# * SCI Ref: Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.
set.seed(SEED)
rf_model <- randomForest(as.factor(data$Source_Name) ~ ., 
                         data = as.data.frame(imputed_data), 
                         importance = TRUE)
varImpPlot(rf_model, main = "오염원 군집화 결정 주도 변수 기여도", family = "malgun")

# ==============================================================================
# 7. 최종 레포트 출력
# ==============================================================================
cat("\n[최종 오염원 스크리닝 레포트]\n")
print(data %>% select(spot, Source_Name) %>% arrange(Source_Name))
