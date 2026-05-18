# ==============================================================================
# 시스템명: 국가 수계 통합 오염원인 정밀 앙상블 스크리닝 시스템 (R Ver.)
# 연구목적: 안정동위원소 및 수질 데이터를 활용한 오염원 판별 자동화 및 고도화
# ==============================================================================

# [패키지 로드]     
# 방법론: pacman을 통한 자동 환경 구축. 
# 구성: VIM(kNN 결측치), mclust(GMM), randomForest(변수 기여도), umap(차원축소), rpart(의사결정나무) 등 활용
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mclust, cluster, dplyr, ggplot2, VIM, umap, ggrepel, 
               randomForest, factoextra, rpart, rpart.plot, purrr, fpc)

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
if (length(unique(k_votes)) == length(k_votes)) {
  calculated_k <- round(median(k_votes))  # 또는 최빈값이 없으면 중앙값
} else {
  calculated_k <- as.numeric(names(sort(table(k_votes), decreasing = TRUE)[1]))
}
best_k <- max(4, min(calculated_k, 8))

# 군집 안정성(Cluster Stability) 정량화
# 본 cluster 구조가 다음주에 데이터 1개 추가되면 뒤집힐 정도로 불안정한지 확인
boot <- clusterboot(scaled_data, B=200, clustermethod=kmeansCBI, krange=best_k, seed=SEED)
cat("[*] Cluster Jaccard 안정성:", round(boot$bootmean, 2), "\n") # 0.75 이상이면 안정, 0.6 미만이면 불안정

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

# 분류 신뢰도(Confidence) 지표 동시 출력
# 개별 알고리즘 간 합치율을 정량화하여 '연구진이 추가 검토해야 할 지점'을 자동 플래깅
# R: 5개 알고리즘 중 몇 개가 majority label에 동의했는지
data$Ensemble_Confidence <- apply(ensemble_results, 1, function(x) {
  max(tabulate(match(x, unique(x)))) / length(x)
})
# 0.4 미만이면 "토론 필요" 플래그
data$Discussion_Flag <- ifelse(data$Ensemble_Confidence < 0.6, "토론 필요", "양호")

# ------------------------------------------------------------------------------
# 3. 범용적 10단계 오염원 정밀 판별 엔진 (Rule-based Expert System) - 고도화 버전
# ------------------------------------------------------------------------------
# [분석과정 및 오염원 판별 기준]
# 질소/산소 안정동위원소 지문(Isotopic Signatures)과 수화학 지표(Hydrochemical proxies)를
# 융합하여 기원을 역추적하는 결정 규칙 기반 알고리즘입니다.
# 주요 오염원(분뇨, 하수, 산업폐수)을 우선 스크리닝하며, 결측 데이터에 대한 Fallback 로직을 
# 추가하여 '판정불가'를 최소화했습니다. (총 21편의 SCI/KCI 문헌 기반)
# * References
# 1. Kendall, C. (1998). Tracing nitrogen sources and cycling in catchments. In Isotope tracers in catchment hydrology (pp. 519-576). Elsevier.
# 2. Xue, D., Botte, J., De Baets, B., Accoe, F., Nestler, A., Taylor, P., ... & Boeckx, P. (2009). Present limitations and future prospects of stable isotope methods for nitrate source identification in surface-and groundwater. Water research, 43(5), 1159-1170.
# 3. Bottcher, J., Strebel, O., Voerkelius, S., & Schmidt, H. L. (1990). Using isotope fractionation of nitrate-nitrogen and nitrate-oxygen for evaluation of microbial denitrification in a sandy aquifer. Journal of hydrology, 114(3-4), 413-424.
# 4. Aravena, R., Evans, M. L., & Cherry, J. A. (1993). Stable isotopes of oxygen and nitrogen in source identification of nitrate from septic systems. Groundwater, 31(2), 180-186.
# 5. Mayer, B., Boyer, E. W., Goodale, C., Jaworski, N. A., Van Breemen, N., Howarth, R. W., ... & Paustian, K. (2002). Sources of nitrate in rivers draining sixteen watersheds in the northeastern US: Isotopic constraints. Biogeochemistry, 57(1), 171-197.
# 6. Elliott, E. M., Kendall, C., Wankel, S. D., Burns, D. A., Boyer, E. W., Harlin, K., ... & Butler, T. J. (2007). Nitrogen isotopes as indicators of NO x source contributions to atmospheric nitrate deposition across the midwestern and northeastern United States. Environmental Science & Technology, 41(22), 7661-7667.
# 7. Widory, D., Petelet-Giraud, E., Negrel, P., & Ladouche, B. (2005). Tracking the sources of nitrate in groundwater using coupled nitrogen and boron isotopes: a synthesis. Environmental Science & Technology, 39(2), 539-548.
# 8. Panno, S. V., Hackley, K. C., Hwang, H. H., Greenberg, S. E., Krapac, I. G., Landsberger, S., & O'kelly, D. J. (2006). Characterization and identification of Na???Cl sources in ground water. Groundwater, 44(2), 176-187.
# 9. Katz, B. G., Bohlke, J. K., & Hornsby, H. D. (2001). Timescales for nitrate contamination of spring waters, northern Florida, USA. Chemical Geology, 179(1-4), 167-186.
# 10. Xue, Y., Song, J., Zhang, Y., Kong, F., Wen, M., & Zhang, G. (2016). Nitrate pollution and preliminary source identification of surface water in a Semi-Arid River Basin, using isotopic and hydrochemical approaches. Water, 8(8), 328.
# 11. Gros, M., Petrovi??, M., & Barcelo, D. (2007). Wastewater treatment plants as a pathway for aquatic contamination by pharmaceuticals in the Ebro river basin (northeast Spain). Environmental toxicology and chemistry, 26(8), 1553-1562.
# 12. Minet, E., Coxon, C. E., Goodhue, R., Richards, K. G., Kalin, R. M., & Meier-Augenstein, W. (2012). Evaluating the utility of 15N and 18O isotope abundance analyses to identify nitrate sources: A soil zone study. Water research, 46(12), 3723-3736.
# 13. Koba, K., Fang, Y., Mo, J., Zhang, W., Lu, X., Liu, L., ... & Senoo, K. (2012). The 15N natural abundance of the N lost from an N???saturated subtropical forest in southern China. Journal of Geophysical Research: Biogeosciences, 117(G2).
# 14. 김형석, 김정인, 이선홍, 최재원, & 김윤석. (2019). 수질 중 질산성질소의 질소 (δ15N) 와 산소 (δ18O) 안정동위원소비를 이용한상수원수의 오염원 추적. 환경분석과 독성보건, 22(3), 145-153.
# 15. 정영철, 이정엽, 최재원, & 김윤석. (2017). 산소 (δ18O) 와 질소 (δ15N) 안정동위원소비를 이용한 상수원의 오염원 분석. 환경분석과 독성보건, 20-29.
# 16. 유지수, & 김윤석. (2021). 질소와 산소 안정동위원소비를 이용한 북한강수계 오염원 분포 특성 규명. 환경분석과 독성보건, 24(4), 164-170.
# 17. Koh, D., & Mayer, B. (2009, December). Source identification of nitrate in groundwater using stable isotopes and Cl/Br ratios in an agricultural area. In AGU Fall Meeting Abstracts (Vol. 2009, pp. H53D-0965).
# 18. 이인경, & 최상훈. (2010). 옥천지역 천부지하수의 지구화학적 특성 및 질산염 오염 특성. 자원환경지질, 43(1), 43-52.
# 19. Ryu, J. S., Lee, K. S., & Chang, H. W. (2007). Hydrogeochemical and isotopic investigations of the Han River basin, South Korea. Journal of hydrology, 345(1-2), 50-60.
# 20. Lee, K. S., Bong, Y. S., Lee, D., Kim, Y., & Kim, K. (2008). Tracing the sources of nitrate in the Han River watershed in Korea, using δ15N-NO3??? and δ18O-NO3??? values. Science of the Total Environment, 395(2-3), 117-124.
# 21. Ryu, H. S., Kang, T. W., Kim, K., Nam, T. H., Han, Y. U., Kim, J., ... & Lee, J. H. (2021). Tracking nitrate sources in agricultural-urban watershed using dual stable isotope and Bayesian mixing model approach: Considering N transformation by Lagrangian sampling. Journal of environmental management, 300, 113693.
# ------------------------------------------------------------------------------
assign_source_universal <- function(row) {
  n15 <- row$d15N; o18 <- row$d18O; cl <- row$Cl
  tp <- row$TP; nh3 <- row$NH3N; no3 <- row$NO3N
  toc <- row$TOC; chl <- row$Chla; ec <- row$EC
  
  # 안전한 NA 체크 함수
  safe_val <- function(val, default = -999) ifelse(is.na(val), default, val)
  
  # [Priority 0] 모든 주요 지표가 결측인 극단적 경우만 판정 불가 처리
  if (is.na(n15) && is.na(cl) && is.na(no3) && is.na(toc)) return("결측-판정불가")
  
  # [Priority 1] 외부 유입 및 기원 변형 (가장 먼저 필터링해야 할 신호)
  # [Ref 1: Kendall(1998), Ref 6: Elliott(2007), Ref 16: 유지수(2021)] 대기 강하물 및 초기 강우 유입
  if (safe_val(o18) > 15.0) return("대기유입/초기강우") 
  
  # [Ref 3: Bottcher(1990), Ref 2: Xue(2009), Ref 21: Ryu(2021)] 미생물 탈질작용에 의한 동위원소 농축 (1:1~1:2 비율)
  if (safe_val(n15) > 6.0 && safe_val(o18) > 3.0 && (safe_val(n15)/safe_val(o18) >= 1.0 && safe_val(n15)/safe_val(o18) <= 2.5)) {
    return("탈질작용(기원변형)") 
  }
  
  # [Priority 2] 고위험 인위적 점오염 (연구과제 최우선 타겟)
  # [Ref 9: Katz(2001), Ref 20: Lee(2008)] 산업폐수 및 도심 점오염: EC와 TOC가 비정상적으로 높음
  if (safe_val(n15) > 7.0 && (safe_val(ec) > 500 || safe_val(cl) > 100) && safe_val(toc) > 3.0) return("산업/인위적 점오염")
  
  # [Ref 8: Panno(2006), Ref 14: 김형석(2019), Ref 17: Koh(2009), Ref 18: 이인경(2010)] 축산분뇨: d15N 12~20‰, 고농도 Cl 및 NH3 동반
  if (safe_val(n15) > 12.0 && (safe_val(cl) > 60 || safe_val(nh3) > 0.3)) return("축산분뇨 직접유출") 
  
  # [Ref 4: Aravena(1993), Ref 11: Gros(2007), Ref 15: 정영철(2017)] 생활하수/정화조: d15N 7~12‰, 인위적 염소(Cl>40), 관리된 TP
  if (safe_val(n15) > 7.0 && safe_val(n15) <= 12.0 && (safe_val(cl) > 40 || safe_val(tp) <= 0.3)) return("생활하수/처리수 영향") 
  
  # [Priority 3] 수계 내부 생태적 부하 (Autochthonous & Internal)
  # [Ref 10: Xue(2016), Ref 20: Lee(2008)] 조류 증식: 광합성으로 인한 고농도 Chl-a 및 TOC 발생
  if (safe_val(chl) > 40.0 && safe_val(toc) > 4.5) return("조류증식(현장발생)")
  
  # [Ref 20: Lee(2008)] 내생부하: 하류 정체수역 퇴적물 용출 (고 TOC, TP)
  if (safe_val(toc) > 6.5 && safe_val(tp) > 0.1) return("내생부하(정체수역 유기물 축적)")
  
  # [Priority 4] 농업 및 토양 비점오염
  # [Ref 1: Kendall(1998), Ref 5: Mayer(2002), Ref 12: Minet(2012), Ref 19: Ryu(2007)] 화학비료: d15N < 4‰, 높은 질산성 질소
  if (safe_val(n15) < 4.0 && safe_val(no3) > 2.0 && safe_val(cl) < 30) return("화학비료 기반 농경지") 
  
  # [Ref 12: Minet(2012), Ref 13: Koba(2012), Ref 14: 김형석(2019)] 유기질 퇴비 및 토양유기물 (4~9‰ 범위)
  if (safe_val(n15) >= 4.0 && safe_val(n15) < 9.0) {
    if (safe_val(cl) > 35 && safe_val(toc) > 4.0) return("유기질비료(퇴비) 영향")
    if (safe_val(toc) < 4.0 && safe_val(cl) < 20) return("토양유기물/야생동물배설물")
  }
  
  # [Priority 5] Fallback: 판정 불가 방지를 위한 복합오염 및 수화학 단독 추정
  # [Ref 7: Widory(2005)] 다중 오염원 혼합 또는 동위원소 결측 시 수화학 지표 기반 우세 오염원 판정
  if (!is.na(n15) && n15 > 7.0) return("복합오염(하수/분뇨 기여 우세)")
  if (!is.na(n15) && n15 <= 7.0) return("복합오염(농경지 비점 우세)")
  
  # 동위원소(d15N)가 결측이더라도 수질 항목이 존재하면 Fallback 판별 수행
  if (safe_val(cl) > 50 || safe_val(tp) > 0.1) return("복합오염(하수/분뇨 수화학적 우세)")
  if (safe_val(no3) > 2.0) return("복합오염(농경지 비점 수화학적 우세)")
  
  return("자연배경/특이징후 없음")
}

# [보완] 개별 포인트 단위 오염원 판별 후, 앙상블 군집(Cluster)별 대표 Mode 선정
imputed_data_df <- as.data.frame(imputed_data)
imputed_data_df$Individual_Source <- apply(imputed_data_df, 1, function(r) assign_source_universal(as.list(r)))
imputed_data_df$Final_Cluster <- data$Final_Cluster

# 군집 내 최빈값 추출 함수
get_mode <- function(v) {
  v_clean <- v[v != "결측-판정불가"] # 최빈값 계산 시 결측 판정은 제외
  if(length(v_clean) == 0) return("판정불가")
  uniqv <- unique(v_clean)
  uniqv[which.max(tabulate(match(v_clean, uniqv)))]
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
umap_res <- umap(scaled_data, method="naive", n_neighbors = n_neighbors_val)
data$umap1 <- umap_res$layout[,1]; data$umap2 <- umap_res$layout[,2]
data$Individual_Source <- imputed_data_df$Individual_Source

# 최종 군집(Cluster) 기준 오염원 판별 결과 지도도
ggplot(data, aes(x = umap1, y = umap2, color = Source_Name, label = spot)) +
  geom_point(size = 5, alpha = 0.7) + geom_text_repel(family = "malgun") +
  labs(title = "UMAP 기반 오염원 분포 지도", subtitle = "안정동위원소-수질 융합 군집") +
  theme_minimal(base_family = "malgun")

# 개별 지점(Individual) 기준 오염원 판별 결과 지도도
ggplot(data, aes(x = umap1, y = umap2, color = Individual_Source, label = spot)) +
  geom_point(size = 5, alpha = 0.7) + geom_text_repel(family = "malgun") +
  labs(title = "UMAP 기반 오염원 분포 지도", subtitle = "안정동위원소-수질 융합 개별") +
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
rf_model <- randomForest(as.factor(data$Final_Cluster) ~ ., 
                         data = as.data.frame(imputed_data), 
                         importance = TRUE)
varImpPlot(rf_model, main = "오염원 군집화 결정 주도 변수 기여도", family = "malgun")

# ==============================================================================
# 5. 최종 레포트 출력
# ==============================================================================
cat("\n[최종 오염원 스크리닝 레포트]\n")
print(data %>% select(spot, Source_Name) %>% arrange(Source_Name))
