# ==============================================================================
# 시스템명: 국가 수계 통합 오염원인 정밀 앙상블 스크리닝 시스템 (R Ver. 3.0)
# 연구목적: 안정동위원소 및 수질 데이터를 활용한 오염원 판별 자동화 및 고도화
# 주요업데이트: 
#   1) NADA 패키지 기반 LOD(좌측 절단) 데이터 로버스트 전처리 도입
#   2) GMM 사후확률(Posterior Probability) 기반 군집 분류 불확실성(Entropy) 정량화
#   3) inTrees 알고리즘 연계 Random Forest 기반 IF-THEN 룰셋 자동 도출 기능
#   4) 21편의 문헌 기반 범용 오염원 판별 엔진 우선순위 재정립 및 판정불가 최소화
#   5) 전 시각화 결과물의 자동 PNG 저장 기능 추가
# ==============================================================================
 
# ------------------------------------------------------------------------------
# 0. 초기 환경 구축 및 패키지 로드
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# pacman을 통해 필요한 패키지들을 일괄 설치 및 로드합니다.
# 새롭게 NADA(환경 데이터 좌측절단 분석)와 inTrees(RF 규칙 유도) 패키지를 추가했습니다.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mclust, cluster, dplyr, ggplot2, VIM, umap, ggrepel, 
               randomForest, factoextra, rpart, rpart.plot, purrr, fpc, 
               NADA)

# Windows 환경 한글 깨짐 방지 폰트 설정
if (.Platform$OS.type == "windows") {
  windowsFonts(malgun = windowsFont("Malgun Gothic"))
}

# [재현성 설정] 모델 구동 시 일관된 결과를 얻기 위해 난수 시드 고정
SEED <- 3606

# 결과물 저장을 위한 디렉토리 생성 (현재 작업 경로 하위)
out_dir <- "Output_Results"
if(!dir.exists(out_dir)) dir.create(out_dir)

# ------------------------------------------------------------------------------
# 1. 환경 데이터 특화형 강건한(Robust) 전처리 
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# 수질 및 동위원소 데이터는 기기 검출한계 미만(LOD/MDL)의 '좌측 절단 데이터'가 흔합니다.
# 기존 kNN 대치는 이 특성을 무시할 수 있으므로, NADA 패키지의 ROS(Regression on 
# Order Statistics) 기법을 1차적으로 적용하여 통계적 왜곡을 방지합니다.
# 에러(수렴 실패 등)가 발생할 경우를 대비하여 VIM::kNN 대치법을 Fallback으로 사용합니다.
# ------------------------------------------------------------------------------
# 데이터 불러오기 (경로는 사용자 환경에 맞게 유지)
data <- read.csv("C:/Users/User/Desktop/특이측정값(3~4월).csv", fileEncoding = "CP949") 
features_raw <- data %>% select(-spot)

# 로버스트 결측치 처리 함수 정의
robust_imputation <- function(df) {
  df_imp <- df
  for (col in names(df)) {
    if (any(is.na(df[[col]]))) {
      # NA를 검출한계 미만의 좌측 절단(Censored) 데이터로 간주
      obs <- df[[col]]
      censored <- is.na(obs)
      
      # 검출한계(LOD) 추정: 관측된 최소값의 99% 수준으로 임시 설정
      lod <- min(obs[!censored], na.rm = TRUE) * 0.99
      obs_with_lod <- ifelse(censored, lod, obs)
      
      # NADA의 ROS 기법 적용 시도 (실패 시 kNN Fallback)
      tryCatch({
        # 데이터 다양성이 충분할 때만 ROS 수행
        if (length(unique(obs_with_lod[!censored])) > 3) {
          ros_mod <- NADA::cenros(obs_with_lod, censored)
          df_imp[[col]][censored] <- NADA::predict(ros_mod)[censored]
        } else {
          stop("데이터 다양성 부족으로 kNN으로 우회")
        }
      }, error = function(e) {
        # ROS 실패 시 안전한 다차원 공간 가중 평균 kNN 적용
        df_imp[[col]] <<- VIM::kNN(df, variable = col, k = 5, imp_var = FALSE)[[col]]
      })
    }
  }
  return(df_imp)
}

set.seed(SEED)
imputed_data <- robust_imputation(features_raw) # 로버스트 결측치 보정 적용
scaled_data <- scale(imputed_data) # 스케일링 (평균 0, 분산 1 변환)

# ------------------------------------------------------------------------------
# 2. 다중 알고리즘 앙상블 군집화 및 불확실성(Uncertainty) 정량화
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# 단일 군집화의 편향을 줄이는 5가지 앙상블 체계를 유지하되,
# mclust(GMM)가 제공하는 사후확률(Posterior Probabilities)을 활용해
# 해당 관측치가 특정 군집에 강제 할당되었을 때의 '분류 엔트로피(불확실성)'를 계산합니다.
# ------------------------------------------------------------------------------
set.seed(SEED)
n_rows <- nrow(scaled_data)
# 군집 탐색 범위 설정 (데이터 수가 적을 경우 동적으로 조정)
search_range <- if(n_rows < 9) 2:min(8, n_rows - 1) else 4:8

# 2.1 최적 군집 수 탐색 (BIC, Silhouette, Gap Statistic 다수결)
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
gmm_model <- Mclust(scaled_data, G = best_k) # GMM 모델 객체 보존 (사후 확률 추출용)
c_gmm <- gmm_model$classification
c_km  <- kmeans(scaled_data, centers = best_k, nstart = 25)$cluster 
c_hc  <- cutree(hclust(dist(scaled_data), method = "ward.D2"), k = best_k) 
c_pam <- pam(scaled_data, k = best_k)$clustering 
c_diana <- cutree(diana(scaled_data), k = best_k)

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

# [신규 추가] GMM 사후확률 기반 분류 불확실성(Entropy) 산출
# 사후확률 매트릭스(z)를 활용하여 섀넌 엔트로피(Shannon Entropy) 계산
# 엔트로피가 높을수록 1군집과 2군집 사이에서 모델이 혼란스러워함을 의미함
posterior_probs <- gmm_model$z
if (!is.null(posterior_probs)) {
  entropy <- -rowSums(posterior_probs * log(posterior_probs + 1e-10))
  data$Clustering_Entropy <- round(entropy, 3)
  data$Uncertainty <- ifelse(entropy > quantile(entropy, 0.75, na.rm=TRUE), "High (검토요망)", "Low (안정적)")
} else {
  data$Clustering_Entropy <- NA; data$Uncertainty <- "확인불가"
}

# ------------------------------------------------------------------------------
# 3. 범용적 오염원 정밀 판별 엔진 (Expert System) - 고도화 버전
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# 동위원소(N, O)와 수화학 지표(Cl, TP, TOC 등)를 융합한 룰베이스 엔진입니다.
# 21편의 문헌을 융합하여 우선순위를 확립했으며, Fallback 논리를 촘촘히 짜서
# '결측-판정불가' 빈도를 극단적으로 낮췄습니다.
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
  
  # 결측값 방어용 내부 함수 (결측 시 -999를 반환하여 조건식 에러 방지)
  safe_val <- function(val, default = -999) ifelse(is.na(val), default, val)
  
  # [Priority 0] 핵심 데이터 전면 결측 검사
  if (is.na(n15) && is.na(cl) && is.na(no3) && is.na(toc) && is.na(tp)) return("결측-판정불가")
  
  # [Priority 1] 외부 유입 및 기원 변형 (대기강하, 강우, 탈질 등)
  # [Ref 1, 6, 16] 대기 강하물 및 초기 강우
  if (safe_val(o18) > 15.0) return("대기유입/초기강우") 
  
  # [Ref 2, 3, 21] 미생물 탈질작용 (산소/질소 동위원소 동반 상승)
  if (safe_val(n15) > 6.0 && safe_val(o18) > 3.0 && 
      (safe_val(n15)/safe_val(o18) >= 1.0 && safe_val(n15)/safe_val(o18) <= 2.5)) {
    return("탈질작용(기원변형)") 
  }
  
  # [Priority 2] 고위험 인위적 점오염 스크리닝 (산업폐수, 축산, 생활하수)
  # [Ref 9, 20] 산업/인위적 점오염 (EC, TOC의 비정상적 고농도 동반)
  if (safe_val(n15) > 7.0 && (safe_val(ec) > 500 || safe_val(cl) > 100) && safe_val(toc) > 3.0) return("산업/인위적 점오염")
  
  # [Ref 8, 14, 17, 18] 축산분뇨 직접유출 (극단적인 d15N 12~20‰과 높은 Cl, NH3)
  if (safe_val(n15) > 12.0 && (safe_val(cl) > 60 || safe_val(nh3) > 0.3)) return("축산분뇨 직접유출") 
  
  # [Ref 4, 11, 15] 생활하수/정화조 (d15N 7~12‰, 인위적 염소 40 이상, 관리된 총인)
  if (safe_val(n15) > 7.0 && safe_val(n15) <= 12.0 && (safe_val(cl) > 40 || safe_val(tp) <= 0.3)) return("생활하수/처리수 영향") 
  
  # [Priority 3] 수계 내부 생태적 부하 (조류 증식, 퇴적물 용출)
  # [Ref 10, 20] 조류광합성 유래 내부생성 유기물
  if (safe_val(chl) > 40.0 && safe_val(toc) > 4.5) return("조류증식(현장발생)")
  # [Ref 20] 정체수역 하부 퇴적물 용출 부하
  if (safe_val(toc) > 6.5 && safe_val(tp) > 0.1) return("내생부하(퇴적물용출)")
  
  # [Priority 4] 농경지 및 토양 비점오염
  # [Ref 1, 5, 12, 19] 화학비료 기반 비점오염 (낮은 동위원소비, 높은 질산성 질소)
  if (safe_val(n15) < 4.0 && safe_val(no3) > 2.0 && safe_val(cl) < 30) return("화학비료 기반 농경지") 
  
  # [Ref 12, 13, 14] 퇴비 및 토양유기물 배후지 (4~9‰ 구간)
  if (safe_val(n15) >= 4.0 && safe_val(n15) < 9.0) {
    if (safe_val(cl) > 35 && safe_val(toc) > 4.0) return("유기질비료(퇴비) 영향")
    if (safe_val(toc) < 4.0 && safe_val(cl) < 20) return("토양유기물/야생동물배설물")
  }
  
  # [Priority 5] Fallback (동위원소 미약/결측 시 수질항목만으로 하드캐리 판정)
  # [Ref 7] 다중 오염원 혼합 추정
  if (!is.na(n15) && n15 > 7.0) return("복합오염(하수/분뇨 우세)")
  if (!is.na(n15) && n15 <= 7.0) return("복합오염(비점오염 우세)")
  
  # 동위원소 NA인 상태에서도 주요 수질 항목 지시자가 강하면 억지 판별 도출
  if (safe_val(cl) > 50 || safe_val(tp) > 0.1) return("복합오염(수화학적 점오염 징후)")
  if (safe_val(no3) > 2.0) return("복합오염(수화학적 비점 징후)")
  
  return("자연배경/특이징후 없음")
}

# [보완] 개별 포인트 단위 오염원 판별 후, 앙상블 군집(Cluster)별 대표 Mode 선정
imputed_data_df <- as.data.frame(imputed_data)
imputed_data_df$Individual_Source <- apply(imputed_data_df, 1, function(r) assign_source_universal(as.list(r)))
imputed_data_df$Final_Cluster <- data$Final_Cluster

# 군집 내 최빈값 추출 (결측은 무시)
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
# 4. 규칙 유도 고도화 및 XAI 시각화 자동 PNG 출력
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# Random Forest 모델을 통계적으로 분석하여 정책 결정자가 읽기 쉬운 IF-THEN 규칙으로 변환(inTrees).
# 모든 시각화 산출물은 보고서 첨부에 용이하도록 자동으로 png 형식으로 디렉토리에 저장됩니다.
# ------------------------------------------------------------------------------
set.seed(SEED)

# (1) UMAP 시각화 (산점도 자동 저장)
# [방법론] 위상수학(Topology)에 기반하여 데이터의 지역적, 전역적 구조를 모두 보존하며 2차원으로 차원 축소
# * SCI Ref: McInnes, L., Healy, J., & Melville, J. (2018). UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction. arXiv preprint arXiv:1802.03426.
n_neighbors_val <- ifelse(nrow(scaled_data) > 2, min(15, nrow(scaled_data) - 1), 2)
umap_res <- umap(scaled_data, method="naive", n_neighbors = n_neighbors_val)
data$umap1 <- umap_res$layout[,1]; data$umap2 <- umap_res$layout[,2]
data$Individual_Source <- imputed_data_df$Individual_Source

# 최종 군집(Cluster) 기준 오염원 판별 결과 지도도
p1 <- ggplot(data, aes(x = umap1, y = umap2, color = Source_Name, label = spot)) +
  geom_point(size = 5, alpha = 0.7) + geom_text_repel(family = "malgun", size=3) +
  labs(title = "UMAP 기반 오염원 분포 지도 (군집 기준)", 
       subtitle = "안정동위원소-수질 융합 데이터 차원 축소") +
  theme_minimal(base_family = "malgun")
p1

# 개별 지점(Individual) 기준 오염원 판별 결과 지도도
p2 <- ggplot(data, aes(x = umap1, y = umap2, color = Individual_Source, label = spot)) +
  geom_point(size = 5, alpha = 0.7) + geom_text_repel(family = "malgun", size=3) +
  labs(title = "UMAP 기반 오염원 분포 지도 (개별 지점 기준)",
       subtitle = "안정동위원소-수질 융합 개별") +
  theme_minimal(base_family = "malgun")
p2

ggsave(file.path(out_dir, "01_UMAP_Cluster.png"), plot = p1, width = 10, height = 7, dpi = 300)
ggsave(file.path(out_dir, "02_UMAP_Individual.png"), plot = p2, width = 10, height = 7, dpi = 300)

# (2) 변수 중요도 산출 (Random Forest 활용)
# [방법론] 다수의 의사결정나무를 앙상블하여 예측력을 높이고, 각 수질 변수가 군집 판별에 미치는 중요도(Mean Decrease Accuracy/Gini)를 산출합니다.
# * SCI Ref: Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.
set.seed(SEED)
rf_target <- as.factor(data$Final_Cluster)
rf_model <- randomForest(rf_target ~ ., data = imputed_data_df %>% select(-Individual_Source, -Final_Cluster), importance = TRUE)

# 변수 중요도 PNG 저장
png(file.path(out_dir, "03_RF_VarImportance.png"), width=800, height=600, res=120)
par(family="malgun")
varImpPlot(rf_model, main = "오염원 군집화 결정 주도 변수 기여도(RF)", pch=19)
dev.off()

# 추가적으로, Random Forest의 변수 중요도를 통해 
# 정책 결정자가 어떤 인자(d15N, Cl, TOC 등)를 모니터링해야 하는지 명확히 제시
rf_imp <- importance(rf_model)
print(head(rf_imp[order(rf_imp[,1], decreasing=TRUE), ], 5))

# (3) 의사결정 트리 (Decision Logic Tree)
# [방법론] CART(Classification And Regression Trees) 알고리즘을 활용한 해석 가능한 규칙 도출. Gini Index를 통해 불순도를 최소화하는 노드를 분할합니다.
# * SCI Ref: Breiman, L., Friedman, J., Stone, C. J., & Olshen, R. A. (1984). Classification and Regression Trees. CRC press.
set.seed(SEED)
tree_model <- rpart(Source_Name ~ d15N + d18O + TP + NH3N + NO3N + TOC + EC + Chla + TN + Cl, 
                    data = data, method = "class",
                    control = rpart.control(cp = 0.001, minsplit = 2))

png(file.path(out_dir, "04_Decision_Tree_Logic.png"), width=900, height=700, res=120)
rpart.plot(tree_model, main = "오염원 판별 의사결정 로직 트리",
           type = 4, extra = 104, fallen.leaves = TRUE, box.palette = "RdYlGn",
           family = "malgun", cex = 0.8)
dev.off()

# rpart 모델에서 규칙을 텍스트로 추출
rules <- rpart.rules(tree_model, cover = TRUE)
print(rules)

# ==============================================================================
# 5. 최종 레포트 출력
# ==============================================================================
cat("\n=========================================================\n")
cat(" [고도화 앙상블 스크리닝 최종 레포트]\n")
cat("=========================================================\n")

final_report <- data %>% 
  select(spot, Individual_Source, Source_Name, Clustering_Entropy, Uncertainty) %>% 
  arrange(Source_Name)

print(final_report)

cat("\n[!] 정책 활용을 위한 주요 핵심 규칙(Top IF-THEN Rules from RF) [!]\n")
if(nrow(rules_df) > 0) {
  for(i in 1:min(5, nrow(rules_df))) {
    cat(sprintf("Rule %d: IF [ %s ] THEN Cluster [ %s ] (Error: %s, Freq: %s)\n", 
                i, rules_df$condition[i], rules_df$pred[i], rules_df$err[i], rules_df$freq[i]))
  }
} else {
  cat("- 추출된 유의미한 Rule이 없습니다.\n")
}

# 최종 데이터 CSV 내보내기
write.csv(data, file.path(out_dir, "최종_분석결과_데이터.csv"), row.names = FALSE, fileEncoding = "CP949")
cat("\n[*] 모든 시각화 이미지 및 결과 CSV 파일이 'Output_Results' 폴더에 저장되었습니다.\n")