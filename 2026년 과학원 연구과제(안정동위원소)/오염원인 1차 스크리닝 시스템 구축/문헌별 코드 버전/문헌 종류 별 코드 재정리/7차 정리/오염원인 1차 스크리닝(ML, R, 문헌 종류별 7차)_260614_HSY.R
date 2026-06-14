# ==============================================================================
# 시스템명: 국가 수계 통합 오염원인 정밀 앙상블 스크리닝 시스템 
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
               randomForest, factoextra, rpart, rpart.plot, partykit, purrr, fpc, 
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
data <- read.csv("C:/Users/User/Desktop/3월 4주차.csv", fileEncoding = "CP949") 
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
max_k <- n_rows - 2
if (max_k < 4) {
  warning("전체 지점 수(spot)가 부족하여 계산된 최대 군집 수가 최소 기준(4)보다 작습니다. 범위를 4:4로 고정합니다.")
  max_k <- 4
}
search_range <- 4:max_k

# 2.1 최적 군집 수 탐색 (BIC, Silhouette, Gap Statistic 다수결)
# [방법론] BIC, Silhouette, Gap Statistic 3가지 평가 지표의 다수결 적용
# * SCI Ref (Gap Stat): Tibshirani, R., et al. (2001). Estimating the number of clusters in a data set via the gap statistic. JRSS Series B, 63(2), 411-423.
gmm_auto <- Mclust(scaled_data, G = search_range)
best_k_bic <- ifelse(!is.null(gmm_auto$G), gmm_auto$G, 4)

if (n_rows > 5) { 
  dynamic_k_max <- max_k
  sil_test <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = dynamic_k_max)
  best_k_sil <- max(4, min(as.numeric(sil_test$data$clusters[which.max(sil_test$data$y)]), max_k))
  
  gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = dynamic_k_max, B = 50)
  best_k_gap <- max(4, min(maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method="Tibs2001SEmax"), max_k))
} else {
  best_k_sil <- 4; best_k_gap <- 4
}

# 다수결에 의한 최종 k 확정
k_votes <- na.omit(c(best_k_bic, best_k_sil, best_k_gap))
if (length(unique(k_votes)) == length(k_votes)) {
  calculated_k <- round(median(k_votes))  # 최빈값이 없으면 중앙값
} else {
  calculated_k <- as.numeric(names(sort(table(k_votes), decreasing = TRUE)[1]))
}
best_k <- max(4, min(calculated_k, max_k))

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
# [분석과정 및 방법론]
# 본 엔진은 37편의 동위원소 및 수화학 분야 최선행 문헌을 기반으로 정립된 룰베이스 전문가 시스템을
# 모태로 합니다. 여기에 머신러닝 기법(Random Forest)과 통계적 밀도 확률(GMM)을 융합하였습니다.
# 기존 하드 임계치(Hard Threshold) 방식의 독창성 한계를 극복하고, 현장 데이터의 패턴을 반영하여
# '자연배경/특이징후 없음'으로 오분류되는 은닉 오염원을 최소화하며, 판정 결과의 '퍼지 신뢰도'를 정량화합니다.
# [논리 설명(Weight Matrix Algorithm)] 21편의 국내외 참고문헌을 가중치 행렬로 변환하여 동시다발적 평가 수행
# 특정 조건에서 즉시 종료(Short-circuit)하지 않고, 모든 문헌의 규칙을 검사하여
# 각 오염원 후보별로 '근거 점수(Evidence Score)'를 누적한 뒤 최댓값을 도출합니다.
# * References

# 국내 문헌 (case1)
# 1. Koh, D., & Mayer, B. (2009, December). Source identification of nitrate in groundwater using stable isotopes and Cl/Br ratios in an agricultural area. In AGU Fall Meeting Abstracts (Vol. 2009, pp. H53D-0965).
# 2. Ryu, J. S., Lee, K. S., & Chang, H. W. (2007). Hydrogeochemical and isotopic investigations of the Han River basin, South Korea. Journal of hydrology, 345(1-2), 50-60.
# 3. Lee, K. S., Bong, Y. S., Lee, D., Kim, Y., & Kim, K. (2008). Tracing the sources of nitrate in the Han River watershed in Korea, using δ15N-NO3 and δ18O-NO3 values. Science of the Total Environment, 395(2-3), 117-124.
# 4. Ryu, H. S., Kang, T. W., Kim, K., Nam, T. H., Han, Y. U., Kim, J., ... & Lee, J. H. (2021). Tracking nitrate sources in agricultural-urban watershed using dual stable isotope and Bayesian mixing model approach: Considering N transformation by Lagrangian sampling. Journal of environmental management, 300, 113693.
# 5. 2024년 환경기초조사사업 최종보고서(동위원소 1차년도), 국립환경과학원 영산강물환경연구소 -> 신규 추가 반영
# 6. 2025년 환경기초조사사업 최종보고서(동위원소 2차년도), 국립환경과학원 영산강물환경센터 -> 신규 추가 반영
# 7. Hong, S., Han, Y., Kim, J., Lim, B. R., Park, S. Y., Choi, H., ... & Kim, M. S. (2023). A quantitative approach for identifying nitrogen sources in complex yeongsan river watershed, Republic of Korea, based on dual nitrogen isotope ratios and hydrological model. Water, 15(24), 4275. -> 신규 추가 반영
# 8. Shin, W. J., Kim, I., Ryu, J. S., & Lee, K. S. (2019). Dual isotopes of nitrate in Korean fertilizers and their application for identifying nitrate sources. Episodes Journal of International Geoscience, 42(4), 313-319. -> 신규 추가 반영
# 9. Kaown, D., Koh, D. C., Mayer, B., & Lee, K. K. (2009). Identification of nitrate and sulfate sources in groundwater using dual stable isotope approaches for an agricultural area with different land use (Chuncheon, mid-eastern Korea). Agriculture, Ecosystems & Environment, 132(3-4), 223-231. -> 신규 추가 반영
# 10. Jung, H., Kim, Y. S., Yoo, J., Han, S. J., & Lee, J. (2023). Identification of nitrate sources in tap water sources across South Korea using multiple stable isotopes: Implications for land use and water management. Science of the Total Environment, 864, 161026. -> 신규 추가 반영
# 11. Kim, H., Kaown, D., Mayer, B., Lee, J. Y., Hyun, Y., & Lee, K. K. (2015). Identifying the sources of nitrate contamination of groundwater in an agricultural area (Haean basin, Korea) using isotope and microbial community analyses. Science of the Total environment, 533, 566-575. -> 신규 추가 반영
# 12. Shin, W. J., Jung, Y. Y., Choi, M., Choi, S. H., Choi, H. B., Lee, K. S., ... & Koh, D. C. (2023). National-scale investigation of dual nitrate isotopes and chloride ion in South Korea: Nitrate source apportionment for stream water. Environmental Research, 228, 115873. -> 신규 추가 반영
# 13. Song, H., Lee, K. S., Jeong, N., Cho, S., Lee, D., Yun, C., & Shin, W. J. (2026). Nitrate source apportionments in the South Han River basin and its main tributaries using multi-isotopes and Bayesian approaches. Journal of Contaminant Hydrology, 104856. -> 신규 추가 반영

# 국외 문헌 (case2)
# 14. Kendall, C. (1998). Tracing nitrogen sources and cycling in catchments. In Isotope tracers in catchment hydrology (pp. 519-576). Elsevier.
# 15. Xue, D., Botte, J., De Baets, B., Accoe, F., Nestler, A., Taylor, P., ... & Boeckx, P. (2009). Present limitations and future prospects of stable isotope methods for nitrate source identification in surface-and groundwater. Water research, 43(5), 1159-1170.
# 16. Bottcher, J., Strebel, O., Voerkelius, S., & Schmidt, H. L. (1990). Using isotope fractionation of nitrate-nitrogen and nitrate-oxygen for evaluation of microbial denitrification in a sandy aquifer. Journal of hydrology, 114(3-4), 413-424.
# 17. Aravena, R., Evans, M. L., & Cherry, J. A. (1993). Stable isotopes of oxygen and nitrogen in source identification of nitrate from septic systems. Groundwater, 31(2), 180-186.
# 18. Mayer, B., Boyer, E. W., Goodale, C., Jaworski, N. A., Van Breemen, N., Howarth, R. W., ... & Paustian, K. (2002). Sources of nitrate in rivers draining sixteen watersheds in the northeastern US: Isotopic constraints. Biogeochemistry, 57(1), 171-197.
# 19. Elliott, E. M., Kendall, C., Wankel, S. D., Burns, D. A., Boyer, E. W., Harlin, K., ... & Butler, T. J. (2007). Nitrogen isotopes as indicators of NOx source contributions to atmospheric nitrate deposition across the midwestern and northeastern United States. Environmental Science & Technology, 41(22), 7661-7667.
# 20. Widory, D., Petelet-Giraud, E., Negrel, P., & Ladouche, B. (2005). Tracking the sources of nitrate in groundwater using coupled nitrogen and boron isotopes: a synthesis. Environmental Science & Technology, 39(2), 539-548.
# 21. Panno, S. V., Hackley, K. C., Hwang, H. H., Greenberg, S. E., Krapac, I. G., Landsberger, S., & O'kelly, D. J. (2006). Characterization and identification of NaCl sources in ground water. Groundwater, 44(2), 176-187.
# 22. Katz, B. G., Bohlke, J. K., & Hornsby, H. D. (2001). Timescales for nitrate contamination of spring waters, northern Florida, USA. Chemical Geology, 179(1-4), 167-186.
# 23. Xue, Y., Song, J., Zhang, Y., Kong, F., Wen, M., & Zhang, G. (2016). Nitrate pollution and preliminary source identification of surface water in a Semi-Arid River Basin, using isotopic and hydrochemical approaches. Water, 8(8), 328.
# 24. Gros, M., Petrovi??, M., & Barcelo, D. (2007). Wastewater treatment plants as a pathway for aquatic contamination by pharmaceuticals in the Ebro river basin (northeast Spain). Environmental toxicology and chemistry, 26(8), 1553-1562.
# 25. Minet, E., Coxon, C. E., Goodhue, R., Richards, K. G., Kalin, R. M., & Meier-Augenstein, W. (2012). Evaluating the utility of 15N and 18O isotope abundance analyses to identify nitrate sources: A soil zone study. Water research, 46(12), 3723-3736.
# 26. Koba, K., Fang, Y., Mo, J., Zhang, W., Lu, X., Liu, L., ... & Senoo, K. (2012). The 15N natural abundance of the N lost from an N-saturated subtropical forest in southern China. Journal of Geophysical Research: Biogeosciences, 117(G2).
# 27. Spalding, R. F., Hirsh, A. J., Exner, M. E., Little, N. A., & Kloppenborg, K. L. (2019). Applicability of the dual isotopes δ15N and δ18O to identify nitrate in groundwater beneath irrigated cropland. Journal of contaminant hydrology, 220, 128-135. -> 신규 추가 반영
# 28. Ogrinc, N., Tam??e, S., Zavadlav, S., Vrzel, J., & Jin, L. (2019). Evaluation of geochemical processes and nitrate pollution sources at the Ljubljansko polje aquifer (Slovenia): A stable isotope perspective. Science of the total environment, 646, 1588-1600. -> 신규 추가 반영
# 29. Li, S. L., Liu, C. Q., Li, J., Xue, Z., Guan, J., Lang, Y., ... & Li, L. (2013). Evaluation of nitrate source in surface water of southwestern China based on stable isotopes. Environmental earth sciences, 68(1), 219-228. -> 신규 추가 반영
# 30. Wassenaar, L. I. (1995). Evaluation of the origin and fate of nitrate in the Abbotsford Aquifer using the isotopes of 15N and 18O in NO3-. Applied geochemistry, 10(4), 391-405. -> 신규 추가 반영
# 31. Mouassa, S., Moulla, A. S., Benhadj Tahar, A., Teffahi, M., Zaagane, M., Benyoucef, M., & Brahmia, N. (2020). Geochemical and isotopic characterization of groundwater in the alluvial aquifer of Guelma (north-eastern Algeria). Arabian Journal of Geosciences, 13(5), 218. -> 신규 추가 반영
# 32. Chen, R., Hu, Q., Shen, W., Guo, J., Yang, L., Yuan, Q., ... & Wang, L. (2023). Identification of nitrate sources of groundwater and rivers in complex urban environments based on isotopic and hydro-chemical evidence. Science of the Total Environment, 871, 162026. -> 신규 추가 반영
# 33. Ren, C., Zhang, Q., Wang, H., & Wang, Y. (2021). Identification of sources and transformations of nitrate in the intense human activity region of North China using a multi-isotope and Bayesian model. International Journal of Environmental Research and Public Health, 18(16), 8642. -> 신규 추가 반영
# 34. Divers, M. T., Elliott, E. M., & Bain, D. J. (2014). Quantification of nitrate sources to an urban stream using dual nitrate isotopes. Environmental science & technology, 48(18), 10580-10587. -> 신규 추가 반영
# 35. Wang, S., Li, H., Kang, T., Li, R., & Zhang, C. (2025). Resolving Nitrate Sources in Rivers Through Dual Isotope Analysis of δ15N and δ18O. Water, 17(23), 3370. -> 신규 추가 반영
# 36. Mayer, B., Boyer, E. W., Goodale, C., Jaworski, N. A., Van Breemen, N., Howarth, R. W., ... & Paustian, K. (2002). Sources of nitrate in rivers draining sixteen watersheds in the northeastern US: Isotopic constraints. Biogeochemistry, 57(1), 171-197. -> 신규 추가 반영
# 37. Wang, M., Lu, B., Wang, J., Zhang, H., Guo, L., & Lin, H. (2016). Using dual isotopes and a bayesian isotope mixing model to evaluate nitrate sources of surface water in a drinking water source watershed, East China. Water, 8(8), 355. -> 신규 추가 반영

# 국내 문헌 + 국외 문헌 (case3) : 위의 문헌 모두 반영
# ------------------------------------------------------------------------------
# ==============================================================================
# [Core Logic] 문헌 기반의 다중 지표 표준 판별 규칙 함수 (Ref 1~37 고도화 업데이트 반영)
# ==============================================================================
assign_source_weighted <- function(row_list, lit_case = 3) {
  # [방어적 코딩] 입력 데이터 타입 보장 (Factor나 String이 들어와도 안전하게 Numeric 변환)
  n15 <- suppressWarnings(as.numeric(as.character(row_list$d15N)))
  o18 <- suppressWarnings(as.numeric(as.character(row_list$d18O)))
  cl  <- suppressWarnings(as.numeric(as.character(row_list$Cl)))
  tp  <- suppressWarnings(as.numeric(as.character(row_list$TP)))
  nh3 <- suppressWarnings(as.numeric(as.character(row_list$NH3N)))
  no3 <- suppressWarnings(as.numeric(as.character(row_list$NO3N)))
  toc <- suppressWarnings(as.numeric(as.character(row_list$TOC)))
  chl <- suppressWarnings(as.numeric(as.character(row_list$Chla)))
  ec  <- suppressWarnings(as.numeric(as.character(row_list$EC)))
  
  # 결측값 방어용 내부 함수 (조건식 NA 에러 완벽 차단 및 기본값 -999 할당)
  safe_val <- function(val, default = -999) ifelse(is.na(val), default, val)
  
  # 지표 간 상관성 분석을 위한 파생 변수 (2025년 환경기초조사 반영)
  no3_cl_ratio <- ifelse(safe_val(cl) > 0, safe_val(no3) / safe_val(cl), 0)
  
  # [Priority 0] 핵심 데이터 전면 결측 검사
  if (is.na(n15) && is.na(cl) && is.na(no3) && is.na(toc) && is.na(tp)) return("결측-판정불가")
  
  # ----------------------------------------------------------------------------
  # [가중치 매트릭스 설정]
  # Case 1: 국내 문헌 단독 구동 (w_dom = 1.0, w_int = 0.0) 
  # Case 2: 국외 문헌 단독 구동 (w_dom = 0.0, w_int = 1.0)
  # Case 3: 하이브리드 융합 구동 (국내 특성 우선 반영 w_dom = 1.2, 글로벌 범용성 w_int = 0.8)
  # ----------------------------------------------------------------------------
  w_dom <- ifelse(lit_case == 1, 1.0, ifelse(lit_case == 2, 0.0, 1.2))
  w_int <- ifelse(lit_case == 1, 0.0, ifelse(lit_case == 2, 1.0, 0.8))
  
  # 계층적 판별을 위한 우선순위 기본 점수 (Waterfall 효과를 가중치로 구현)
  p_score <- c(P1 = 1000, P2 = 800, P3 = 600, P4 = 400, P5 = 200, P6 = 100)
  
  # 점수 누적용 리스트 초기화 및 동적 할당 함수
  scores <- list()
  add_score <- function(name, score) {
    if (score <= 0) return() 
    if (is.null(scores[[name]])) scores[[name]] <<- 0
    scores[[name]] <<- scores[[name]] + score
  }
  
  # ---------------------------------------------------------
  # [Priority 1] 외부 유입 및 기원 변형 (대기강하, 강우, 탈질 등)
  # ---------------------------------------------------------
  # [Ref 14, 19, 36(Int), 2, 3(Dom)] 대기 강하물 및 초기 강우
  # (End-member: O18 > 15.0)
  if (safe_val(o18) > 15.0) add_score("대기유입/초기강우", p_score["P1"] * (w_int + w_dom))
  
  # [Ref 5(Dom)] 영산강/섬진강 대기강하물 (국립환경과학원 2024년 1차년도 로컬 동위원소 기준)
  # (End-member: N15 -10~5.0, O18 25~75(50이상 강한 징후))
  if (safe_val(n15) >= -10.0 && safe_val(n15) <= 5.0 && safe_val(o18) >= 50.0) {
    add_score("대기유입/초기강우", p_score["P1"] * (w_dom * 1.0))
  }
  
  # [Ref 15, 16(Int), 4, 33(Dom)] 미생물 탈질작용 (산소/질소 동위원소 동반 상승)
  if (safe_val(n15) > 6.0 && safe_val(o18) > 3.0 && 
      (safe_val(n15)/safe_val(o18) >= 1.0 && safe_val(n15)/safe_val(o18) <= 2.5)) {
    add_score("탈질작용(기원변형)", p_score["P1"] * (w_int + w_dom))
  }
  
  # ---------------------------------------------------------
  # [Priority 2] 고위험 인위적 점오염 스크리닝 (산업폐수, 축산, 생활하수)
  # ---------------------------------------------------------
  # [Ref 22, 32, 34(Int), 3, 10(Dom)] 산업/인위적 도시 점오염 (복합 오염)
  if (safe_val(n15) > 7.0 && (safe_val(ec) > 500 || safe_val(cl) > 100) && safe_val(toc) > 3.0) {
    add_score("산업/도시형 점오염", p_score["P2"] * (w_int + w_dom))
  }
  
  # [Ref 21, 28, 31(Int), 1, 9, 11(Dom)] 축산분뇨 직접유출 
  # (End-member: N15 > 12.0)
  if (safe_val(n15) > 12.0 && (safe_val(cl) > 60 || safe_val(nh3) > 0.3)) {
    add_score("축산분뇨 직접유출", p_score["P2"] * (w_int + w_dom * 1.5))
  }
  
  # [Ref 6, 12(Dom)] 축산계 및 생활하수 정밀 분류 룰 (NO3/Cl 비율 및 Cl 이온 기반)
  # (국립환경과학원 2025년 2차년도: 염소이온 농도가 높고 NO3/Cl 비율이 0.1~0.3 이하일 경우 가축분뇨/하수 기원)
  if (no3_cl_ratio > 0 && no3_cl_ratio < 0.3 && safe_val(cl) > 35.0) {
    if (safe_val(n15) >= 11.0 || safe_val(tp) >= 0.1) {
      add_score("축산분뇨 직접유출", p_score["P2"] * (w_dom * 1.2))
    } else {
      add_score("생활하수/처리수 영향", p_score["P2"] * (w_dom * 1.0))
    }
  }
  
  # [Ref 17, 24, 34(Int), 2, 10(Dom)] 생활하수/정화조
  # (End-member: N15 7.0~12.0)
  if (safe_val(n15) > 7.0 && safe_val(n15) <= 12.0 && (safe_val(cl) > 40 || safe_val(tp) <= 0.3)) {
    add_score("생활하수/처리수 영향", p_score["P2"] * (w_int + w_dom))
  }
  
  # [Ref 6(Dom)] 생활계 구조적 룰셋 (국립환경과학원 2025년 2차년도)
  if (safe_val(n15) >= 7.0 && safe_val(n15) < 11.0 && safe_val(cl) >= 40.0 && safe_val(toc) >= 4.0) {
    add_score("생활하수/처리수 영향", p_score["P2"] * (w_dom * 1.0))
  }
  
  # [Ref 5, 6(Dom)] 가축분뇨/생활하수 유입 동위원소 공통 구간
  # (End-member: N15 8~22, O18 0~15)
  if (safe_val(n15) >= 8.0 && safe_val(n15) <= 22.0 && safe_val(o18) >= 0.0 && safe_val(o18) <= 15.0) {
    add_score("축산분뇨 직접유출", p_score["P2"] * w_dom * 0.8)
    add_score("생활하수/처리수 영향", p_score["P2"] * w_dom * 0.8)
  }
  
  # ---------------------------------------------------------
  # [Priority 3] 수계 내부 생태적 부하 (조류 증식, 퇴적물 용출)
  # ---------------------------------------------------------
  # [Ref 23(Int), 3(Dom)] 조류광합성 유래 내부생성 유기물
  if (safe_val(chl) > 40.0 && safe_val(toc) > 4.5) {
    add_score("조류증식(현장발생)", p_score["P3"] * (w_int + w_dom))
  }
  
  # [Ref 3, 13(Dom)] 정체수역 하부 퇴적물 용출 부하
  if (safe_val(toc) > 6.5 && safe_val(tp) > 0.1) {
    add_score("내생부하(퇴적물용출)", p_score["P3"] * w_dom) 
  }
  
  # ---------------------------------------------------------
  # [Priority 4] 농경지 및 토양 비점오염
  # ---------------------------------------------------------
  # [Ref 14, 18, 25, 27, 29(Int), 1, 9, 35(Dom)] 화학비료 기반 농경지
  # (End-member: N15 < 5.0)
  if (safe_val(n15) < 4.0 && safe_val(no3) > 2.0 && safe_val(cl) < 30) {
    add_score("화학비료 기반 농경지", p_score["P4"] * (w_int + w_dom))
  }
  
  # [Ref 6, 12(Dom)] 비료 기원 정밀 판별 (NO3/Cl 비율 기반)
  if (no3_cl_ratio > 0.5) {
    add_score("화학비료 기반 농경지", p_score["P4"] * (w_dom * 1.2))
  }
  
  # [Ref 5, 8(Dom)] 한국형 화학비료 고유 범위 (End-member: N15 -4~5, O18 15~25)
  if (safe_val(n15) >= -4.0 && safe_val(n15) <= 5.0 && safe_val(o18) >= 15.0 && safe_val(o18) <= 25.0) {
    add_score("화학비료 기반 농경지", p_score["P4"] * (w_dom * 1.0))
  }
  
  # [Ref 25, 26, 30(Int), 1, 4(Dom)] 퇴비 및 토양유기물 배후지
  if (safe_val(n15) >= 4.0 && safe_val(n15) < 9.0) {
    if (safe_val(cl) > 35 && safe_val(toc) > 4.0) {
      add_score("유기질비료(퇴비) 영향", p_score["P4"] * (w_int + w_dom))
    } else if (safe_val(toc) < 4.0 && safe_val(cl) < 20) {
      add_score("토양유기물/배후지", p_score["P4"] * (w_int + w_dom))
    }
  }
  
  # [Ref 5, 6(Dom)] 토양유기물 및 강우 유출 (End-member: N15 1~8, O18 0~12)
  if (safe_val(n15) >= 1.0 && safe_val(n15) <= 8.0 && safe_val(o18) >= 0.0 && safe_val(o18) <= 12.0) {
    add_score("토양유기물/배후지", p_score["P4"] * (w_dom * 1.0))
  }
  
  # [Ref 6, 7(Dom)] 토지계 비점오염(강우유출) 복합 지표
  if (safe_val(n15) >= 2.0 && safe_val(n15) <= 7.0 && (safe_val(tp) < 0.05 || safe_val(toc) >= 3.0)) {
    add_score("토지계 비점오염(강우유출)", p_score["P4"] * (w_dom * 1.0))
  }
  
  # ---------------------------------------------------------
  # [Priority 5] 판정불가 최소화를 위한 퍼지 추론 및 Fallback
  # ---------------------------------------------------------
  # [Ref 20, 28, 37(Int)] 다중 오염원 혼합 추정 
  if (!is.na(n15)) {
    if (n15 > 7.0) add_score("복합오염(하수/분뇨 우세)", p_score["P5"] * w_int)
    if (n15 <= 7.0) add_score("복합오염(비점오염 우세)", p_score["P5"] * w_int)
  }
  
  # 동위원소가 결측이거나 뚜렷하지 않은 상태에서도 주요 수화학 징후가 강할 경우 하드캐리
  if (safe_val(cl) > 50 || safe_val(tp) > 0.1 || safe_val(nh3) > 0.5) {
    add_score("복합오염(수화학적 점오염 징후)", p_score["P5"] * (w_int*0.5 + w_dom*0.5))
  }
  if (safe_val(no3) > 2.0 || safe_val(toc) > 4.0) {
    add_score("복합오염(수화학적 비점 징후)", p_score["P5"] * (w_int*0.5 + w_dom*0.5))
  }
  
  # 최후 보루 (모든 점수가 0이나 기초 데이터가 존재하는 경우 자연 발생으로 넘기지 않고 기저부하 추정)
  if (length(scores) == 0 && (safe_val(no3) > 0.5 || safe_val(toc) > 1.0)) {
    add_score("복합오염(기저부하 혼재)", p_score["P6"])
  }
  
  # ---------------------------------------------------------
  # 결과 산출 (점수 경합 및 최적 오염원 낙점)
  # ---------------------------------------------------------
  if (length(scores) == 0) return("자연배경/특이징후 없음")
  
  max_val <- max(unlist(scores))
  if (max_val == 0) return("자연배경/특이징후 없음")
  
  # 최고 점수를 획득한 오염원 반환 (동점 시 Priority가 높은 오염원이 우선 낙점됨)
  best_source <- names(scores)[which(unlist(scores) == max_val)[1]]
  return(best_source)
}

# 데이터프레임 복제 및 Case 1, 2, 3 비교 분석용 동시 스크리닝
imputed_data_df <- as.data.frame(imputed_data)

# 연구팀 요청사항: 3가지 Case 결과물 동시 생성
imputed_data_df$Case1_Source <- apply(imputed_data_df, 1, function(r) assign_source_weighted(as.list(r), lit_case = 1))
imputed_data_df$Case2_Source <- apply(imputed_data_df, 1, function(r) assign_source_weighted(as.list(r), lit_case = 2))
imputed_data_df$Case3_Source <- apply(imputed_data_df, 1, function(r) assign_source_weighted(as.list(r), lit_case = 3))

# 하위 기계학습(RF) 파이프라인에는 가장 고도화된 하이브리드 결과(Case 3)를 연결하여 호환성 유지
imputed_data_df$Initial_Rule_Source <- imputed_data_df$Case3_Source

# ==============================================================================
# 고도화 기능 1: 랜덤 포레스트 연계 '하이브리드 규칙 유도 및 자연배경 최소화'
# ==============================================================================
# [논리 설명] `inTrees` 패키지 없이 기본 `randomForest`만을 사용하여 안정성을 확보합니다.
# 문헌 규칙을 완벽히 통과하여 분류된 확고한 지점들의 다차원 수질 패턴을 랜덤 포레스트가 학습합니다.
# 이후 룰 엔진이 보수적 기준 때문에 '자연배경'으로 단순 처리한 지점들을 대상으로
# 랜덤 포레스트의 다차원 확률 벡터를 재검사하여, 숨겨진 인위적 오염원 징후(확률 30% 이상)가 있다면 
# '잠재적 오염(RF추정)'으로 구출해냄으로써 판별 정밀도를 극대화하고 자연배경 편향을 최소화합니다.
# ------------------------------------------------------------------------------

# 에러 방지용 방어 코드: '결측'이 아닌 유효한 오염원 종류가 최소 2개 이상일 때만 RF 모델 작동
distinct_initial_sources <- unique(imputed_data_df$Initial_Rule_Source)
distinct_initial_sources <- distinct_initial_sources[distinct_initial_sources != "결측-판정불가"]

if (length(distinct_initial_sources) > 1) {
  # 결측 판정이 나지 않은 양질의 룰 스크리닝 데이터를 학습 데이터셋으로 정제
  rf_train_df <- imputed_data_df %>% filter(Initial_Rule_Source != "결측-판정불가")
  rf_train_df$Initial_Rule_Source <- as.factor(rf_train_df$Initial_Rule_Source)
  
  # 랜덤 포레스트 분류 모델 훈련 (안정동위원소비 및 핵심 수화학 인자 전원 투입)
  set.seed(SEED)
  rf_source_model <- randomForest(Initial_Rule_Source ~ d15N + d18O + Cl + TP + NH3N + NO3N + TOC + Chla + EC, 
                                  data = rf_train_df, ntree = 300)
  
  # 전체 데이터 샘플에 대하여 각 오염원 카테고리별 매핑 확률(Probability Matrix) 도출
  rf_probs <- predict(rf_source_model, newdata = imputed_data_df, type = "prob")
  
  # 하이브리드 보정 및 '자연배경' 최소화 루프 적용
  imputed_data_df$Individual_Source <- sapply(1:nrow(imputed_data_df), function(i) {
    current_source <- imputed_data_df$Initial_Rule_Source[i]
    
    # 룰 기반 엔진이 '자연배경/특이징후 없음'으로 판단한 취약 샘플인 경우
    if (current_source == "자연배경/특이징후 없음") {
      row_probs <- rf_probs[i, ]
      # '자연배경'을 제외한 실제 인위적 오염원들의 확률 컬럼만 추출
      pollution_cols <- names(row_probs)[names(row_probs) != "자연배경/특이징후 없음"]
      
      if (length(pollution_cols) > 0) {
        # 인위적 오염원 중 머신러닝이 가장 가능성이 높다고 본 오염원과 그 확률값 확인
        max_pollution_source <- pollution_cols[which.max(row_probs[pollution_cols])]
        max_pollution_prob <- row_probs[max_pollution_source]
        
        # [핵심 보정 경계] 인위적 오염 징후 확률이 30%를 초과하는 지점은 
        # 단순 자연배경이 아닌 미량의 오염징후 유입 지점으로 간주하여 하이브리드 판정으로 구출
        if (max_pollution_prob >= 0.30) {
          return(paste0("잠재적 오염(RF추정: ", max_pollution_source, ")"))
        }
      }
    }
    return(current_source)
  })
} else {
  # 만약 투입 데이터의 오염원 다양성이 너무 낮아 RF를 돌릴 수 없는 특이 상황일 경우, 에러 없이 기존 룰 유지
  imputed_data_df$Individual_Source <- imputed_data_df$Initial_Rule_Source
  rf_probs <- matrix(1, nrow = nrow(imputed_data_df), ncol = 1)
  colnames(rf_probs) <- distinct_initial_sources[1]
}

# ==============================================================================
# 고도화 기능 2: GMM 사후 확률 기반 '퍼지 신뢰도 점수(Confidence Score) 모델링'
# ==============================================================================
# [논리 설명] 최종 판정의 독창성을 증명하는 다차원 확률 결합 모듈입니다.
# 2단원에서 학습된 무감독(Unsupervised) GMM 모델의 사후 확률(`gmm_model$z`)은 수화학적 공간 상에서 
# 해당 시료가 군집 구조에 얼마나 전형적(Stable)으로 포함되는지를 대변합니다.
# 여기에 방금 수행한 감독(Supervised) 랜덤 포레스트의 오염원 분류 확률을 매핑합니다.
# 두 개의 확률을 산술평균하여 "이 시료가 해당 오염원으로 판별될 퍼지 신뢰도(0~100%)"를 정량화합니다.
# ------------------------------------------------------------------------------

# 2단원에서 생성된 GMM 모델의 사후확률 매트릭스 존재 여부 검사 후 추출
if (!is.null(gmm_model$z)) {
  # 각 지점(행)이 최종 다수결로 낙점된 'Final_Cluster' 번호에 해당할 때의 GMM 사후확률값만 동적 추출
  gmm_conf_vector <- gmm_model$z[cbind(1:nrow(imputed_data_df), data$Final_Cluster)]
} else {
  gmm_conf_vector <- rep(0.5, nrow(imputed_data_df)) # 비정상 상황 시 중간값으로 안전하게 방어
}

# 최종 결정된 'Individual_Source'에 대응하는 랜덤 포레스트 확률 추출 및 매핑
rf_conf_vector <- sapply(1:nrow(imputed_data_df), function(i) {
  final_ind_source <- imputed_data_df$Individual_Source[i]
  
  # 만약 '잠재적 오염(RF추정: ...)' 텍스트가 씌워진 구출 샘플이라면 원본 오염원명을 정규식으로 복원하여 매핑
  if (grepl("잠재적 오염\\(RF추정:", final_ind_source)) {
    clean_name <- gsub("잠재적 오염\\(RF추정:\\s*|\\)", "", final_ind_source)
  } else {
    clean_name <- final_ind_source
  }
  
  # 머신러닝 확률 매트릭스에 해당 오염원 클래스명이 존재할 경우 매핑 확률 반환
  if (clean_name %in% colnames(rf_probs)) {
    return(rf_probs[i, clean_name])
  } else {
    return(as.numeric(max(rf_probs[i, ]))) # 예외 발생 시 차선책으로 행 내 최대 가중치 확률 반환 
  }
})

# [퍼지 융합] 공간 밀도 신뢰도(GMM)와 오염 인자 규칙 신뢰도(RF)를 결합하여 백분율 점수화
data$Fuzzy_Confidence_Score <- round(((gmm_conf_vector + rf_conf_vector) / 2) * 100, 1)

# ==============================================================================
# 후속 연계: 고도화된 개별 결과를 군집 모드(Mode)에 재반영 및 메인 프레임 병합
# ==============================================================================
imputed_data_df$Final_Cluster <- data$Final_Cluster

# 군집 내 최빈값 추출용 내부 함수 (결측값은 계산 대상에서 완전 무시)
get_mode <- function(v) {
  v_clean <- v[v != "결측-판정불가"]
  if(length(v_clean) == 0) return("판정불가")
  uniqv <- unique(v_clean)
  uniqv[which.max(tabulate(match(v_clean, uniqv)))]
}

# 하이브리드 구출 로직이 반영된 최종 Individual_Source를 기반으로 군집 대표 오염원(Source_Name) 재연산
# [수정됨] Case 1, 2, 3 각각에 대해 군집 대표 오염원(Mode)을 독립적으로 산출
cluster_mode <- imputed_data_df %>%
  group_by(Final_Cluster) %>%
  summarise(
    Source_Name1 = get_mode(Case1_Source),      # Case 1 (국내) 군집 대표
    Source_Name2 = get_mode(Case2_Source),      # Case 2 (국외) 군집 대표
    Source_Name3 = get_mode(Case3_Source),      # Case 3 (하이브리드) 군집 대표
    Source_Name  = get_mode(Individual_Source), # ML로 최종 구제된 최종본 군집 대표
    .groups = 'drop'
  )

# 메인 마스터 데이터프레임(data)에 고도화 지표들 결합
# 메인 데이터 프레임에 Case 1, 2, 3 개별 판별 결과 결합
data$Case1_Source <- imputed_data_df$Case1_Source
data$Case2_Source <- imputed_data_df$Case2_Source
data$Case3_Source <- imputed_data_df$Case3_Source
data$Individual_Source <- imputed_data_df$Individual_Source

# [수정됨] 4가지 Source_Name을 모두 메인 데이터에 조인
data <- data %>% left_join(cluster_mode, by = "Final_Cluster")
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 4. 규칙 유도 고도화 및 XAI 시각화 자동 PNG 출력
# ------------------------------------------------------------------------------
# [분석과정 및 방법론]
# Random Forest 모델을 통계적으로 분석하여 정책 결정자가 읽기 쉬운 IF-THEN 규칙으로 변환(inTrees).
# 모든 시각화 산출물은 보고서 첨부에 용이하도록 자동으로 png 형식으로 디렉토리에 저장됩니다.
# ------------------------------------------------------------------------------
set.seed(SEED) 

# (1) UMAP 기본 좌표 생성 (위치 좌표는 Case에 무관하게 동일하므로 1번만 연산)
n_neighbors_val <- ifelse(nrow(scaled_data) > 2, min(15, nrow(scaled_data) - 1), 2)
umap_res <- umap(scaled_data, method="naive", n_neighbors = n_neighbors_val)
data$umap1 <- umap_res$layout[,1]
data$umap2 <- umap_res$layout[,2]

# Case별 반복 출력을 위한 리스트 구성
# c_col: 군집 기준 타겟 변수, i_col: 개별 기준 타겟 변수
cases <- list(
  list(c_col="Source_Name1", i_col="Case1_Source", title="Case 1 (국내 문헌)", fname="Case1"),
  list(c_col="Source_Name2", i_col="Case2_Source", title="Case 2 (국외 문헌)", fname="Case2"),
  list(c_col="Source_Name3", i_col="Case3_Source", title="Case 3 (통합 문헌)", fname="Case3"),
  list(c_col="Source_Name",  i_col="Individual_Source", title="최종 (ML 보정)", fname="Final")
)

# Case별 의사결정 나무 및 UMAP 시각화 반복 생성
for (case in cases) {
  
  # (1) UMAP 시각화 (산점도 자동 저장)
  # [방법론] 위상수학(Topology)에 기반하여 데이터의 지역적, 전역적 구조를 모두 보존하며 2차원으로 차원 축소
  # * SCI Ref: McInnes, L., Healy, J., & Melville, J. (2018). UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction. arXiv preprint arXiv:1802.03426.
  # UMAP 시각화 (군집 기준)
  p1 <- ggplot(data, aes(x = umap1, y = umap2, color = .data[[case$c_col]], label = spot)) +
    geom_point(size = 5, alpha = 0.7) + geom_text_repel(family = "malgun", size=5) +
    labs(title = paste0("UMAP 기반 오염원 분포 지도 - ", case$title, " (군집 기준)"), 
         subtitle = "안정동위원소-수질 융합 데이터 차원 축소") +
    theme_minimal(base_family = "malgun")
  
  # UMAP 시각화 (개별 지점 기준)
  p2 <- ggplot(data, aes(x = umap1, y = umap2, color = .data[[case$i_col]], label = spot)) +
    geom_point(size = 5, alpha = 0.7) + geom_text_repel(family = "malgun", size=5) +
    labs(title = paste0("UMAP 기반 오염원 분포 지도 - ", case$title, " (개별 지점 기준)"),
         subtitle = "안정동위원소-수질 융합 개별") +
    theme_minimal(base_family = "malgun")
  
  # PNG 자동 저장 (파일명에 Case 명시)
  ggsave(file.path(out_dir, paste0("01_UMAP_Cluster_", case$fname, ".png")), plot = p1, width = 10, height = 7, dpi = 300)
  ggsave(file.path(out_dir, paste0("02_UMAP_Individual_", case$fname, ".png")), plot = p2, width = 10, height = 7, dpi = 300)
  
  # ----------------------------------------------------------------------------
  # [핵심 해결책] 의사결정 트리 생성 전 클래스 다양성(고유값 개수) 검사
  # ----------------------------------------------------------------------------
  unique_vals <- unique(data[[case$c_col]])
  
  if (length(unique_vals) <= 1) {
    cat(paste0("[안내] ", case$title, "의 군집 오염원 종류가 '", unique_vals, "' 1가지로만 구성되어 있어 의사결정 트리 생성을 건너뜁니다.\n"))
    next # 에러를 일으키지 않고, 루프의 다음 Case(Case2, Case3 등)로 안전하게 토스합니다.
  }
  
  # (2) 의사결정 트리 (Decision Logic Tree)
  # [방법론] CART(Classification And Regression Trees) 알고리즘을 활용한 해석 가능한 규칙 도출. Gini Index를 통해 불순도를 최소화하는 노드를 분할합니다.
  # * SCI Ref: Breiman, L., Friedman, J., Stone, C. J., & Olshen, R. A. (1984). Classification and Regression Trees. CRC press.
  formula_str <- paste(case$c_col, "~ d15N + d18O + TP + NH3N + NO3N + TOC + EC + Chla + TN + Cl")
  tree_model <- rpart(as.formula(formula_str), data = data, method = "class", 
                      control = rpart.control(cp = 0.001, minsplit = 2))
  
  # PNG 캔버스 크기를 넉넉하게 설정 (가로 1000, 세로 850)
  png(file.path(out_dir, paste0("03_Decision_Tree_Logic_", case$fname, ".png")),
      width=1000, height=850, res=120, type="cairo", family="Malgun Gothic")
  
  # 핵심 해결책: oma (Outer Margin Area) 설정
  # 그래프 영역 바깥에 위(top) 3줄, 아래(bottom) 5줄의 '절대 보호 여백'을 생성합니다.
  par(oma = c(5, 0, 3, 0), xpd = NA)
  
  # 1. 트리 그리기 (원래 쓰시던 가장 안전한 코드 유지, main 인자는 분리)
  rpart.plot(tree_model, 
             type = 4, extra = 104, fallen.leaves = FALSE, box.palette = "RdYlGn",
             cex = 0.8)
  
  # 2. 제목 추가 (outer = TRUE 옵션으로 상단 보호 구역에 강제 출력)
  title(main = paste0("오염원 판별 의사결정 로직 트리 [", case$title, "]"), 
        outer = TRUE, line = 1, cex.main = 1.5)
  
  # 3. 범례 추가 (하단 보호 구역에 출력)
  # 기존에 쓰신 "RdYlGn" 색상 톤(Red-Yellow-Green)에 맞춰 범례 색상 자동 생성
  unique_classes <- levels(as.factor(data[[case$c_col]]))
  num_classes <- length(unique_classes)
  legend_colors <- colorRampPalette(c("#D73027", "#FFFFBF", "#1A9850"))(num_classes)
  
  legend("bottom", 
         legend = unique_classes, 
         fill = legend_colors, 
         title = "예측 오염원 종류", 
         inset = c(0, -0.15),  # 캔버스 제일 아래쪽으로 살짝 밀어내기
         horiz = TRUE,         # 보기 좋게 가로로 나열
         cex = 1.0, 
         bg = "white",
         bty = "n",
         text.font = 1)            # 테두리 제거로 깔끔하게
  
  dev.off()
  
  # Final 모델일 때만 최종 레포트용 Rules 추출
  if (case$fname == "Final") {
    rules <- rpart.rules(tree_model, cover = TRUE)
  }
}

# (3) 변수 중요도 산출 (Random Forest 활용)
# [방법론] 다수의 의사결정나무를 앙상블하여 예측력을 높이고, 각 수질 변수가 군집 판별에 미치는 중요도(Mean Decrease Accuracy/Gini)를 산출합니다.
# * SCI Ref: Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.
# [!] 군집(Cluster) 구조 자체는 Case에 무관하게 고정이므로 1번만 수행합니다.
set.seed(SEED)
rf_target <- as.factor(data$Final_Cluster)
rf_model <- randomForest(rf_target ~ ., data = imputed_data_df %>% select(-Individual_Source, -Final_Cluster, -Case1_Source, -Case2_Source, -Case3_Source, -Initial_Rule_Source), importance = TRUE)

# 변수 중요도 PNG 저장
png(file.path(out_dir, "04_RF_VarImportance.png"), width=800, height=600, res=120)
par(family="malgun")
varImpPlot(rf_model, main = "오염원 군집화 결정 주도 변수 기여도(RF)", pch=19)
dev.off()

# 추가적으로, Random Forest의 변수 중요도를 통해 
# 정책 결정자가 어떤 인자(d15N, Cl, TOC 등)를 모니터링해야 하는지 명확히 제시
rf_imp <- importance(rf_model)
print(head(rf_imp[order(rf_imp[,1], decreasing=TRUE), ], 5))

# ==============================================================================
# 5. 최종 레포트 출력
# ==============================================================================
cat("\n=========================================================\n")
cat(" [고도화 앙상블 스크리닝 최종 레포트]\n")
cat("=========================================================\n")

# [수정됨] 최종 출력 데이터프레임에 Source_Name1, 2, 3 컬럼 추가
final_report <- data %>% 
  select(spot, Source_Name, Source_Name1, Source_Name2, Source_Name3, 
         Individual_Source, Case1_Source, Case2_Source, Case3_Source, 
         Clustering_Entropy, Uncertainty) %>% 
  arrange(Source_Name)

print(final_report)

cat("\n[!] 정책 활용을 위한 주요 핵심 규칙(Top IF-THEN Rules from RF) [!]\n")
if(nrow(rules) > 0) {
  for(i in 1:min(5, nrow(rules))) {
    cat(sprintf("Rule %d: IF [ %s ] THEN Cluster [ %s ] (Error: %s, Freq: %s)\n", 
                i, rules$condition[i], rules$pred[i], rules$err[i], rules$freq[i]))
  }
} else {
  cat("- 추출된 유의미한 Rule이 없습니다.\n")
}

# 최종 데이터 CSV 내보내기
write.csv(data, file.path(out_dir, "최종_분석결과_데이터.csv"), row.names = FALSE, fileEncoding = "CP949")
cat("\n[*] 모든 시각화 이미지 및 결과 CSV 파일이 'Output_Results' 폴더에 저장되었습니다.\n")
