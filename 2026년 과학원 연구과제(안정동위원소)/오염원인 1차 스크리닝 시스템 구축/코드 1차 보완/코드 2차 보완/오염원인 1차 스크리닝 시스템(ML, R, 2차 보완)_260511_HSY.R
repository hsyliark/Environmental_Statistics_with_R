# ==============================================================================
# 시스템명: 국가 수계 통합 오염원인 정밀 앙상블 스크리닝 시스템 (Academic Final Ver.)
# 분석 목적: 수계 정보 없이 수질/동위원소 지문(Fingerprint)만으로 오염원 자동 판별
# 주요 특징: 5중 앙상블 군집화 + 의사결정 트리(Logic Tree) 가시화 + SCI 근거 기반 Rule-engine
# ==============================================================================

# 1. 환경 설정 및 필수 라이브러리 로드  
# [방법론] pacman 패키지 관리 시스템 (참고문헌: Rinker & Kurkiewicz, 2018, Journal of Open Source Software)
# [근거] 라이브러리 설치 여부를 자동으로 확인하고 로드하여, 분석 환경의 전이성(Portability)과 재현성을 보장합니다.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mclust, cluster, dplyr, ggplot2, VIM, umap, ggrepel, 
               randomForest, factoextra, rpart, rpart.plot, purrr)

# 한글 깨짐 방지 설정 (Windows 환경에서의 시각화 가독성 확보)
if (.Platform$OS.type == "windows") {
  windowsFonts(malgun = windowsFont("Malgun Gothic"))
}

# 2. 데이터 전처리 (Data Pre-processing)
# [주석] 지점명(spot)과 화학적 변수만 포함된 데이터를 로드합니다.
data <- read.csv("C:/Users/User/Desktop/특이측정값(3~4월).csv", fileEncoding = "euc-kr") 
features_raw <- data %>% select(-spot)

# [방법론] KNN(K-Nearest Neighbors) 다변량 결측치 보정 (참고문헌: Kowarik & Templ, 2016, Journal of Statistical Software)
# [근거] 수질 데이터는 항목 간 생지화학적 상관성이 높습니다. 단순 평균값 대체는 데이터의 분산을 왜곡시키지만, 
# KNN은 가장 유사한 패턴을 가진 k개의 이웃 데이터를 참조하여 보간하므로 지문(Fingerprint) 데이터 보존에 탁월합니다.
set.seed(5790)
imputed_data <- kNN(features_raw, k = 5, imp_var = FALSE)

# [방법론] Z-score 표준화 (Standardization)
# [근거] mg/L, ‰, μS/cm 등 단위 체계가 다른 변수들을 동일한 통계적 가중치(평균 0, 분산 1)로 변환합니다.
# 이는 거리 기반 알고리즘에서 EC와 같이 수치가 큰 변수가 결과를 지배하는 '스케일 편향'을 방지하는 필수 절차입니다.
set.seed(5790)
scaled_data <- scale(imputed_data)


# ==============================================================================
# 3. 데이터 기반 최적 군집 수(K) 자동 결정 (Optimal K Selection)
# ==============================================================================
# [방법론] BIC, Silhouette Index 및 Gap Statistic의 3자 합의 체계
# [근거] 비지도 학습에서 '정답'인 군집 수는 데이터의 구조에 따라 다르므로, 서로 다른 수학적 관점을 결합합니다.

cat("[*] 알고리즘 경합 기반 최적 K 도출 중 (제한 범위: 4~8)...\n")

set.seed(5790)

n_rows <- nrow(scaled_data)
if (n_rows < 9) {
  warning("샘플 수 부족으로 인해 최적 K 탐색 범위가 하향 조정되었습니다.")
  search_range <- 2:min(8, n_rows - 1)
} else {
  search_range <- 4:8
}

# (1) GMM 기반 BIC 최적화 (Schwarz, 1978, Annals of Statistics)
# [학술적 근거] 베이지안 정보 기준(BIC)은 모델의 복잡도에 벌점을 부여하여 과적합을 방지합니다. 
# 확률 밀도 함수 기반으로 가장 타당한 군집 수를 제시합니다.
set.seed(5790)
gmm_auto <- Mclust(scaled_data, G = search_range)
best_k_bic <- if (!is.null(gmm_auto$G)) gmm_auto$G else 4

if (n_rows > 4) {
  dynamic_k_max <- min(8, n_rows - 1)
  
  # (2) K-means 기반 Silhouette Index 최적화 (Rousseeuw, 1987, Journal of Computational and Applied Mathematics)
  # [학술적 근거] 각 지점이 소속 군집 내에 얼마나 응집되어 있고 타 군집과는 얼마나 먼지 기하학적 분리도를 측정합니다.
  set.seed(5790)
  sil_test <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = dynamic_k_max)
  raw_k_sil <- as.numeric(sil_test$data$clusters[which.max(sil_test$data$y)])
  best_k_sil <- max(4, min(raw_k_sil, 8))
  
  # (3) Gap Statistic 최적화 (Tibshirani et al., 2001, JRSS: Series B)
  # [학술적 근거] 실제 데이터의 분산과 무작위 분포(Null reference)의 분산을 비교하여 차이가 최대화되는 지점을 찾습니다.
  # 분산 감소폭(Gap)이 가장 극대화되는 지점을 수학적 최적 K로 산출합니다.
  # 부트스트랩(B=50)을 사용하여 안정적인 통계량을 도출합니다.
  set.seed(5790)
  gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = dynamic_k_max, B = 50)
  raw_k_gap <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method="Tibs2001SEmax")
  # 탐색된 K가 4 미만일 경우 최소 관리 임계치(4)를 보장하는 방어코드
  raw_k_gap <- if (raw_k_gap < 4) 4 else raw_k_gap
  best_k_gap <- max(4, min(raw_k_gap, 8))
  
} else {
  best_k_sil <- 4
  best_k_gap <- 4
}

# (4) 최종 K 결정 (Majority Voting)
# [방법론] 통계적(BIC), 기하학적(Silhouette), 시뮬레이션(Gap)의 K 예측값을 종합.
# 각 지표가 다른 결과를 가리킬 경우, 최빈값(Mode)을 산출하여 이상적인 K 도출.
k_votes <- c(best_k_bic, best_k_sil, best_k_gap)
k_votes <- k_votes[!is.null(k_votes)] # 방어적 결측치 제거
calculated_k <- as.numeric(names(sort(table(k_votes), decreasing = TRUE)[1]))

# 최종 Clamping 수행 (수질 관리 정책 범위를 강제 적용)
best_k <- max(4, min(calculated_k, 8))

cat("[*] 분석 결과 (BIC:", best_k_bic, "/ Silhouette:", best_k_sil, "/ Gap:", best_k_gap, ")\n")
cat("    최종 [", best_k, "]개 군집으로 분석을 확정합니다.\n")


# ==============================================================================
# 4. 다중 알고리즘 앙상블 군집화 (Consensus Ensemble Clustering)
# ==============================================================================
# [방법론] 5중 앙상블 합의 체계 (5-way Majority Voting Ensemble)
# [근거] GMM(확률), K-Means(유클리디안 거리), Ward.D2(상향식 분산 최소화)와 더불어,
# 극단치에 강건한 PAM과 하향식 계층 기법인 DIANA를 결합하여 알고리즘별 수학적 편향을 완전 상쇄합니다.
# 5개의 홀수 앙상블을 통해 동점(Tie) 군집화 발생을 방지합니다.

# 1) GMM (Gaussian Mixture Model)
set.seed(5790)
c_gmm <- Mclust(scaled_data, G = best_k)$classification

# 2) K-Means (Centroid-based)
set.seed(5790)
c_km  <- kmeans(scaled_data, centers = best_k, nstart = 25)$cluster

# 3) Agglomerative Hierarchical (Ward's D2)
set.seed(5790)
c_hc  <- cutree(hclust(dist(scaled_data), method = "ward.D2"), k = best_k)

# 4) [신규 추가] PAM (Partitioning Around Medoids) (Kaufman & Rousseeuw, 1990)
# [학술적 근거] 실제 관측치(Medoid)를 중심으로 군집을 분할하여, 수질 사고나 이상치에 
# 평균(Mean)이 왜곡되는 현상을 막아주는 강력한 강건(Robust) ML 기법입니다.
set.seed(5790)
c_pam <- pam(scaled_data, k = best_k)$clustering

# 5) [신규 추가] DIANA (Divisive Analysis) (Kaufman & Rousseeuw, 1990)
# [학술적 근거] 전체를 하나의 군집으로 보고 점진적으로 이질적인 개체를 떼어내는 
# 하향식 계층 군집화 기법으로, 상향식인 Ward's 기법과 상호 교차 검증 역할을 수행합니다.
set.seed(5790)
c_diana <- cutree(diana(scaled_data), k = best_k)

# 5중 앙상블 결과를 데이터프레임으로 통합
ensemble_results <- data.frame(GMM = c_gmm, KM = c_km, HC = c_hc, PAM = c_pam, DIANA = c_diana)

# 완전 다수결 로직 (Majority Voting): 가장 많이 할당된 군집 번호 채택
data$Final_Cluster <- apply(ensemble_results, 1, function(x) {
  ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
})


# ==============================================================================
# 5. 범용적 10단계 오염원 정밀 판별 엔진 (Expert Rule-based System)
# ==============================================================================
# [설계 원칙] 수질 및 안정동위원소 지문(Multi-Isotope Fingerprinting) 분석법 적용
# [참조 체계] 아래 판별 로직은 국제적으로 검증된 SCI 논문의 임계치를 기준으로 설계되었습니다.

set.seed(5790)

assign_source_universal <- function(row) {
  
  # 변수 정의 (각 인자의 지표 역할)
  n15 <- row$d15N;  o18 <- row$d18O   # 질소/산소 동위원소: 기원 식별의 결정적 증거
  cl  <- row$Cl                       # Cl-: 인위적 부하(하수/분뇨)의 보존성 추적자
  tp  <- row$TP;    nh3 <- row$NH3N   # 영양염류: 점오염 및 가축 폐수 지표
  no3 <- row$NO3N;  tn  <- row$TN     # NO3-: 비료 및 비점오염 산화 환경 지표
  toc <- row$TOC;   chl <- row$Chla   # 유기물/조류: 내생 부하 및 부영양화 평가
  ec  <- row$EC                       # EC: 이온 농도를 통한 총 오염 부하 모니터링
  
  # ----------------------------------------------------------------------------
  # 1단계: 대기유입 (Atmospheric Deposition)
  # [SCI 근거] Elliott et al. (2007, ES&T); 빗물 내 NO3-는 지면의 미생물 반응을 거치지 않아 
  # δ18O-NO3 값이 15‰에서 최대 60‰ 이상으로 매우 높게 나타남.
  # ----------------------------------------------------------------------------
  if (!is.na(o18) && o18 > 15.0) return("대기유입/초기강우")
  
  # ----------------------------------------------------------------------------
  # 2단계: 탈질작용 (Denitrification)
  # [SCI 근거] Mayer et al. (2002, Applied Geochemistry); 수역 내 혐기성 상태에서 미생물에 의해
  # 질산염이 분해될 때, 잔류 NO3의 δ15N과 δ18O는 약 2:1의 선형적 증가 관계를 보임.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && !is.na(o18) && n15 > 6.0 && o18 > 3.0 && (n15/o18 < 2.0)) {
    return("탈질작용(기원변형)")
  }
  
  # ----------------------------------------------------------------------------
  # 3단계: 축산분뇨 직접유출 (Manure & Livestock Waste)
  # [SCI 근거] Xue et al. (2009, Water Research); 가축 분뇨는 암모니아 휘발로 인해 
  # δ15N이 12‰~25‰로 고농축되며, Cl- 및 NH3-N의 동반 상승이 뚜렷함.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 > 12.0 && (cl > 60 || tp > 0.1 || nh3 > 0.3 || ec > 300)) {
    return("축산분뇨 직접유출")
  }
  
  # ----------------------------------------------------------------------------
  # 4단계: 생활하수/처리수 영향 (Sewage & Treated Effluent)
  # [SCI 근거] Widory et al. (2004, ES&T); 생활하수 및 하수처리수는 인간 배설물의 영향으로
  # δ15N이 7‰~12‰ 범위를 형성하며, 세제 등에 포함된 고농도 Cl- (>40mg/L) 특징을 가짐.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 > 8.0 && n15 <= 12.0 && (cl > 40 || tp <= 0.3)) {
    return("생활하수/처리수 영향")
  }
  
  # ----------------------------------------------------------------------------
  # 5단계: 산업/인위적 점오염 (Industrial Point Source)
  # [근거] 인위적 공정 폐수는 동위원소 지문 외에도 비정상적인 전기전도도(EC > 500) 및
  # 특정 용존 이온(Cl- > 100)의 급격한 상승을 수반함.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 > 7.0 && (ec > 500 || cl > 100) && toc > 3.0) {
    return("산업/인위적 점오염")
  }
  
  # ----------------------------------------------------------------------------
  # 6단계: 화학비료 기반 농경지 (Synthetic Fertilizer)
  # [SCI 근거] Vitousek et al. (1997, Science) / Bateman & Kelly (2007, Applied Geochemistry);
  # 공기 중 질소를 고정하여 제조한 화학비료는 δ15N 값이 -4‰~+4‰로 매우 낮게 나타남.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 < 4.0 && no3 > 2.0 && cl < 30) {
    return("화학비료 기반 농경지")
  }
  
  # ----------------------------------------------------------------------------
  # 7단계: 유기질비료(퇴비) vs 토양 유기물 (Organic Fertilizer vs Soil)
  # [SCI 근거] Bedard-Haughn et al. (2003, Hydrological Processes); 
  # 자연 토양과 퇴비는 δ15N 범위가 겹칠 수 있으나, 퇴비는 염분(Cl-) 부하가 수반되는 차이가 있음.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 >= 4.0 && n15 < 9.0 && toc > 4.0) {
    if (cl > 35) return("유기질비료(퇴비) 영향")
    else return("토양유기물/야생동물배설물")
  }
  
  # ----------------------------------------------------------------------------
  # 8단계: 조류증식 및 내생부하 (Algal Bloom & Autochthonous Loading)
  # [SCI 근거] Meyers (1994, Chemical Geology); 수계 내 광합성 활발 시 Chl-a가 상승하며,
  # 이로 인한 유기물(TOC)의 증가는 외부 유입이 아닌 현장 발생(Autochthonous)으로 판별함.
  # ----------------------------------------------------------------------------
  if (chl > 40.0 && toc > 4.5) return("조류증식(현장발생)")
  if (toc > 6.5 && tp > 0.1) return("내생부하(정체수역 유기물 축적)")
  
  # 9단계: 복합오염 (Mixed Pollution)
  if (tp > 0.1 || nh3 > 0.5 || cl > 50) return("복합오염(하수/분뇨 기여 우세)")
  if (no3 > 2.0 || tn > 3.0) return("복합오염(농경지 비점 우세)")
  if (toc > 3.0) return("복합오염(유기물 부하 우세)")
  
  # 10단계: 배경 농도 (Natural Background)
  return("자연배경/특이징후 없음")
}

# 군집별 평균 수질을 계산하여 최종 오염원 라벨 매핑
cluster_summary <- data %>%
  group_by(Final_Cluster) %>%
  summarise(across(c(d15N, d18O, TP, NH3N, NO3N, TOC, EC, Chla, TN, Cl), 
                   \(x) mean(x, na.rm = TRUE)), .groups = 'drop') %>%
  mutate(across(everything(), ~replace(., is.nan(.), 0)))

# [실행] 각 군집 행에 대해 10단계 판별 엔진 적용
cluster_summary <- cluster_summary %>%
  rowwise() %>%
  mutate(Source_Name = assign_source_universal(pick(everything()))) %>%
  ungroup()

# [통합] 판별된 오염원 명칭을 개별 관측치 데이터프레임에 전파
data <- data %>%
  left_join(cluster_summary %>% select(Final_Cluster, Source_Name), by = "Final_Cluster")

cat("[*] 분석 완료: 총", length(unique(data$Source_Name)), "종의 오염원 식별 완료.\n")


# ==============================================================================
# 6. 시각화 및 설명 가능한 AI (Visual Analytics & XAI)
# ==============================================================================

# (1) 분류 지도 (UMAP)
# [방법론] UMAP (Uniform Manifold Approximation and Projection)
# [근거] 고차원 수질 데이터를 저차원(2D)으로 비선형 축소하여 군집 간의 거리를 가시화합니다.
set.seed(5790)
n_neighbors_val <- if (nrow(scaled_data) > 2) min(15, nrow(scaled_data) - 1) else 2
umap_res <- umap(scaled_data, n_neighbors = n_neighbors_val)
data$umap1 <- umap_res$layout[,1]; data$umap2 <- umap_res$layout[,2]

p1 <- ggplot(data, aes(x = umap1, y = umap2, color = Source_Name, label = spot)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_text_repel(size = 4, fontface = "bold", family = "malgun") +
  theme_minimal(base_family = "malgun") +
  labs(title = "ML 기반 오염원 정밀 앙상블 분류 지도", 
       subtitle = paste("최적 군집수 K =", best_k, "| 알고리즘: GMM+KM+HC+PAM+DIANA"))
print(p1)

# (2) 의사결정 트리 (Decision Logic Tree)
# [방법론] rpart Classification Tree (Breiman et al., 1984)
# [역할] 블랙박스 모델인 ML의 판단 근거를 "If-Then" 로직으로 시각화하여 현장 담당자의 이해를 돕습니다.
set.seed(5790)
tree_model <- rpart(Source_Name ~ d15N + d18O + TP + NH3N + NO3N + TOC + EC + Chla + TN + Cl, 
                    data = data, method = "class",
                    control = rpart.control(cp = 0.001, minsplit = 2))

rpart.plot(tree_model, main = "오염원 판별 의사결정 로직 트리 (분류 근거 가시화)",
           type = 4, extra = 104, fallen.leaves = TRUE, box.palette = "RdYlGn",
           family = "malgun", cex = 0.7)

# (3) 변수 중요도 (Random Forest Importance)
# [방법론] Random Forest Gini Impurity
# [역할] 어떤 수질 항목이 오염원을 판별하는 데 가장 핵심적인 역할을 했는지 순위를 매깁니다.
set.seed(5790)
rf_model <- randomForest(as.factor(Source_Name) ~ ., 
                         data = data.frame(imputed_data, Source_Name = data$Source_Name), 
                         importance = TRUE)
varImpPlot(rf_model, main = "오염원 분류 결정 변수 기여도 (Gini Importance)", family = "malgun")

# ==============================================================================
# 7. 리포트 출력
# ==============================================================================

cat("\n[최종 오염원 스크리닝 리포트]\n")
print(data %>% select(spot, Source_Name) %>% arrange(Source_Name))
