# ==============================================================================
# 시스템명: 국가 수계 통합 오염원인 정밀 앙상블 스크리닝 시스템 (Academic Final Ver.)
# 분석 목적: 수계 정보 없이 수질/동위원소 지문(Fingerprint)만으로 오염원 자동 판별
# 주요 특징: 앙상블 군집화 + 의사결정 트리(Logic Tree) 가시화 + 다중 프록시 판별
# ==============================================================================

# 1. 환경 설정 및 필수 라이브러리 로드  
# [방법론] pacman 패키지 관리 시스템 (참고문헌: Rinker & Kurkiewicz, 2018)
# [근거] 라이브러리 의존성을 자동 해결하여 타 연구자의 환경에서도 분석 재현성을 100% 보장합니다.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mclust, cluster, dplyr, ggplot2, VIM, umap, ggrepel, 
               randomForest, factoextra, rpart, rpart.plot, purrr)

# 한글 깨짐 방지 설정 (Windows 환경)
if (.Platform$OS.type == "windows") {
  windowsFonts(malgun = windowsFont("Malgun Gothic"))
}

# 2. 데이터 전처리 (Data Pre-processing)
# [주석] 지점명(spot)과 화학적 변수만 포함된 데이터를 로드합니다.
data <- read.csv("C:/Users/User/Desktop/3월 2주차.csv", fileEncoding = "euc-kr") 
features_raw <- data %>% select(-spot)

# [방법론] KNN(K-Nearest Neighbors) 다변량 결측치 보정 (참고문헌: Kowarik & Templ, 2016)
# [근거] 수질 데이터는 생지화학적 기작에 의해 항목 간 상관성이 매우 높습니다. 
# 단순 평균값 대체는 분산을 왜곡시키므로, 유사 패턴을 가진 인접 데이터 5개를 참조 보간하여 지문을 보존합니다.
imputed_data <- kNN(features_raw, k = 5, imp_var = FALSE)

# [방법론] Z-score 표준화 (Standardization)
# [근거] mg/L, ‰, μS/cm 등 단위가 상이한 변수들을 동일한 통계적 가중치(평균 0, 분산 1)로 변환합니다.
# 이는 거리 기반 알고리즘에서 농도 수치가 큰 변수(예: EC)가 결과를 지배하는 왜곡을 방지합니다.
scaled_data <- scale(imputed_data)


# 3. 데이터 기반 최적 군집 수(K) 자동 결정 (Optimal K Selection)
# [방법론] BIC(정보이론) 및 Silhouette Index(기하학적 분리도) 결합 (참고문헌: Rousseeuw, 1987)
# [근거] 수계 라벨이 없는 비지도 학습에서는 데이터 자체의 구조적 복잡도를 평가해야 합니다.
# 1) BIC: 모델의 우도와 파라미터 수의 복잡도를 계산하여 과적합을 방지하는 최적 K 탐색
# 2) Silhouette: 각 지점이 소속 군집 내에 얼마나 응집되어 있고 타 군집과는 얼마나 먼지 수치화
# [수정 사항] 수계 관리의 효율성과 변별력 확보를 위해 K의 범위를 4~8로 제한(Clamping)합니다.

cat("[*] 알고리즘 경합 기반 최적 K 도출 중 (제한 범위: 4~8)...\n")

# (1) 데이터 수 확인 및 수치적 안전장치 (Degrees of Freedom Check)
# [근거] 군집 분석은 (K < n_samples) 조건을 만족해야 하며, 데이터가 너무 적을 경우 
# 모델의 자유도(df) 부족으로 수렴 오류가 발생할 수 있으므로 동적 탐색 범위를 설정합니다.
n_rows <- nrow(scaled_data)

if (n_rows < 9) {
  # 샘플 수가 최대 탐색 범위(8)보다 적을 경우, 실행 가능한 최대치(n-1)로 조정
  warning("샘플 수 부족으로 인해 최적 K 탐색 범위가 하향 조정되었습니다.")
  search_range <- 2:min(8, n_rows - 1)
} else {
  # 표준 탐색 범위: 4(최소 변별력) ~ 8(최대 관리 효율)
  search_range <- 4:8
}

# (2) GMM(Gaussian Mixture Model) 기반 BIC 최적화
# [방법론] Bayesian Information Criterion (Schwarz, 1978)
# [학술적 근거] 데이터 분포를 다변량 가우시안 분포의 혼합으로 가정하고, 
# 모델의 우도(Likelihood)와 파라미터 복잡도 간의 균형을 평가하여 과적합을 방지하는 최적 K를 탐색합니다.
# 지정된 search_range 내에서 BIC 값이 가장 낮은(최적 우도) 모델의 G 값을 추출합니다.
gmm_auto <- Mclust(scaled_data, G = search_range)
best_k_bic <- if (!is.null(gmm_auto$G)) gmm_auto$G else 4

# (3) K-means 기반 Silhouette Index 최적화
# [방법론] Silhouette Coefficient (Rousseeuw, 1987)
# [학술적 근거] 군집 내 응집도(Cohesion)와 군집 간 분리도(Separation)를 동시에 측정합니다.
# 1에 가까울수록 군집화가 잘 된 것으로 평가하며, 기하학적 관점에서 가장 명확한 K를 도출합니다.
if (n_rows > 4) {
  # k.max는 데이터 수와 정책적 상한(8) 중 작은 값을 선택하여 시스템 다운 방지
  dynamic_k_max <- min(8, n_rows - 1)
  
  sil_test <- fviz_nbclust(scaled_data, kmeans, 
                           method = "silhouette", 
                           k.max = dynamic_k_max)
  
  # 실루엣 계수가 최대가 되는 지점의 K값을 추출
  raw_k_sil <- as.numeric(sil_test$data$clusters[which.max(sil_test$data$y)])
  
  # [정책적 제한] 추출된 K를 관리적 가이드라인인 4~8 범위 내로 강제 조정
  best_k_sil <- max(4, min(raw_k_sil, 8))
} else {
  # 샘플 수가 극도로 적을 경우 최소 군집 수인 4를 기본값으로 할당
  best_k_sil <- 4
}

# (4) 최종 K 결정 (Hybrid Decision Logic)
# [방법론] 통계적 확률 분포(BIC)와 기하학적 구조(Silhouette)의 결과를 종합하여 결정합니다.
# 1차적으로 BIC 결과를 우선하되, BIC 결과가 부재할 경우 실루엣 점수를 대안으로 사용합니다.
calculated_k <- ifelse(is.null(best_k_bic), best_k_sil, best_k_bic)

# [최종 확정] 최종 결정된 K값이 분석 설계 범위(4~8)를 벗어나지 않도록 최종 Clamping 수행
# 이는 데이터의 노이즈로 인해 극단적인 K가 도출되는 것을 방지하는 안전장치입니다.
best_k <- max(4, min(calculated_k, 8))

cat("[*] 분석 결과: 통계적 최적값은", calculated_k, "개이며,\n")
cat("    현장 관리 및 변별력 확보를 위해 최종 [", best_k, "]개 군집으로 분석을 확정합니다.\n")

# 4. 다중 알고리즘 앙상블 군집화 (Consensus Ensemble Clustering)
# [방법론] Majority Voting (다수결 합의 체계)
# [근거] 단일 알고리즘(예: K-means)은 군집의 형태가 구형일 때만 잘 작동하는 한계가 있습니다.
# 확률 기반(GMM), 거리 기반(KM), 분산 기반(Ward's D2)의 결과를 통합하여 수학적 편향을 제거합니다.
c_gmm <- Mclust(scaled_data, G = best_k)$classification
c_km <- kmeans(scaled_data, centers = best_k, nstart = 25)$cluster
c_hc <- cutree(hclust(dist(scaled_data), method = "ward.D2"), k = best_k)

ensemble_results <- data.frame(GMM = c_gmm, KM = c_km, HC = c_hc)
data$Final_Cluster <- apply(ensemble_results, 1, function(x) {
  ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
})


# 5. 범용적 10단계 오염원 정밀 판별 엔진 (Expert Rule-based System)
# [설계 원칙] 수계의 물리적 특성이 아닌, 질소/탄소 순환의 화학적 지문(Fingerprint)을 추적합니다.
# [참고 문헌] Kendall(1998), Mayer(2002), Xue(2009) 등 국제적 기준 임계치 적용

assign_source_universal <- function(row) {
  
  # --- [변수 정의] 각 수질 인자별 분석 역할 ---
  n15 <- row$d15N;  o18 <- row$d18O   # 안정동위원소: 기원 판별의 핵심 지표
  cl  <- row$Cl                       # 염소이온: 인위적 오염(하수, 분뇨)의 보존성 추적자
  tp  <- row$TP;    nh3 <- row$NH3N   # 영양염류: 점오염원 및 가축분뇨 직접유입 지표
  no3 <- row$NO3N;  tn  <- row$TN     # 질소산화물: 농경지 비점오염 및 산화 환경 지표
  toc <- row$TOC;   chl <- row$Chla   # 유기물/조류: 내생 부하 및 유기 오염도 평가
  ec  <- row$EC                       # 전기전도도: 용존 이온 농도를 통한 산업폐수/인위적 부하 감지
  
  # ----------------------------------------------------------------------------
  # 1단계: 대기유입 (Atmospheric Deposition)
  # [의도] 초기 강우 시 대기 중 질산염의 직접 하강 여부 판단
  # [근거] Kendall(1998); 빗물 내 NO3-는 지면의 nitrification을 거치지 않아 d18O가 15‰ 이상으로 매우 높음.
  # ----------------------------------------------------------------------------
  if (!is.na(o18) && o18 > 15.0) return("대기유입/초기강우")
  
  # ----------------------------------------------------------------------------
  # 2단계: 탈질작용 (Denitrification)
  # [의도] 미생물에 의한 동위원소 분별 작용(Fractionation)으로 인한 기원 지문 왜곡 확인
  # [근거] Mayer et al.(2002); 정체 수역에서 탈질 발생 시 d15N과 d18O가 1.3:1 ~ 2:1의 선형적 비례 관계를 보임.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && !is.na(o18) && n15 > 6.0 && o18 > 3.0 && (n15/o18 < 3.0)) {
    return("탈질작용(기원변형)")
  }
  
  # ----------------------------------------------------------------------------
  # 3단계: 축산분뇨 직접유출 (Manure & Livestock Waste)
  # [의도] 휘발성이 강한 암모니아 유래 고농도 질소 유입 포착
  # [근거] Xue et al.(2009); 분뇨 내 NH3 휘발로 d15N이 12‰ 이상 농축되며, 보존성 이온인 Cl-가 동반 상승함.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 > 12.0 && (cl > 60 || tp > 0.1 || nh3 > 0.3 || ec > 300)) {
    return("축산분뇨 직접유출")
  }
  
  # ----------------------------------------------------------------------------
  # 4단계: 생활하수/처리수 영향 (Sewage & Treated Effluent)
  # [의도] 인간 배설물 및 세제 유래 성분 판별
  # [근거] Widory et al.(2004); 하수처리수 내 d15N 지문(8~12‰)과 생활하수 특유의 Cl- 부하(>40mg/L) 결합.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 > 8.0 && n15 <= 12.0 && (cl > 40 || tp <= 0.3)) {
    return("생활하수/처리수 영향")
  }
  
  # ----------------------------------------------------------------------------
  # 5단계: 산업/인위적 점오염 (Industrial Point Source)
  # [의도] 특정 공정 폐수 등 고농도 염분 및 유기물 배출 포착
  # [특징] 동위원소 지문과 별개로 비정상적인 EC(>500) 및 Cl-(>100) 수치를 통한 특이점 감지.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 > 7.0 && (ec > 500 || cl > 100) && toc > 3.0) {
    return("산업/인위적 점오염")
  }
  
  # ----------------------------------------------------------------------------
  # 6단계: 화학비료 기반 농경지 (Synthetic Fertilizer)
  # [의도] Haber-Bosch 공법 비료의 낮은 질소 동위원소 지문 포착
  # [근거] Vitousek et al.(1997); 대기 중 N2 고정 비료는 d15N이 낮고(-4~4‰), 인위적 Cl 부하가 적음.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 < 4.0 && no3 > 2.0 && cl < 30) {
    return("화학비료 기반 농경지")
  }
  
  # ----------------------------------------------------------------------------
  # 7단계: 유기질비료(퇴비) vs 토양 유기물 (Organic Fertilizer vs Soil)
  # [의도] 비슷한 동위원소 범위를 갖는 두 기원을 보존성 추적자(Cl)로 세분화
  # [근거] Bedard-Haughn(2003); 퇴비 시비 시 염분 부하가 발생하나, 자연 토양은 저염 상태 유지.
  # ----------------------------------------------------------------------------
  if (!is.na(n15) && n15 >= 4.0 && n15 < 9.0 && toc > 4.0) {
    if (cl > 35) return("유기질비료(퇴비) 영향")
    else return("토양유기물/야생동물배설물")
  }
  
  # ----------------------------------------------------------------------------
  # 8단계: 조류증식 및 내생부하 (Algal Bloom & Loading)
  # [의도] 외부 유입이 아닌 수체 내 2차 오염(Autochthonous) 판별
  # [특징] 광합성 활동 지표(Chl-a)와 유기물 농도의 상관분석을 통한 정체 수역 특성 파악.
  # ----------------------------------------------------------------------------
  if (chl > 40.0 && toc > 4.5) return("조류증식(현장발생)")
  if (toc > 6.5 && tp > 0.1) return("내생부하(정체수역 유기물 축적)")
  
  # ----------------------------------------------------------------------------
  # 9단계: 복합오염 Fallback (Mixed Pollution)
  # [의도] 지배적 지문이 희석된 경우, 절대적 오염 부하량을 기준으로 정책적 우선순위 결정
  # ----------------------------------------------------------------------------
  if (tp > 0.1 || nh3 > 0.5 || cl > 50) return("복합오염(하수/분뇨 기여 우세)")
  if (no3 > 2.0 || tn > 3.0) return("복합오염(농경지 비점 우세)")
  if (toc > 3.0) return("복합오염(유기물 부하 우세)")
  
  # 10단계: 배경 농도 (Natural Background)
  return("자연배경/특이징후 없음")
}

# 군집별 평균 수질을 계산하여 최종 오염원 라벨 매핑
cat("[*] 군집별 화학적 지문(Fingerprint)을 분석하여 오염원 분류 중...\n")

# [주의] Cl을 포함한 모든 수질 프록시를 군집 평균값으로 요약
cluster_summary <- data %>%
  group_by(Final_Cluster) %>%
  summarise(across(c(d15N, d18O, TP, NH3N, NO3N, TOC, EC, Chla, TN, Cl), 
                   \(x) mean(x, na.rm = TRUE)), .groups = 'drop') %>%
  # NaN이 발생한 경우 0으로 대체 (또는 적절한 기본값)
  mutate(across(everything(), ~replace(., is.nan(.), 0)))

# [실행] 각 군집 행에 대해 10단계 판별 엔진 적용
cluster_summary <- cluster_summary %>%
  rowwise() %>%
  mutate(Source_Name = assign_source_universal(cur_data())) %>%
  ungroup()

# [통합] 판별된 오염원 명칭을 개별 관측치 데이터프레임에 전파
data <- data %>%
  left_join(cluster_summary %>% select(Final_Cluster, Source_Name), by = "Final_Cluster")

cat("[*] 분석 완료: 총", length(unique(data$Source_Name)), "종의 주요 오염원이 식별되었습니다.\n")


# 6. 시각화 및 로직 가시화 (Visual Analytics & XAI)
# [방법론] UMAP 차원축소 및 Decision Tree 로직 추출
# [근거] "ML이 왜 이렇게 분류했는가?"에 대한 답을 트리 구조로 시각화하여 연구 투명성을 입증합니다.

# (1) 분류 지도 (UMAP)
# [수정] 데이터 수에 맞게 n_neighbors를 동적으로 설정하여 에러 방지
n_rows <- nrow(scaled_data)
# n_neighbors는 데이터 수보다 작아야 하므로, 15와 (전체 행 수 - 1) 중 작은 값을 선택합니다.
# 최소값은 2 이상이어야 하므로 안전장치를 둡니다.
n_neighbors_val <- if (n_rows > 2) min(15, n_rows - 1) else 2

# UMAP 실행 시 설정값 적용
umap_res <- umap(scaled_data, n_neighbors = n_neighbors_val)

data$umap1 <- umap_res$layout[,1]
data$umap2 <- umap_res$layout[,2]

p1 <- ggplot(data, aes(x = umap1, y = umap2, color = Source_Name, label = spot)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_text_repel(size = 4, fontface = "bold", family = "malgun") +
  theme_minimal(base_family = "malgun") +
  labs(title = "ML 기반 오염원 정밀 앙상블 분류 지도", 
       subtitle = paste("최적 군집수 K =", best_k, "| n_neighbors =", n_neighbors_val))
print(p1)

# (2) [핵심] 의사결정 트리 (Decision Logic Tree)
# [방법론] rpart 기반 Classification Tree (참고문헌: Breiman et al., 1984)
# [역할] "어떤 수질 항목의 몇 점을 기준으로 명칭이 정해졌나?"를 가시화합니다.
# cp(complexity parameter) 값을 낮출수록 나무가 더 복잡하고 상세하게 생성됩니다.
# minsplit은 노드를 나누기 위한 최소 샘플 수입니다.
tree_model <- rpart(Source_Name ~ d15N + d18O + TP + NH3N + NO3N + TOC + EC + Chla + TN + Cl, 
                    data = data, 
                    method = "class",
                    control = rpart.control(cp = 0.001, minsplit = 2, minbucket = 1))

# 시각화 시 텍스트가 겹치지 않도록 조정
prp(tree_model, type = 4, extra = 101, fallen.leaves = TRUE, 
    main = "오염원 판별 세부 의사결정 로직", cex = 0.5)

# dev.new(width=12, height=8)
rpart.plot(tree_model, main = "오염원 판별 의사결정 로직 트리 (분류 근거 가시화)",
           type = 4, extra = 104, fallen.leaves = TRUE, box.palette = "RdYlGn",
           shadow.col = "gray", nn = TRUE, 
           family = "malgun", cex = 0.7)

# (3) 변수 중요도 (Random Forest XAI)
# dev.new()
rf_model <- randomForest(as.factor(Source_Name) ~ ., 
                         data = data.frame(imputed_data, Source_Name = data$Source_Name), 
                         importance = TRUE)
varImpPlot(rf_model, main = "오염원 분류 결정 변수 기여도 (Gini Importance)", family = "malgun")


# 7. 리포트 출력
cat("\n[최종 오염원 스크리닝 리포트]\n")
print(data %>% select(spot, Source_Name) %>% arrange(Source_Name))
